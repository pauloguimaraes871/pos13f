# Pipeline definition for pos13f ----
#
# Medallion layers (docs/IMPLEMENTATION_PLAN.md, Section 1.0), each deployed by one
# function that builds, runs its data-quality suite and pins the result on the
# versioned board in data/prod/:
#   bronze     deploy_bronze(), deploy_bronze_openfigi(), deploy_bronze_tiingo():
#              SEC 13F archives, FIGI replies and daily prices, as delivered
#   silver     deploy_silver(): filings, holdings state, identity, units, prices
#   gold       deploy_gold(): activity proxy, universe, cohort, F1 signal
#   backtests  deploy_backtests(): quintile ladders in factoRverse, spreads, evaluation
#   reports    build_report_tables(), report/report.qmd (HTML) and memo/memo.qmd (PDF)
#
# Unit tests run first, on every run, and every deploy depends on them: a failing
# unit test stops the pipeline before anything is downloaded or pinned.
#
# Sample window: only archives whose filing window lies between window_start and
# window_end are ingested. window_end is set here by the user, never inferred from the
# archives on SEC: moving it to a new batch (SEC posts three-month filing windows, Dec to
# Feb, Mar to May, Jun to Aug, Sep to Nov) requires redownloading every Tiingo price,
# because adjusted prices are restated: the pipeline stops until the user deletes the
# cached price files (data-raw/cache/tiingo/t_*.parquet). The manifest
# (data/manifest/window_end.txt) records the window once the bronze layer concludes.
# Demo profile: POS13F_WINDOW_START=2024-01-01 in .Renviron runs a short sample.

# Load package code ----
## The package is developed in place, so the pipeline runs on the working tree
## rather than on an installed copy that could be stale
devtools::load_all(quiet = TRUE)

## imports = "pos13f" makes targets track the package's functions, so editing a deploy
## function invalidates the targets that use it
targets::tar_option_set(packages = character(0), imports = "pos13f")

ingest_from <- as.Date(Sys.getenv("POS13F_WINDOW_START", "2013-01-01"))
## Last day of the latest SEC batch in the sample (archive 01jun2026-31aug2026)
window_end <- as.Date("2026-08-31")
## First report quarter usable as q-1: all its filings, which start the day after the
## quarter ends, must lie inside the window. The XML data sets start in May 2013, so 2013Q1
## is partial and 2013Q2 is the floor. A later window start moves it to the first quarter
## ending on or after the day before the window opens (the demo profile, opening
## 2024-01-01, starts with Q4 2023 as q-1 at end-May 2024)
first_quarter <- max(
  as.Date("2013-06-30"),
  lubridate::ceiling_date(ingest_from, "quarter", change_on_boundary = FALSE) - 1
)

# Targets ----
list(
  ## Unit tests: always rerun; downstream targets depend on their result
  targets::tar_target(
    unit_tests,
    run_unit_tests(),
    cue = targets::tar_cue(mode = "always")
  ),

  ## Board location; every deploy opens the board from this path
  targets::tar_target(board_path, file.path("data", "prod")),

  ## The archive list is fetched on every run so new SEC postings are picked up
  targets::tar_target(
    sec_archives,
    list_sec_archives(verbose = FALSE),
    cue = targets::tar_cue(mode = "always")
  ),
  targets::tar_target(
    sec_archives_window,
    ## !! rather than .env$: targets does not see globals behind .env$, so a new window
    ## would not rerun this filter (Codex review). tar_target() injects !! values into
    ## the command, and a changed command reruns the target
    dplyr::filter(
      sec_archives, .data$window_start >= !!ingest_from,
      .data$window_end <= !!window_end
    )
  ),

  ## Bronze: archives already pinned from the same source file are skipped inside
  targets::tar_target(
    bronze_sec,
    {
      unit_tests
      deploy_bronze(init_pipeline_board(board_path), sec_archives_window, verbose = FALSE)
    }
  ),

  ## Bronze OpenFIGI: FIGI replies for every held share CUSIP (cached per batch)
  targets::tar_target(
    bronze_openfigi,
    {
      bronze_sec
      deploy_bronze_openfigi(init_pipeline_board(board_path), verbose = FALSE)
    }
  ),

  ## Bronze Tiingo: daily prices of US common stocks plus SPY (cached per ticker)
  targets::tar_target(
    bronze_tiingo,
    {
      bronze_openfigi
      deploy_bronze_tiingo(init_pipeline_board(board_path),
        window_end = window_end, verbose = FALSE
      )
    }
  ),

  ## Manifest: the window of the concluded bronze layer, written only after every bronze
  ## deploy has passed its suite
  targets::tar_target(
    bronze_manifest,
    {
      bronze_tiingo
      write_window_end(window_end, file.path("data", "manifest", "window_end.txt"))
    }
  ),

  ## Silver targets name every bronze target they read. A bronze deploy returns its pin
  ## versions, so a re-pinned bronze table reruns silver; the manifest alone returns a
  ## constant path and would let silver keep stale data (Codex review, PR #4)

  ## Silver filings: holdings filings with usable_date, relations and trading calendar
  targets::tar_target(
    silver_filings,
    {
      bronze_manifest
      bronze_sec
      bronze_tiingo
      deploy_silver_filings(init_pipeline_board(board_path), sec_archives_window,
        verbose = FALSE
      )
    }
  ),

  ## Silver holdings: amendment state machine over every share position, point in time
  targets::tar_target(
    silver_holdings,
    {
      silver_filings
      bronze_sec
      deploy_silver_holdings(init_pipeline_board(board_path), sec_archives_window,
        verbose = FALSE
      )
    }
  ),

  ## Silver prices: security master, prices, returns, illiquid flags, terminal events
  ## and the implied-price identity check
  targets::tar_target(
    silver_prices,
    {
      silver_holdings
      bronze_openfigi
      bronze_tiingo
      deploy_silver_prices(init_pipeline_board(board_path), verbose = FALSE)
    }
  ),

  ## Gold: cohort, universe, F1 and the panels run_port_backtest() consumes
  targets::tar_target(
    gold,
    {
      silver_filings
      silver_holdings
      silver_prices
      deploy_gold(init_pipeline_board(board_path), first_quarter = first_quarter, verbose = FALSE)
    }
  ),

  ## Filing-date audit: a random sample of filings checked against EDGAR acceptance times;
  ## stops the pipeline if any filing could be used on or before the day it became public
  targets::tar_target(
    filing_date_audit,
    {
      silver_filings
      deploy_filing_date_audit(init_pipeline_board(board_path), verbose = FALSE)
    }
  ),

  ## Bronze Fama-French factors: monthly FF5 and momentum for the alpha regressions
  targets::tar_target(
    bronze_ff_factors,
    deploy_bronze_ff_factors(init_pipeline_board(board_path), verbose = FALSE)
  ),

  ## Backtests: quintile ladders under cs, ew and rp, the momentum double sort, spreads
  ## under cost and borrow scenarios, and their evaluation
  targets::tar_target(
    backtests,
    {
      gold
      bronze_ff_factors
      deploy_backtests(init_pipeline_board(board_path), verbose = FALSE)
    }
  ),

  ## Report inputs: every table and figure of the report and memo, from the pins
  targets::tar_target(
    report_tables,
    {
      backtests
      filing_date_audit
      build_report_tables(init_pipeline_board(board_path))
    }
  ),

  ## Technical report (self-contained HTML) and memo (PDF); both only render
  ## report_tables, read with targets::tar_read()
  tarchetypes::tar_quarto(report, path = "report/report.qmd", quiet = TRUE),
  tarchetypes::tar_quarto(memo, path = "memo/memo.qmd", quiet = TRUE)
)

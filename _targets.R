# Pipeline definition for pos13f ----
#
# Medallion layers (docs/IMPLEMENTATION_PLAN.md, Section 1.0), each deployed by one
# function that builds, runs its data-quality suite and pins the result on the
# versioned board in data/prod/:
#   bronze     deploy_bronze(), deploy_bronze_openfigi(), deploy_bronze_tiingo():
#              SEC 13F archives, FIGI replies and daily prices, as delivered
#   silver     deploy_silver(): filings, holdings state, identity, units, prices
#   gold       deploy_gold(): activity proxy, universe, cohort, F1 signal
#   backtests  deploy_backtests(): weights, returns, costs, evaluation
#   reports    deploy_reports(): analysis tables, HTML report, memo inputs
#
# Unit tests run first, on every run, and every deploy depends on them: a failing
# unit test stops the pipeline before anything is downloaded or pinned.
#
# Sample window: only archives whose filing window lies between window_start and
# window_end are ingested. window_end is set here by the user, never inferred from the
# archives on SEC: moving it to a new batch (SEC posts three-month filing windows, Dec to
# Feb, Mar to May, Jun to Aug, Sep to Nov) requires redownloading every Tiingo price,
# because adjusted prices are restated (deploy_bronze_tiingo(force = TRUE)).
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
## TRUE only when window_end moves to a new batch: redownloads all prices (about 2 h).
## Set it back to FALSE after that run, or every later Tiingo rebuild redownloads again
tiingo_force <- FALSE

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
    dplyr::filter(
      sec_archives, .data$window_start >= .env$ingest_from,
      .data$window_end <= .env$window_end
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
        window_end = window_end, force = tiingo_force, verbose = FALSE
      )
    }
  )
)

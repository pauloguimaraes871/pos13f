#' Deploy the gold layer: cohort, universe, F1 signal and factoRverse-ready panels
#'
#' Builds, for every formation (last trading day of Feb, May, Aug, Nov), the filer
#' cohort, the liquid universe and the F1 breadth-change signal with its controls, then
#' the monthly panels `run_port_backtest()` consumes. Runs [run_test_gold()] and pins
#' only if every check passes.
#'
#' @param board A `pins_board` from [init_pipeline_board()].
#' @param log_path Character scalar, folder for the data-quality log.
#' @param memory_limit Character scalar, duckdb memory limit.
#' @param temp_dir Character scalar, folder where duckdb spills and writes work files.
#' @param first_quarter Date scalar, earliest report quarter that may serve as q-1. A
#'   formation whose q-1 reports were filed before the ingested window would compare a
#'   full quarter with a handful of late filers, so formations start at the first q-1
#'   quarter whose whole filing window was ingested (see `build_formation_dates()`).
#' @param verbose Logical; print progress and pins messages.
#'
#' @return A tibble with one row per pinned table: `table` and `version`.
#'
#' @details
#' Pins, each a plain data frame (the backtest layer wraps them with
#' `create_meta_dataframe()` and `create_meta_xts()`): `gold_signals_m_df`,
#' `gold_fwd_1m_returns_m_df`, `gold_liquidity_m_df`, `gold_volatility_m_df` (shared
#' `id`, `tickers`, `dates`), `gold_universe_diagnostics`, `gold_cohort`,
#' `gold_daily_returns_d` (daily total returns of every instrument that entered a
#' universe, percentage points), `gold_benchmark_d` and `gold_benchmark_m` (SPY).
#' Backtest settings implied: `rebalancing_months = c(3, 6, 9, 12)`, and daily returns
#' cut at the formation date before any covariance estimate.
#' @export
deploy_gold <- function(board, log_path = file.path("logs", "tests"), memory_limit = "3GB",
                        temp_dir = file.path("data-raw", "cache", "duckdb_tmp"),
                        first_quarter = as.Date("2013-06-30"), verbose = TRUE) {
  # Validate inputs
  if (!inherits(board, "pins_board")) stop("board must be a pins board (init_pipeline_board()).")
  if (!inherits(first_quarter, "Date") || length(first_quarter) != 1 || is.na(first_quarter)) {
    stop("first_quarter must be a single non-missing Date (a report quarter end).")
  }

  # Small silver tables, in R
  read <- function(name) pins::pin_read(board, name)
  calendar <- read("silver_trading_calendar")$date
  formations <- build_formation_dates(calendar, first_quarter = first_quarter)
  master <- read("silver_security_master")
  terminal <- read("silver_terminal_events")
  filings <- read("silver_filings")

  # Large tables, inside duckdb
  con <- open_duckdb_work(memory_limit, temp_dir)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rel <- function(name) {
    paste0("read_parquet('", normalizePath(pins::pin_download(board, name), winslash = "/"), "')")
  }
  get <- function(sql) tibble::as_tibble(DBI::dbGetQuery(con, sql))
  duckdb::duckdb_register(con, "formations", formations)
  duckdb::duckdb_register(con, "master", master)
  activity <- get(sql_gold_filer_activity(rel("silver_holdings_state"), "formations"))
  versions <- read("silver_holdings_versions")
  relations <- read("silver_filing_relations")
  cohort <- select_gold_cohort(formations, versions, activity, relations)
  if (verbose) message("Gold: cohort selected")
  duckdb::duckdb_register(con, "cohort", cohort)
  breadth <- get(sql_gold_breadth(
    rel("silver_holdings_state"), "formations", "cohort", "master", rel("silver_prices_d")
  ))
  features <- get(sql_gold_monthly_features(rel("silver_prices_d"), rel("silver_returns_d")))
  monthly_returns <- get(sql_gold_monthly_returns(rel("silver_returns_d")))
  if (verbose) message("Gold: cohort, breadth and features computed")

  # Universe and panels, in R
  universe <- build_gold_universe(formations, breadth, features, terminal, master)
  panels <- build_gold_panels(formations, universe, cohort, features, monthly_returns,
    terminal,
    last_calendar_day = max(calendar)
  )
  benchmark_d <- get(sql_gold_benchmark_d(rel("bronze_tiingo_prices")))
  ## Monthly benchmark dated like the panels: the first day of the month it covers
  benchmark_m <- benchmark_d |>
    dplyr::mutate(dates = lubridate::floor_date(.data$date, "month")) |>
    dplyr::summarise(
      bench_return_1m = 100 * (prod(1 + .data$bench_return / 100) - 1),
      .by = "dates"
    )

  # Recompute a random sample of panel rows in dplyr, then test and pin
  reference <- build_gold_reference(con, sample_gold_check_rows(panels, terminal), formations,
    cohort, master, terminal, rel,
    last_month = lubridate::floor_date(max(calendar), "month")
  )
  ok <- run_logged_test(
    function() {
      run_test_gold(
        panels, cohort, benchmark_d, benchmark_m, formations, filings, versions, relations,
        reference
      )
    },
    log_file = "test_gold", log_path = log_path
  )
  check_test_success(ok)
  write_gold_pins(board, con, panels, cohort, benchmark_d, benchmark_m, rel, temp_dir, verbose)
}

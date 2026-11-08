#' Pin the gold tables with their lineage
#'
#' Called by `deploy_gold()` after the suite passed. The daily returns of every
#' instrument that entered a universe are written by duckdb (millions of rows) and
#' uploaded as a file; the other tables are pinned as data frames.
#'
#' @param board A `pins_board`.
#' @param con A duckdb connection holding the `formations` relation.
#' @param panels Named list from `build_gold_panels()`.
#' @param cohort Tibble from `select_gold_cohort()`.
#' @param benchmark_d,benchmark_m Tibbles of daily and monthly SPY returns.
#' @param rel Function returning the SQL relation of a pin name.
#' @param temp_dir Character scalar, folder for the work file.
#' @param verbose Logical; print pins messages.
#'
#' @return A tibble with one row per pinned table: `table` and `version`.
#' @noRd
write_gold_pins <- function(board, con, panels, cohort, benchmark_d, benchmark_m, rel,
                            temp_dir, verbose) {
  # Daily returns of universe instruments, in percentage points
  members <- tibble::tibble(instrument_id = unique(panels$signals_m_df$tickers))
  duckdb::duckdb_register(con, "members", members)
  work_dir <- tempfile("gold_", tmpdir = temp_dir)
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  daily_path <- normalizePath(file.path(work_dir, "DAILY_RETURNS.parquet"),
    winslash = "/", mustWork = FALSE
  )
  DBI::dbExecute(con, paste0(
    "COPY (SELECT r.instrument_id || '-' || strftime(r.date, '%Y-%m-%d') AS id, ",
    "r.instrument_id AS tickers, r.date AS dates, 100 * r.ret AS ret, r.gap_days ",
    "FROM ", rel("silver_returns_d"), " r JOIN members m ON r.instrument_id = m.instrument_id ",
    "ORDER BY id) TO '", daily_path, "' (FORMAT PARQUET)"
  ))

  # Pin everything with the silver versions it was built from
  sources <- c(
    "silver_trading_calendar", "silver_filings", "silver_filing_relations",
    "silver_holdings_state", "silver_holdings_versions", "silver_security_master",
    "silver_prices_d", "silver_returns_d", "silver_terminal_events", "bronze_tiingo_prices"
  )
  version_of <- function(p) pins::pin_meta(board, p)$local$version
  inputs <- purrr::map(stats::setNames(sources, sources), version_of)
  pin <- function(data, table) {
    save_local_pin(board, data, "gold", table, inputs = inputs, verbose = verbose)
  }
  tables <- list(
    signals_m_df = panels$signals_m_df,
    fwd_1m_returns_m_df = panels$fwd_1m_returns_m_df,
    liquidity_m_df = panels$liquidity_m_df,
    volatility_m_df = panels$volatility_m_df,
    universe_diagnostics = panels$universe_diagnostics,
    cohort = cohort,
    daily_returns_d = daily_path,
    benchmark_d = benchmark_d,
    benchmark_m = benchmark_m
  )
  tibble::tibble(
    table = names(tables),
    version = purrr::imap_chr(tables, \(data, table) pin(data, table))
  )
}

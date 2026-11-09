#' Pin a factoRverse backtest cohort as an S4 object
#'
#' The other backtest pins are tidy tables; this one keeps a `port_backtest_cohort` whole
#' (weights, transactions log, costs, metrics, workflow), so a reader can call
#' factoRverse methods on it, for example `plot(cohort, plot_id = "Time-Series of Raw
#' Returns")`.
#'
#' @param board A `pins_board`.
#' @param cohort A `port_backtest_cohort` from `run_quantile_ladder()`.
#' @param name Character scalar, `<signal>_<method>_<design>`; the pin is
#'   `backtests_cohort_<name>`.
#' @param verbose Logical; print pins messages.
#' @param keep_versions Numeric scalar, versions kept per cohort pin (a cohort is tens of
#'   MB, and every backtest run writes a new version).
#'
#' @return Character scalar, the version written.
#' @noRd
write_backtest_cohort <- function(board, cohort, name, verbose, keep_versions = 2) {
  # Validate inputs
  if (!methods::is(cohort, "port_backtest_cohort")) stop("cohort must be a port_backtest_cohort.")
  if (!grepl("^[a-z0-9_]+$", name)) {
    stop("name must be snake_case (lower-case letters, digits, underscores).")
  }

  pin_name <- paste0("backtests_cohort_", name)
  ## Every pin build_backtest_inputs() reads, including the covariance inputs of rp
  sources <- c(
    "gold_signals_m_df", "gold_fwd_1m_returns_m_df", "gold_liquidity_m_df",
    "gold_volatility_m_df", "gold_daily_returns_d"
  )
  inputs <- purrr::map(
    stats::setNames(sources, sources), \(p) pins::pin_meta(board, p)$local$version
  )
  write <- function() {
    pins::pin_write(board, cohort,
      name = pin_name, type = "rds", versioned = TRUE,
      ## Without this, pins skips a write whose object is unchanged and drops new lineage
      force_identical_write = TRUE,
      metadata = list(
        stage = "backtests", table = paste0("cohort_", name), inputs = inputs,
        factoRverse_version = as.character(utils::packageVersion("factoRverse"))
      )
    )
    pins::pin_versions_prune(board, pin_name, n = keep_versions)
  }
  if (verbose) write() else suppressMessages(write())
  pins::pin_meta(board, pin_name)$local$version
}

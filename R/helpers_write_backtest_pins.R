#' Pin the backtest tables with their lineage
#'
#' @param board A `pins_board`.
#' @param tables Named list of tibbles: `ladder_returns` and the elements of
#'   `evaluate_backtests()`.
#' @param verbose Logical; print pins messages.
#'
#' @return A tibble with one row per pinned table: `table` and `version`.
#' @noRd
write_backtest_pins <- function(board, tables, verbose) {
  sources <- c(
    "gold_signals_m_df", "gold_fwd_1m_returns_m_df", "gold_liquidity_m_df",
    "gold_volatility_m_df", "gold_daily_returns_d", "gold_benchmark_m", "bronze_ff_factors_m"
  )
  version_of <- function(p) pins::pin_meta(board, p)$local$version
  inputs <- purrr::map(stats::setNames(sources, sources), version_of)
  extra <- list(factoRverse_version = as.character(utils::packageVersion("factoRverse")))
  tibble::tibble(
    table = names(tables),
    version = purrr::imap_chr(tables, function(data, table) {
      save_local_pin(board, data, "backtests", table,
        inputs = inputs, extra = extra, verbose = verbose
      )
    })
  )
}

#' Average return, signal level, size tilt and holdings of every ladder portfolio
#'
#' @param ladder Tibble pinned as `backtests_ladder_returns`.
#'
#' @return A tibble with one row per `signal`, `method`, `design` and `quantile`:
#'   `ann_return` and `ann_vol` (percentage points per year, raw returns), `signal_level`
#'   and `liquidity_level` (average weighted signal and liquidity percentile) and
#'   `n_holdings` (average number of stocks).
#' @noRd
report_ladder_table <- function(ladder) {
  ladder |>
    dplyr::filter(!is.na(.data$raw_return)) |>
    dplyr::summarise(
      ann_return = 12 * mean(.data$raw_return),
      ann_vol = sqrt(12) * stats::sd(.data$raw_return),
      signal_level = mean(.data$signal_level), liquidity_level = mean(.data$liquidity_level),
      n_holdings = mean(.data$n_holdings),
      .by = c("signal", "method", "design", "quantile")
    ) |>
    dplyr::arrange(.data$signal, .data$method, .data$design, .data$quantile)
}

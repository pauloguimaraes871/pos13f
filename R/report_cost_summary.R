#' Turnover and trading-cost decomposition of the spreads
#'
#' @param ladder Tibble pinned as `backtests_ladder_returns` (`direct_cost` at the run
#'   fee).
#' @param spreads Tibble pinned as `backtests_spreads`.
#' @param run_cost_bps Numeric scalar, the fee of the factoRverse runs in bps.
#'
#' @return A list: `turnover` (tibble with `signal`, `method`, `design`, `quantile` and
#'   `traded_per_rebalance`, the average sum of absolute weight changes at a rebalance
#'   after the first), `decomposition` (tibble with `signal`, `method`, `design`,
#'   `cost_bps`, `borrow_bps` and the annual `gross`, `trading`, `borrow` and `net`, in
#'   percentage points) and `by_year` (tibble with `signal`, `method`, `design`, `year`
#'   and `trading_25bps`, the spread trading cost at 25 bps in percentage points).
#' @noRd
report_cost_summary <- function(ladder, spreads, run_cost_bps = 5) {
  keys <- c("signal", "method", "design")
  first_month <- min(ladder$dates)
  turnover <- ladder |>
    dplyr::filter(.data$direct_cost > 1e-10, .data$dates > first_month) |>
    dplyr::summarise(
      traded_per_rebalance = mean(.data$direct_cost / (run_cost_bps / 100)),
      .by = c(keys, "quantile")
    )
  decomposition <- spreads |>
    dplyr::summarise(
      gross = 12 * mean(.data$gross_return), trading = 12 * mean(.data$trading_cost),
      borrow = 12 * mean(.data$borrow_cost), net = 12 * mean(.data$net_return),
      .by = c(keys, "cost_bps", "borrow_bps")
    )
  by_year <- spreads |>
    dplyr::filter(.data$cost_bps == 25, .data$borrow_bps == 0) |>
    dplyr::mutate(year = lubridate::year(.data$dates)) |>
    dplyr::summarise(trading_25bps = sum(.data$trading_cost), .by = c(keys, "year"))
  list(turnover = turnover, decomposition = decomposition, by_year = by_year)
}

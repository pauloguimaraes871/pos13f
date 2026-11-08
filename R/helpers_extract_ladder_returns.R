#' Tidy monthly returns, costs and signal level of every portfolio in a ladder
#'
#' Turns a factoRverse `port_backtest_cohort` from `run_quantile_ladder()` into one row
#' per portfolio and finished holding month (factoRverse reports no return for the
#' unfinished last month), with every series dated by the first day of its holding month
#' (the gold panel convention).
#'
#' @param cohort A `port_backtest_cohort` from `run_quantile_ladder()`.
#'
#' @return A tibble ordered by `quantile` and `dates`: `quantile` (integer, 1 = lowest
#'   signal), `dates`, `raw_return`, `net_return`, `direct_cost`, `market_impact_cost`
#'   (percentage points of the portfolio, at the fee of the run), `signal_level` (the
#'   portfolio's weighted average signal) and `n_holdings` (stocks with positive weight).
#'
#' @details
#' factoRverse stamps each series differently: returns on the first day of the month
#' after the holding month, costs on the day after the rebalancing date, portfolio
#' metrics on the holding month itself. All three are mapped to the holding month here,
#' and the mapping is checked by the backtest suite (net return equals raw return minus
#' costs in every month).
#' @noRd
extract_ladder_returns <- function(cohort) {
  long <- function(m_xts, value, to_holding_month) {
    x <- m_xts@data
    tibble::as_tibble(zoo::coredata(x), .name_repair = "minimal") |>
      ## xts::.index() is always seconds since 1970; zoo::index() returns seconds or days
      ## depending on whether xts is loaded
      dplyr::mutate(stamp = as.Date(as.POSIXct(xts::.index(x), tz = "UTC"))) |>
      tidyr::pivot_longer(-"stamp", names_to = "config", values_to = value) |>
      dplyr::mutate(
        dates = to_holding_month(.data$stamp),
        quantile = as.integer(stringr::str_match(.data$config, "_q([0-9]+)_s__")[, 2])
      ) |>
      dplyr::select("quantile", "dates", dplyr::all_of(value))
  }
  month_before <- function(d) add_months(lubridate::floor_date(d, "month"), -1)
  same_month <- function(d) lubridate::floor_date(d, "month")
  returns <- cohort@port_returns_m_xts_list
  costs <- cohort@port_costs_m_xts_list
  key <- dplyr::join_by("quantile", "dates")
  long(returns$raw_returns_m_xts, "raw_return", month_before) |>
    dplyr::left_join(long(returns$net_returns_m_xts, "net_return", month_before),
      by = key, relationship = "one-to-one"
    ) |>
    dplyr::left_join(long(costs$direct_cost_m_xts, "direct_cost", same_month),
      by = key, relationship = "one-to-one"
    ) |>
    dplyr::left_join(long(costs$market_impact_cost_m_xts, "market_impact_cost", same_month),
      by = key, relationship = "one-to-one"
    ) |>
    dplyr::left_join(long(cohort@port_metrics_m_xts_list[[1]], "signal_level", same_month),
      by = key, relationship = "one-to-one"
    ) |>
    dplyr::left_join(count_ladder_holdings(cohort), by = key, relationship = "one-to-one") |>
    dplyr::mutate(
      dplyr::across(c("direct_cost", "market_impact_cost"), \(x) dplyr::coalesce(x, 0))
    ) |>
    dplyr::arrange(.data$quantile, .data$dates)
}

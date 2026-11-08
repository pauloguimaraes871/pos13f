#' Forward-return columns of the gold panel, with the terminal policy
#'
#' @param rows Tibble of panel rows (`tickers`, `dates` = first day of the holding
#'   month, and the key columns).
#' @param key Character vector, the key columns.
#' @param monthly_returns Tibble from `sql_gold_monthly_returns()`.
#' @param terminal Tibble pinned as `silver_terminal_events`.
#' @param last_month Date scalar, first day of the calendar's last (unfinished) month.
#' @param terminal_stress Numeric scalar, extra return on the last price day in the
#'   stress column (-0.3).
#'
#' @return A tibble with the key columns, `fwd_return_1m` and `fwd_return_1m_stress`
#'   (percentage points).
#'
#' @details
#' Baseline: a month after the instrument's last price earns 0 (position closed at the
#' last price); a month without any price earns 0 too (no observed move). Stress: the
#' month holding the last price of a stock that stopped trading is compounded with
#' `terminal_stress`, multiplicatively. The unfinished last month is `NA`.
#' @noRd
gold_return_columns <- function(rows, key, monthly_returns, terminal, last_month,
                                terminal_stress) {
  rows |>
    dplyr::select(dplyr::all_of(key)) |>
    dplyr::left_join(
      dplyr::select(monthly_returns, tickers = "instrument_id", dates = "month", "ret_m"),
      by = dplyr::join_by("tickers", "dates"), relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      dplyr::select(terminal, tickers = "instrument_id", "last_date", "is_live"),
      by = dplyr::join_by("tickers"), relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      ended_before = !.data$is_live & .data$last_date < .data$dates,
      ends_now = !.data$is_live & lubridate::floor_date(.data$last_date, "month") == .data$dates,
      base = dplyr::if_else(.data$ended_before, 0, dplyr::coalesce(.data$ret_m, 0)),
      stress = dplyr::if_else(
        .data$ends_now, (1 + .data$base) * (1 + terminal_stress) - 1, .data$base
      ),
      unfinished = .data$dates >= last_month,
      fwd_return_1m = dplyr::if_else(.data$unfinished, NA_real_, 100 * .data$base),
      fwd_return_1m_stress = dplyr::if_else(.data$unfinished, NA_real_, 100 * .data$stress)
    ) |>
    dplyr::select(dplyr::all_of(key), "fwd_return_1m", "fwd_return_1m_stress")
}

#' Forward monthly returns recomputed from daily returns, as a reference
#'
#' An independent restatement of the forward-return columns of the gold panel: the
#' holding month's return is the product of the instrument's daily total returns dated
#' in that month, with the terminal policy of plan 1.6. The panel instead goes through
#' `sql_gold_monthly_returns()` and `gold_return_columns()`.
#'
#' @param rows Tibble with `tickers` (instrument id) and `dates` (Date, first day of the
#'   holding month).
#' @param returns_d Tibble of `silver_returns_d` rows (`instrument_id`, `date`, `ret`, a
#'   fraction).
#' @param terminal Tibble pinned as `silver_terminal_events` (`instrument_id`,
#'   `last_date`, `is_live`).
#' @param last_month Date scalar, first day of the calendar's unfinished last month.
#' @param terminal_stress Numeric scalar, extra return on the last price day in the
#'   stress column (plan 1.6: -0.3).
#'
#' @return `rows` (distinct `tickers`, `dates`) with `fwd_return_1m` and
#'   `fwd_return_1m_stress`, in percentage points.
#'
#' @details
#' A month without daily returns earns 0, which covers both a month with no trade and
#' every month after the last price (the position was closed at the last price). The
#' stress return compounds `terminal_stress` into the month of the last price of an
#' instrument that stopped trading. The unfinished month is `NA`.
#' @noRd
reference_gold_fwd_returns <- function(rows, returns_d, terminal, last_month,
                                       terminal_stress = -0.3) {
  targets <- rows |>
    dplyr::distinct(.data$tickers, .data$dates) |>
    dplyr::mutate(month_end = lubridate::ceiling_date(.data$dates, "month") - 1)
  compounded <- targets |>
    dplyr::inner_join(dplyr::select(returns_d, tickers = "instrument_id", "date", "ret"),
      by = dplyr::join_by("tickers", between(y$date, x$dates, x$month_end)),
      relationship = "one-to-many"
    ) |>
    dplyr::summarise(growth = prod(1 + .data$ret, na.rm = TRUE), .by = c("tickers", "dates"))

  targets |>
    dplyr::left_join(compounded,
      by = dplyr::join_by("tickers", "dates"), relationship = "one-to-one"
    ) |>
    dplyr::left_join(dplyr::select(terminal, tickers = "instrument_id", "last_date", "is_live"),
      by = dplyr::join_by("tickers"), relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      growth = dplyr::coalesce(.data$growth, 1),
      ends_here = !.data$is_live & .data$last_date >= .data$dates &
        .data$last_date <= .data$month_end,
      stressed = dplyr::if_else(
        .data$ends_here, .data$growth * (1 + terminal_stress), .data$growth
      ),
      fwd_return_1m = dplyr::if_else(.data$dates >= last_month, NA_real_, 100 * (.data$growth - 1)),
      fwd_return_1m_stress = dplyr::if_else(
        .data$dates >= last_month, NA_real_, 100 * (.data$stressed - 1)
      )
    ) |>
    dplyr::select("tickers", "dates", "fwd_return_1m", "fwd_return_1m_stress")
}

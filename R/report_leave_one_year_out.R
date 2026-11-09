#' Leave-one-year-out stability of the long-short spreads
#'
#' A premium that rests on one or two years is fragile. For each spread and each
#' calendar year this reports the spread's return in that year and its annualised mean
#' over all the other months, so a reader sees how far any single year moves the
#' estimate (plan Section 4: leave-one-period-out stability).
#'
#' @param spreads Tibble pinned as `backtests_spreads`: `signal`, `method`, `design`,
#'   `cost_bps`, `borrow_bps`, `dates` (Date, first day of the holding month) and
#'   `gross_return` (percentage points per month).
#' @param cost_bps,borrow_bps Numeric scalars, the cost scenario to use (gross by
#'   default).
#'
#' @return A tibble with one row per spread (`signal`, `method`, `design`) and year:
#'   `n_months`, `year_mean` (annualised mean of the year's months, percent),
#'   `mean_without` (annualised mean of every other month, percent) and `mean_all`.
#'
#' @details
#' Means are arithmetic and annualised by 12, as in the summary table, so `mean_all` is
#' the summary's `ann_return`. The first and last years are partial (March 2014 and
#' October 2026 are the end months); `n_months` shows how many months each year has.
#' @noRd
report_leave_one_year_out <- function(spreads, cost_bps = 0, borrow_bps = 0) {
  # Validate inputs
  required_cols <- c(
    "signal", "method", "design", "cost_bps", "borrow_bps", "dates", "gross_return"
  )
  missing_cols <- setdiff(required_cols, names(spreads))
  if (length(missing_cols) > 0) {
    stop("spreads is missing columns: ", paste(missing_cols, collapse = ", "), ".")
  }
  keys <- c("signal", "method", "design")
  scenario <- dplyr::filter(
    spreads, .data$cost_bps == .env$cost_bps, .data$borrow_bps == .env$borrow_bps,
    !is.na(.data$gross_return)
  )
  if (nrow(scenario) == 0) stop("spreads has no rows for cost_bps ", cost_bps, ".")

  # Totals per spread, then each year against the rest
  totals <- dplyr::summarise(scenario,
    total = sum(.data$gross_return), n_total = dplyr::n(), .by = dplyr::all_of(keys)
  )
  scenario |>
    dplyr::mutate(year = lubridate::year(.data$dates)) |>
    dplyr::summarise(
      sum_year = sum(.data$gross_return), n_months = dplyr::n(),
      .by = dplyr::all_of(c(keys, "year"))
    ) |>
    dplyr::inner_join(totals, by = dplyr::join_by(!!!keys), relationship = "many-to-one") |>
    dplyr::mutate(
      year_mean = 12 * .data$sum_year / .data$n_months,
      ## The mean without a year uses every remaining month, not the other years' means
      mean_without = 12 * (.data$total - .data$sum_year) / (.data$n_total - .data$n_months),
      mean_all = 12 * .data$total / .data$n_total
    ) |>
    dplyr::select(
      dplyr::all_of(keys), "year", "n_months", "year_mean", "mean_without", "mean_all"
    ) |>
    dplyr::arrange(.data$signal, .data$method, .data$design, .data$year)
}

#' Cumulative share of original 13F reports usable by days after quarter end
#'
#' Supports the month-end formation (memo Section 7): reports bunch just before the
#' 45-day deadline.
#'
#' @param filings Tibble pinned as `silver_filings`.
#' @param days Numeric vector, days after quarter end to evaluate.
#'
#' @return A tibble with `days` and `share_usable` (share of original 13F-HR reports for
#'   quarters from 2014 on whose `usable_date` is at most that many days after quarter
#'   end).
#' @noRd
report_filing_delay <- function(filings, days = 0:120) {
  delay <- filings |>
    dplyr::filter(
      .data$submission_type == "13F-HR", !.data$is_amendment,
      .data$period_of_report >= as.Date("2014-01-01")
    ) |>
    dplyr::transmute(delay = as.numeric(.data$usable_date - .data$period_of_report))
  tibble::tibble(days = days, share_usable = purrr::map_dbl(days, \(d) mean(delay$delay <= d)))
}

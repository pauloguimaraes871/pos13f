#' Filings, filers and amendments per report quarter
#'
#' @param filings Tibble pinned as `silver_filings` (`cik`, `period_of_report`,
#'   `submission_type`, `is_amendment`, `usable_date`).
#' @param first_quarter,last_quarter Date scalars, report quarters to keep.
#'
#' @return A tibble with one row per report quarter: `period_of_report`, `n_filings`
#'   (original 13F-HR reports), `n_filers` (distinct filing CIKs), `n_amendments` and
#'   `share_by_day_45` (share of original reports usable within 45 days of quarter end).
#' @noRd
report_filing_coverage <- function(filings, first_quarter = as.Date("2013-06-30"),
                                   last_quarter = as.Date("2026-06-30")) {
  filings |>
    dplyr::filter(
      .data$period_of_report >= first_quarter, .data$period_of_report <= last_quarter,
      .data$submission_type == "13F-HR"
    ) |>
    dplyr::mutate(delay = as.numeric(.data$usable_date - .data$period_of_report)) |>
    dplyr::summarise(
      n_filings = sum(!.data$is_amendment),
      n_filers = dplyr::n_distinct(.data$cik),
      n_amendments = sum(.data$is_amendment),
      share_by_day_45 = mean(.data$delay[!.data$is_amendment] <= 45),
      .by = "period_of_report"
    ) |>
    dplyr::arrange(.data$period_of_report)
}

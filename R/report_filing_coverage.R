#' Filings, filers, amendments and confidential omissions per report quarter
#'
#' @param filings Tibble pinned as `silver_filings` (`cik`, `period_of_report`,
#'   `submission_type`, `is_amendment`, `usable_date`, `is_confidential_omitted`).
#' @param first_quarter,last_quarter Date scalars, report quarters to keep.
#'
#' @return A tibble with one row per report quarter: `period_of_report`, `n_filings`
#'   (original 13F-HR reports), `n_filers` (distinct CIKs filing an original report),
#'   `n_amendments` (13F-HR/A filings), `n_confidential` (original reports declaring that
#'   positions were omitted under a confidential treatment request; a blank flag counts
#'   as not declared) and `share_by_day_45` (share of original reports usable within 45
#'   days of quarter end).
#' @noRd
report_filing_coverage <- function(filings, first_quarter = as.Date("2013-06-30"),
                                   last_quarter = as.Date("2026-06-30")) {
  filings |>
    dplyr::filter(
      .data$period_of_report >= first_quarter, .data$period_of_report <= last_quarter,
      .data$submission_type %in% c("13F-HR", "13F-HR/A")
    ) |>
    dplyr::mutate(delay = as.numeric(.data$usable_date - .data$period_of_report)) |>
    dplyr::summarise(
      n_filings = sum(!.data$is_amendment),
      n_filers = dplyr::n_distinct(.data$cik[!.data$is_amendment]),
      n_amendments = sum(.data$is_amendment),
      n_confidential = sum(.data$is_confidential_omitted & !.data$is_amendment, na.rm = TRUE),
      share_by_day_45 = mean(.data$delay[!.data$is_amendment] <= 45),
      .by = "period_of_report"
    ) |>
    dplyr::arrange(.data$period_of_report)
}

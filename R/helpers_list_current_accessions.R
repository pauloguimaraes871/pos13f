#' Filings that make up each filer's report state at each formation date
#'
#' The report a filer has for quarter p at formation t is the holdings version current
#' at t (`valid_from <= t < valid_to`). That version is built from its base filing (the
#' latest original or restatement) plus the NEW HOLDINGS amendments after it
#' (`build_holdings_sources()`). Anything read from filings at t, such as the managers
#' a report includes, must come from these accessions only: a superseded filing is no
#' longer part of the report.
#'
#' @param versions Tibble pinned as `silver_holdings_versions` (`cik`,
#'   `period_of_report`, `accession_number`, `kind`, `event_no`, `base_no`,
#'   `valid_from`, `valid_to`, `is_visible`).
#' @param formations Tibble from `build_formation_dates()`.
#'
#' @return A tibble with one row per formation, filer, compared quarter and contributing
#'   filing: `formation_date`, `cik`, `period_of_report`, `quarter` (`"curr"` for q,
#'   `"prev"` for q-1) and `accession_number`.
#' @noRd
list_current_accessions <- function(versions, formations) {
  quarters <- dplyr::bind_rows(
    dplyr::transmute(formations, .data$formation_date,
      period_of_report = .data$report_quarter, quarter = "curr"
    ),
    dplyr::transmute(formations, .data$formation_date,
      period_of_report = .data$prev_quarter, quarter = "prev"
    )
  )
  build_holdings_sources(versions) |>
    dplyr::inner_join(quarters,
      by = dplyr::join_by("period_of_report"), relationship = "many-to-many"
    ) |>
    ## Point in time: the version current at the formation date
    dplyr::filter(
      .data$valid_from <= .data$formation_date,
      is.na(.data$valid_to) | .data$valid_to > .data$formation_date
    ) |>
    dplyr::select("formation_date", "cik", "period_of_report", "quarter", "accession_number")
}

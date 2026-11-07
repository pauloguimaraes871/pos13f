#' Build the silver filings table: one row per 13F holdings filing, dated point in time
#'
#' Keeps the holdings filings (13F-HR and its amendments 13F-HR/A), joins the cover and
#' summary pages, and attaches the two dates every later layer relies on: `usable_date`,
#' the first trading day after the filing date (nothing in a filing is known before it),
#' and `value_regime`, the unit of VALUE fixed by the filing date.
#'
#' @param submission Tibble, bronze SUBMISSION of all archives (`read_bronze_sec_table()`):
#'   `ACCESSION_NUMBER`, `FILING_DATE` (Date), `SUBMISSIONTYPE`, `CIK`, `PERIODOFREPORT`
#'   (Date, quarter end) and `source_archive`.
#' @param coverpage Tibble, bronze COVERPAGE: `ACCESSION_NUMBER`, `REPORTTYPE`,
#'   `AMENDMENTTYPE`, `AMENDMENTNO`, `FILINGMANAGER_NAME`, `FORM13FFILENUMBER`.
#' @param summarypage Tibble, bronze SUMMARYPAGE: `ACCESSION_NUMBER`,
#'   `OTHERINCLUDEDMANAGERSCOUNT`, `TABLEENTRYTOTAL`, `TABLEVALUETOTAL`,
#'   `ISCONFIDENTIALOMITTED`.
#' @param calendar Date vector, trading days (`read_trading_calendar()`).
#'
#' @return A tibble, one row per holdings filing, ordered by `cik`, `period_of_report`,
#'   `filing_date` and `accession_number`, with: `accession_number`, `cik`,
#'   `period_of_report` (Date, quarter end the holdings refer to), `filing_date`,
#'   `usable_date` (Date), `submission_type`, `is_amendment` (lgl), `amendment_type`
#'   (`"RESTATEMENT"`, `"NEW HOLDINGS"`, or `NA`), `amendment_no` (dbl), `report_type`
#'   (`"holdings"` or `"combination"`), `value_regime` (`"thousands"` or `"dollars"`),
#'   `manager_name`, `form13f_file_number`, `other_included_managers_count`,
#'   `table_entry_total`, `table_value_total` (in `value_regime` units),
#'   `is_confidential_omitted` (lgl) and `source_archive`.
#'
#' @details
#' Point in time: each accession enters from its own `usable_date`. Amendments are new
#' accessions with their own filing date, so they never reach back into an earlier
#' formation; the amendment state machine (silver holdings) applies them in
#' `usable_date` order.
#'
#' Amendment status comes from the submission type: the cover-page flag `ISAMENDMENT`
#' disagrees with it on 48 filings and is missing on most filings before 2018. The
#' period comes from SUBMISSION, which is a quarter end on every filing (COVERPAGE
#' differs on 5). 18 holdings filings have no summary page; their summary fields are
#' `NA`.
#'
#' Units: VALUE is in thousands of dollars for filings made before 2023-01-03 and in
#' dollars from that date on (plan Section 1.5, checked in the G1 spike), whatever the
#' report quarter, so an old-quarter amendment filed in 2023 is in dollars.
#' @noRd
build_silver_filings <- function(submission, coverpage, summarypage, calendar) {
  # Validate inputs
  need <- list(
    submission = c(
      "ACCESSION_NUMBER", "FILING_DATE", "SUBMISSIONTYPE", "CIK", "PERIODOFREPORT",
      "source_archive"
    ),
    coverpage = c(
      "ACCESSION_NUMBER", "REPORTTYPE", "AMENDMENTTYPE", "AMENDMENTNO",
      "FILINGMANAGER_NAME", "FORM13FFILENUMBER"
    ),
    summarypage = c(
      "ACCESSION_NUMBER", "OTHERINCLUDEDMANAGERSCOUNT", "TABLEENTRYTOTAL",
      "TABLEVALUETOTAL", "ISCONFIDENTIALOMITTED"
    )
  )
  tables <- list(submission = submission, coverpage = coverpage, summarypage = summarypage)
  for (nm in names(need)) {
    missing_cols <- setdiff(need[[nm]], names(tables[[nm]]))
    if (length(missing_cols) > 0) {
      stop(nm, " is missing columns: ", paste(missing_cols, collapse = ", "), ".")
    }
  }

  # Holdings filings with their cover and summary pages
  ## Notice filings (13F-NT, 13F-NT/A) report no holdings: another manager reports them
  holdings <- dplyr::filter(submission, .data$SUBMISSIONTYPE %in% c("13F-HR", "13F-HR/A"))
  ## Every filing has a cover page (an unmatched one stops); cover pages of notices drop
  filings <- holdings |>
    dplyr::inner_join(
      dplyr::select(coverpage, dplyr::all_of(need$coverpage)),
      by = dplyr::join_by("ACCESSION_NUMBER"), relationship = "one-to-one",
      unmatched = c("error", "drop")
    ) |>
    dplyr::left_join(
      dplyr::select(summarypage, dplyr::all_of(need$summarypage)),
      by = dplyr::join_by("ACCESSION_NUMBER"), relationship = "one-to-one"
    )

  # Point-in-time dates and units
  filings |>
    dplyr::transmute(
      accession_number = .data$ACCESSION_NUMBER,
      cik = trimws(.data$CIK),
      period_of_report = .data$PERIODOFREPORT,
      filing_date = .data$FILING_DATE,
      usable_date = compute_usable_date(.data$FILING_DATE, calendar),
      submission_type = .data$SUBMISSIONTYPE,
      is_amendment = .data$SUBMISSIONTYPE == "13F-HR/A",
      ## The cover page also types some originals as amendments; only /A filings are
      amendment_type = dplyr::if_else(.data$is_amendment, .data$AMENDMENTTYPE, NA_character_),
      amendment_no = dplyr::if_else(.data$is_amendment, .data$AMENDMENTNO, NA_real_),
      report_type = dplyr::case_when(
        .data$REPORTTYPE == "13F HOLDINGS REPORT" ~ "holdings",
        .data$REPORTTYPE == "13F COMBINATION REPORT" ~ "combination"
      ),
      value_regime = dplyr::if_else(
        .data$FILING_DATE >= as.Date("2023-01-03"), "dollars", "thousands"
      ),
      manager_name = .data$FILINGMANAGER_NAME,
      form13f_file_number = .data$FORM13FFILENUMBER,
      other_included_managers_count = .data$OTHERINCLUDEDMANAGERSCOUNT,
      table_entry_total = .data$TABLEENTRYTOTAL,
      table_value_total = .data$TABLEVALUETOTAL,
      is_confidential_omitted = .data$ISCONFIDENTIALOMITTED == "Y",
      source_archive = .data$source_archive
    ) |>
    dplyr::arrange(.data$cik, .data$period_of_report, .data$filing_date, .data$accession_number)
}

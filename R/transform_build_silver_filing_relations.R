#' Build the silver filing relations: which other managers each holdings filing names
#'
#' Two relations, kept apart because they mean opposite things (plan Section 1.4):
#' `reports_for_me` (OTHERMANAGER: managers that report holdings on behalf of the filer)
#' and `included_in_report` (OTHERMANAGER2: managers whose holdings this report
#' includes). The second one detects overlapping reports in silver holdings.
#'
#' @param othermanager Tibble, bronze OTHERMANAGER: `ACCESSION_NUMBER`, `CIK`,
#'   `FORM13FFILENUMBER`, `NAME`.
#' @param othermanager2 Tibble, bronze OTHERMANAGER2: `ACCESSION_NUMBER`,
#'   `SEQUENCENUMBER`, `CIK`, `FORM13FFILENUMBER`, `NAME`.
#' @param filings Tibble from `build_silver_filings()` (`accession_number`).
#'
#' @return A tibble of distinct rows, ordered by `accession_number`, `relation` and
#'   `sequence_number`, with `accession_number`, `relation`, `sequence_number` (dbl,
#'   position in OTHERMANAGER2; `NA` for `reports_for_me`), `related_cik`,
#'   `related_file_number` (13F file number, `028-xxxxx`) and `related_name`. Only
#'   relations of silver filings are kept.
#'
#' @details
#' About 40% of related managers have no CIK in the data (the form asks for the 13F
#' file number). The file number and name are kept so that silver holdings can resolve
#' them through the filers' own `form13f_file_number`. Exact duplicate rows (the same
#' manager listed twice at the same position) are dropped.
#' @noRd
build_silver_filing_relations <- function(othermanager, othermanager2, filings) {
  # Validate inputs
  base_cols <- c("ACCESSION_NUMBER", "CIK", "FORM13FFILENUMBER", "NAME")
  if (!all(base_cols %in% names(othermanager))) {
    stop("othermanager must have columns: ", paste(base_cols, collapse = ", "), ".")
  }
  if (!all(c(base_cols, "SEQUENCENUMBER") %in% names(othermanager2))) {
    stop(
      "othermanager2 must have columns: ", paste(base_cols, collapse = ", "),
      ", SEQUENCENUMBER."
    )
  }
  if (!"accession_number" %in% names(filings)) stop("filings must have accession_number.")

  # Stack both relations in one shape
  relate <- function(df, relation, sequence_number) {
    dplyr::transmute(df,
      accession_number = .data$ACCESSION_NUMBER,
      relation = relation,
      sequence_number = {{ sequence_number }},
      related_cik = trimws(.data$CIK),
      related_file_number = trimws(.data$FORM13FFILENUMBER),
      related_name = trimws(.data$NAME)
    )
  }
  dplyr::bind_rows(
    relate(othermanager, "reports_for_me", NA_real_),
    relate(othermanager2, "included_in_report", .data$SEQUENCENUMBER)
  ) |>
    ## Relations of notice filings are dropped with the notices themselves
    dplyr::semi_join(filings, by = dplyr::join_by("accession_number")) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$accession_number, .data$relation, .data$sequence_number)
}

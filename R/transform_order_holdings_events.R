#' Order the filings of each filer and quarter into holdings-state events
#'
#' The amendment state machine of plan Section 1.3, at filing level. Per `cik` and
#' `period_of_report`, filings are applied in order of `usable_date` (then
#' `accession_number`): an original 13F-HR or a RESTATEMENT replaces the whole state; a
#' NEW HOLDINGS amendment adds to it. Each event opens a version of the state, valid from
#' its `usable_date` until the next event's.
#'
#' @param filings Tibble from `build_silver_filings()`: `accession_number`, `cik`,
#'   `period_of_report`, `usable_date`, `is_amendment`, `amendment_type`.
#'
#' @return A tibble with one row per applied filing, ordered by `cik`,
#'   `period_of_report` and `event_no`: `cik`, `period_of_report`, `accession_number`,
#'   `usable_date`, `kind` (`"replace"` or `"append"`), `event_no` (order within the
#'   quarter), `base_no` (event number of the latest replacing filing at or before this
#'   one; 0 when only appends precede), `valid_from`, `valid_to` (Date, exclusive; `NA`
#'   for the current version) and `is_visible` (`FALSE` when a later filing of the
#'   same quarter becomes usable on the same day, so this version is never observable).
#'   Attribute `n_untyped_ignored`: amendments without a type, left out.
#'
#' @details
#' Point in time: a version starts at the `usable_date` of the filing that creates it,
#' never earlier, so an amendment cannot reach back into a formation before it was
#' public. The quarter comes from `period_of_report` only: a late report that SEC placed
#' in a later archive still updates its own quarter, from its own `usable_date`.
#'
#' Amendments without an amendment type (53 of 15,727) are ignored, the conservative
#' choice of the owner (2026-11-07): read as a restatement, a partial list would erase
#' the original; read as new holdings, a full list would double it. The original
#' stands.
#'
#' Two filings of one quarter usable on the same day (1,941 cases) are ordered by
#' accession number; only the last state of that day is visible.
#' @noRd
order_holdings_events <- function(filings) {
  # Validate inputs
  need <- c(
    "accession_number", "cik", "period_of_report", "usable_date", "is_amendment",
    "amendment_type"
  )
  missing_cols <- setdiff(need, names(filings))
  if (length(missing_cols) > 0) {
    stop("filings is missing columns: ", paste(missing_cols, collapse = ", "), ".")
  }
  if (anyNA(filings$usable_date)) stop("filings$usable_date must have no NA (silver suite).")

  # Drop untyped amendments, then order and chain the events of each quarter
  untyped <- filings$is_amendment & is.na(filings$amendment_type)
  events <- filings |>
    dplyr::filter(!(.data$is_amendment & is.na(.data$amendment_type))) |>
    dplyr::mutate(
      kind = dplyr::if_else(.data$amendment_type %in% "NEW HOLDINGS", "append", "replace")
    ) |>
    dplyr::arrange(.data$cik, .data$period_of_report, .data$usable_date, .data$accession_number) |>
    dplyr::mutate(
      event_no = dplyr::row_number(),
      ## The latest replacing event so far; appends before any original start from empty
      base_no = cummax(dplyr::if_else(.data$kind == "replace", .data$event_no, 0L)),
      valid_from = .data$usable_date,
      valid_to = dplyr::lead(.data$usable_date),
      is_visible = is.na(.data$valid_to) | .data$valid_to > .data$valid_from,
      .by = c("cik", "period_of_report")
    ) |>
    dplyr::select(
      "cik", "period_of_report", "accession_number", "usable_date", "kind", "event_no",
      "base_no", "valid_from", "valid_to", "is_visible"
    )
  structure(events, n_untyped_ignored = sum(untyped))
}

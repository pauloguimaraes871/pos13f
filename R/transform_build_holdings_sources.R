#' List the filings that make up each visible version of a holdings state
#'
#' A version created by event k of a quarter holds the positions of its base filing
#' (the latest original or restatement at or before k) plus every NEW HOLDINGS
#' amendment between that base and k.
#'
#' @param events Tibble from `order_holdings_events()`.
#'
#' @return A tibble with one row per visible version and contributing filing:
#'   `cik`, `period_of_report`, `valid_from`, `valid_to`, `accession_number`, and
#'   `is_base` (lgl, `TRUE` for the replacing filing the version starts from).
#' @noRd
build_holdings_sources <- function(events) {
  # Validate inputs
  need <- c(
    "cik", "period_of_report", "accession_number", "kind", "event_no", "base_no",
    "valid_from", "valid_to", "is_visible"
  )
  missing_cols <- setdiff(need, names(events))
  if (length(missing_cols) > 0) {
    stop("events is missing columns: ", paste(missing_cols, collapse = ", "), ".")
  }

  # Pair each visible version with the events from its base up to itself
  versions <- events |>
    dplyr::filter(.data$is_visible) |>
    dplyr::select("cik", "period_of_report", "valid_from", "valid_to",
      version_no = "event_no", "base_no"
    )
  candidates <- dplyr::select(
    events, "cik", "period_of_report", "event_no", "kind",
    "accession_number"
  )
  ## Within a quarter (at most 13 events), so the many-to-many join stays small
  versions |>
    dplyr::inner_join(candidates,
      by = dplyr::join_by("cik", "period_of_report"), relationship = "many-to-many"
    ) |>
    dplyr::filter(.data$event_no >= .data$base_no, .data$event_no <= .data$version_no) |>
    ## Between base and version only appends add; an earlier replace is the base itself
    dplyr::filter(.data$event_no == .data$base_no | .data$kind == "append") |>
    dplyr::mutate(is_base = .data$event_no == .data$base_no) |>
    dplyr::select(
      "cik", "period_of_report", "valid_from", "valid_to", "accession_number", "is_base"
    ) |>
    dplyr::arrange(.data$cik, .data$period_of_report, .data$valid_from, .data$accession_number)
}

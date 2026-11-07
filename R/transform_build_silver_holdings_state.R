#' Build the holdings state: each filer's positions per quarter, valid point in time
#'
#' Combines the positions of the filings behind each visible version (an original or
#' restatement plus the NEW HOLDINGS amendments after it) into one row per filer,
#' report quarter, CUSIP and version. A row says: from `valid_from` until `valid_to`,
#' the market knew that filer `cik` held `shares` of `cusip` at the end of
#' `period_of_report`. Works on local tibbles or lazy duckdb tables of one connection.
#'
#' @param positions Table from `summarise_holdings_positions()`: `accession_number`,
#'   `cusip`, `shares`, `value_usd`.
#' @param sources Table from `build_holdings_sources()`: `cik`, `period_of_report`,
#'   `valid_from`, `valid_to`, `accession_number`.
#'
#' @return A table with one row per `cik`, `period_of_report`, `cusip` and
#'   `valid_from`: `valid_to` (exclusive, `NA` while current), `shares` (dbl),
#'   `value_usd` (dbl) and `n_sources` (filings of the version reporting this CUSIP; 2
#'   or more when a NEW HOLDINGS amendment adds to a position already reported, which
#'   is summed).
#'
#' @details
#' A version without positions (an empty restatement) produces no rows: the filer then
#' reported no share position for that quarter from `valid_from` on.
#' @noRd
build_silver_holdings_state <- function(positions, sources) {
  positions |>
    dplyr::inner_join(sources,
      by = dplyr::join_by("accession_number"), relationship = "many-to-many"
    ) |>
    dplyr::summarise(
      shares = sum(.data$shares, na.rm = FALSE),
      value_usd = sum(.data$value_usd, na.rm = FALSE),
      n_sources = dplyr::n(),
      .by = c("cik", "period_of_report", "cusip", "valid_from", "valid_to")
    )
}

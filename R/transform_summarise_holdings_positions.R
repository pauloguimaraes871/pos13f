#' Sum the share positions of each filing by CUSIP, in US dollars
#'
#' A filing can report the same CUSIP on several rows (one per investment discretion or
#' other-manager split). Ownership counts need one position per filing and security,
#' so rows are summed. Works on a local tibble or on a lazy duckdb table (the full
#' INFOTABLE has 124 million rows and never enters R).
#'
#' @param infotable Tibble or lazy table of bronze INFOTABLE rows: `ACCESSION_NUMBER`,
#'   `CUSIP`, `VALUE` (dbl, in the filing's `value_regime` unit), `SSHPRNAMT` (dbl),
#'   `SSHPRNAMTTYPE`, `PUTCALL`.
#' @param valid_cusips Table of the same source (local or the same duckdb connection)
#'   with one column `cusip`: the identifiers to keep (`list_valid_position_cusips()`).
#' @param regimes Table of the same source with `accession_number` and `value_regime`
#'   (`"thousands"` or `"dollars"`), from silver filings.
#'
#' @return A table of the same kind with one row per `accession_number` and `cusip`:
#'   `shares` (dbl), `value_usd` (dbl, US dollars) and `n_rows` (source rows summed).
#'
#' @details
#' Only share positions count (`SSHPRNAMTTYPE == "SH"`, no `PUTCALL`): an option or a
#' principal amount (bond) is not a holding of the stock (owner decision 2026-11-07).
#' VALUE is converted per filing before any amendment is combined with its original,
#' because an old-quarter amendment filed after 2023-01-03 is in dollars while its
#' original is in thousands.
#' @noRd
summarise_holdings_positions <- function(infotable, valid_cusips, regimes) {
  infotable |>
    dplyr::filter(.data$SSHPRNAMTTYPE == "SH", is.na(.data$PUTCALL)) |>
    ## Renamed before deriving: duckdb names are case-insensitive, so a new `cusip`
    ## would collide with `CUSIP`
    dplyr::select(
      accession_number = "ACCESSION_NUMBER", cusip_raw = "CUSIP", value_raw = "VALUE",
      share_amount = "SSHPRNAMT"
    ) |>
    dplyr::mutate(cusip = toupper(stringr::str_trim(.data$cusip_raw))) |>
    dplyr::semi_join(valid_cusips, by = dplyr::join_by("cusip")) |>
    dplyr::summarise(
      shares = sum(.data$share_amount, na.rm = FALSE),
      value_raw = sum(.data$value_raw, na.rm = FALSE),
      n_rows = dplyr::n(),
      .by = c("accession_number", "cusip")
    ) |>
    ## Every position's filing is a silver filing; the unit comes from its filing date
    dplyr::inner_join(regimes, by = dplyr::join_by("accession_number")) |>
    dplyr::mutate(
      value_usd = .data$value_raw * dplyr::if_else(.data$value_regime == "thousands", 1000, 1)
    ) |>
    dplyr::select("accession_number", "cusip", "shares", "value_usd", "n_rows")
}

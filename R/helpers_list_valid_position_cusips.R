#' List the CUSIPs of share positions that are well formed
#'
#' The check digit can only be verified in R (`is_valid_cusip()`), so the distinct
#' identifiers (about 1.5 million) are collected, checked, and returned for a semi join.
#' Same rule as `list_bronze_cusips()`, so silver holdings and OpenFIGI cover the same
#' identifiers.
#'
#' @param infotable Tibble or lazy table of bronze INFOTABLE rows (`CUSIP`,
#'   `SSHPRNAMTTYPE`, `PUTCALL`).
#'
#' @return A local tibble with one column `cusip` (9 upper-case characters, valid check
#'   digit), sorted. Attribute `n_dropped`: distinct share-position identifiers left out.
#' @noRd
list_valid_position_cusips <- function(infotable) {
  cusips <- infotable |>
    dplyr::filter(.data$SSHPRNAMTTYPE == "SH", is.na(.data$PUTCALL)) |>
    dplyr::transmute(cusip = toupper(stringr::str_trim(.data$CUSIP))) |>
    dplyr::distinct() |>
    dplyr::collect()
  valid <- cusips |>
    dplyr::filter(
      stringr::str_detect(.data$cusip, "^[0-9A-Z]{9}$"),
      is_valid_cusip(.data$cusip)
    ) |>
    dplyr::arrange(.data$cusip)
  structure(valid, n_dropped = nrow(cusips) - nrow(valid))
}

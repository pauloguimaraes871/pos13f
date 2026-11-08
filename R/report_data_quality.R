#' Data-quality counts recorded in the pin metadata
#'
#' Each deploy records what its suite counted (rows dropped, suspects, malformed
#' identifiers) in the pin metadata; this collects them for the report.
#'
#' @param meta Function taking a pin name and returning its user metadata (a list).
#'
#' @return A tibble with `pin`, `field` and `value` (numeric), one row per recorded
#'   count.
#' @noRd
report_data_quality <- function(meta) {
  fields <- list(
    bronze_tiingo_prices = c("n_rows", "n_bad_adj_rows"),
    bronze_openfigi = c("n_rows", "n_malformed_cusips"),
    silver_filings = "n_rows",
    silver_holdings_state = c(
      "n_state_rows", "n_untyped_amendments_ignored", "n_malformed_cusips",
      "n_filings_with_rows_not_in_silver"
    ),
    silver_security_master = c("n_rows", "n_ambiguous_cusips"),
    silver_prices_d = "n_rows",
    silver_identity_checks = c(
      "n_rows", "n_identity_suspects", "share_rows_thousand", "share_rows_thousandth"
    )
  )
  purrr::imap(fields, function(keep, pin) {
    m <- meta(pin)
    value <- purrr::map_dbl(keep, \(f) if (is.null(m[[f]])) NA_real_ else as.numeric(m[[f]]))
    tibble::tibble(pin = pin, field = keep, value = value)
  }) |>
    dplyr::bind_rows()
}

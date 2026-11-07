#' Tickers to request from Tiingo, from the bronze OpenFIGI replies
#'
#' The price request set: US common stocks only (owner decision 2026-11-07, matching the
#' CRSP share codes 10 and 11 behind the Fama-French factors; REITs, ADRs and ETFs are
#' out of scope), plus SPY for the trading calendar and the market benchmark.
#'
#' @param replies Tibble pinned as `bronze_openfigi` (columns `status`, `ticker`,
#'   `exch_code`, `security_type`).
#'
#' @return A tibble with one column `ticker`, distinct and sorted, in Tiingo's format.
#'
#' @details
#' OpenFIGI writes share classes as `BRK/B`; Tiingo uses `BRK-B`. Tickers of the form
#' digits followed by `D` (for example `1284849D`, Allergan) are Bloomberg codes for
#' dead securities; Tiingo answered 404 for every one in the G1 spike, so they are not
#' requested. Which ticker belongs to which instrument at which date is decided in
#' silver.
#' @noRd
list_tiingo_tickers <- function(replies) {
  replies |>
    dplyr::filter(
      .data$status == "found", .data$exch_code == "US",
      .data$security_type == "Common Stock", !is.na(.data$ticker),
      !stringr::str_detect(.data$ticker, "^[0-9]+D$")
    ) |>
    dplyr::transmute(ticker = stringr::str_replace_all(.data$ticker, "/", "-")) |>
    dplyr::bind_rows(tibble::tibble(ticker = "SPY")) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$ticker)
}

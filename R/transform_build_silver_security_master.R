#' Build the security master: one permanent instrument per US common-stock CUSIP
#'
#' Maps each held CUSIP to its OpenFIGI composite FIGI, the permanent `instrument_id`
#' used in every later table (plan Section 1.1). Two CUSIPs are the same instrument
#' only when OpenFIGI gives them the same composite FIGI; no other chaining.
#'
#' @param replies Tibble pinned as `bronze_openfigi`: `cusip`, `status`,
#'   `composite_figi`, `share_class_figi`, `ticker`, `name`, `exch_code`,
#'   `security_type`.
#'
#' @return A tibble with one row per CUSIP, ordered by `cusip`: `cusip`,
#'   `instrument_id` (composite FIGI), `share_class_figi`, `ticker` (Tiingo format, `/`
#'   written `-`), `name`, `ticker_shared` (lgl, the ticker belongs to more than one
#'   instrument) and `has_price_source` (lgl, the ticker was requested from Tiingo and
#'   identifies one instrument only). Attribute `n_ambiguous_cusips`: CUSIPs left out
#'   because OpenFIGI returned two composite FIGIs.
#'
#' @details
#' Ticker reuse: 110 tickers name two or more instruments in the OpenFIGI replies (for
#' example ABAT, a dead battery maker and a live one). Tiingo serves one history per
#' ticker, the current owner's, so those prices cannot be attributed and
#' `has_price_source` is `FALSE`. A ticker reused by a company outside the 13F universe
#' is not visible here; the implied-price check of the prices layer covers it.
#' @noRd
build_silver_security_master <- function(replies) {
  # Validate inputs
  need <- c(
    "cusip", "status", "composite_figi", "share_class_figi", "ticker", "name",
    "exch_code", "security_type"
  )
  missing_cols <- setdiff(need, names(replies))
  if (length(missing_cols) > 0) {
    stop("replies is missing columns: ", paste(missing_cols, collapse = ", "), ".")
  }

  # US common stocks with one composite FIGI per CUSIP
  common <- replies |>
    dplyr::filter(
      .data$status == "found", .data$exch_code == "US",
      .data$security_type == "Common Stock", !is.na(.data$composite_figi)
    ) |>
    dplyr::distinct(
      .data$cusip, .data$composite_figi, .data$share_class_figi, .data$ticker, .data$name
    )
  ambiguous <- common |>
    dplyr::summarise(n = dplyr::n_distinct(.data$composite_figi), .by = "cusip") |>
    dplyr::filter(.data$n > 1)
  master <- common |>
    dplyr::anti_join(ambiguous, by = dplyr::join_by("cusip")) |>
    dplyr::transmute(
      cusip = .data$cusip,
      instrument_id = .data$composite_figi,
      share_class_figi = .data$share_class_figi,
      ticker = stringr::str_replace_all(.data$ticker, "/", "-"),
      name = .data$name
    ) |>
    ## A CUSIP can still appear twice with the same FIGI (two share-class records)
    dplyr::distinct(.data$cusip, .keep_all = TRUE)

  # Tickers that identify one instrument, and were requested from Tiingo
  requested <- list_tiingo_tickers(replies)$ticker
  master |>
    dplyr::mutate(
      ticker_shared = dplyr::n_distinct(.data$instrument_id) > 1,
      .by = "ticker"
    ) |>
    dplyr::mutate(
      has_price_source = !.data$ticker_shared & .data$ticker %in% requested
    ) |>
    dplyr::arrange(.data$cusip) |>
    structure(n_ambiguous_cusips = nrow(ambiguous))
}

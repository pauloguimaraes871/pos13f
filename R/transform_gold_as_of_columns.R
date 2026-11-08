#' Liquidity or volatility columns of the gold panel, as of the day before the row
#'
#' A row dated the first day of month M takes the features of the instrument's last
#' trading day in M-1 or, when it did not trade in M-1, of the latest earlier month
#' (rolling join). Features already end the day before their own date
#' (`sql_gold_monthly_features()`), so nothing from month M enters.
#'
#' @param rows Tibble of panel rows (`tickers`, `info_month`, and the key columns).
#' @param key Character vector, the key columns.
#' @param features Tibble from `sql_gold_monthly_features()`.
#' @param cols Character vector, the feature columns to keep.
#'
#' @return A tibble with the key columns then `cols`.
#'
#' @details
#' `daily_vol` falls back to `vol_3m` when fewer than two returns fall in the previous
#' 21 trading days, because factoRverse needs a value for every row.
#' @noRd
gold_as_of_columns <- function(rows, key, features, cols) {
  joined <- rows |>
    dplyr::select(dplyr::all_of(key), "info_month") |>
    dplyr::left_join(
      dplyr::rename(features, tickers = "instrument_id", feature_month = "month"),
      by = dplyr::join_by("tickers", closest(info_month >= feature_month)),
      relationship = "many-to-one"
    )
  if ("daily_vol" %in% cols) {
    joined <- dplyr::mutate(joined, daily_vol = dplyr::coalesce(.data$daily_vol, .data$vol_3m))
  }
  dplyr::select(joined, dplyr::all_of(c(key, cols)))
}

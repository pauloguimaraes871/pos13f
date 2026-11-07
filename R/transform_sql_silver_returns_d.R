#' SQL for the silver daily returns of each instrument
#'
#' Total return from the adjusted close, between consecutive available prices of the
#' same instrument.
#'
#' @param prices_d Character scalar, SQL relation of silver daily prices
#'   (`sql_silver_prices_d()`).
#'
#' @return Character scalar, a SELECT statement with one row per `instrument_id` and
#'   `date` after its first price: `id`, `instrument_id`, `date`, `ticker`, `ret`
#'   (simple total return, a fraction) and `gap_days` (trading days since the previous
#'   price; 1 for consecutive days).
#'
#' @details
#' A return over a gap (`gap_days > 1`) spans several trading days; it is kept and
#' flagged, never spread over the missing days, so later layers decide how to treat it
#' (no balanced panel is fabricated).
#' @noRd
sql_silver_returns_d <- function(prices_d) {
  window <- "OVER (PARTITION BY instrument_id ORDER BY date)"
  paste0(
    "SELECT id, instrument_id, date, ticker, ret, gap_days FROM (",
    "SELECT id, instrument_id, date, ticker, ",
    "adj_close / lag(adj_close) ", window, " - 1 AS ret, ",
    "day_index - lag(day_index) ", window, " AS gap_days FROM ", prices_d,
    ") WHERE ret IS NOT NULL"
  )
}

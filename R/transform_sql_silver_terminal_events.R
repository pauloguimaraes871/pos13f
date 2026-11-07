#' SQL for the terminal events: first and last price of each instrument
#'
#' Tiingo gives no delisting return, so the backtest needs the last price date of every
#' instrument to apply the terminal policy (plan 1.6: close at the last price in the
#' baseline, -30% in the stress case).
#'
#' @param prices_d Character scalar, SQL relation of silver daily prices.
#' @param last_calendar_day Date scalar, last day of the trading calendar.
#'
#' @return Character scalar, a SELECT statement with one row per `instrument_id`:
#'   `ticker`, `first_date`, `last_date`, `n_days` (prices), `max_gap_days` (longest
#'   stretch of trading days between two prices) and `is_live` (lgl, priced on the
#'   last calendar day, so no terminal event yet).
#'
#' @details
#' A long gap followed by new prices (`max_gap_days` above 252 for 468 tickers) often
#' means a ticker passed to another company; the implied-price check tests it.
#' @noRd
sql_silver_terminal_events <- function(prices_d, last_calendar_day) {
  if (!inherits(last_calendar_day, "Date") || length(last_calendar_day) != 1) {
    stop("last_calendar_day must be a single Date.")
  }
  paste0(
    "SELECT instrument_id, any_value(ticker) AS ticker, min(date) AS first_date, ",
    "max(date) AS last_date, count(*) AS n_days, coalesce(max(gap), 0) AS max_gap_days, ",
    "max(date) = DATE '", format(last_calendar_day), "' AS is_live FROM (",
    "SELECT instrument_id, ticker, date, day_index - lag(day_index) OVER ",
    "(PARTITION BY instrument_id ORDER BY date) AS gap FROM ", prices_d,
    ") GROUP BY instrument_id"
  )
}

#' SQL for the silver daily prices: clean Tiingo prices keyed by instrument and date
#'
#' Keeps priced rows with a positive raw and adjusted close (bronze tolerates 9,680
#' adjusted closes of 0, a vendor rounding defect on tiny OTC prices), on trading days
#' of the SPY calendar, for tickers that identify exactly one instrument. Written as
#' SQL because the price table (28 million rows) stays in duckdb.
#'
#' @param prices Character scalar, SQL relation of bronze Tiingo prices (`ticker`,
#'   `status`, `date`, `close`, `adj_close`, `volume`, `div_cash`, `split_factor`).
#' @param master Character scalar, SQL relation of the security master (`ticker`,
#'   `instrument_id`, `share_class_figi`, `has_price_source`).
#' @param calendar Character scalar, SQL relation of the trading calendar with `date`
#'   and `day_index` (1, 2, ... in date order).
#'
#' @return Character scalar, a SELECT statement with one row per `instrument_id` and
#'   `date`: `id` (`<instrument_id>_<YYYY-MM-DD>`), `instrument_id`, `date`, `ticker`,
#'   `share_class_figi`, `close` (raw, USD), `adj_close` (split and dividend adjusted,
#'   USD), `volume` (shares), `dollar_volume` (`close * volume`, USD), `div_cash`,
#'   `split_factor` and `day_index`.
#' @noRd
sql_silver_prices_d <- function(prices, master, calendar) {
  paste0(
    "WITH px AS (SELECT ticker, date, close, adj_close, volume, div_cash, split_factor ",
    "FROM ", prices, " WHERE status = '200' AND close > 0 AND adj_close > 0), ",
    ## any_value() is safe: the master gives each priced ticker one instrument
    "map AS (SELECT ticker, any_value(instrument_id) AS instrument_id, ",
    "any_value(share_class_figi) AS share_class_figi FROM ", master,
    " WHERE has_price_source GROUP BY ticker) ",
    "SELECT m.instrument_id || '_' || strftime(px.date, '%Y-%m-%d') AS id, ",
    "m.instrument_id, px.date, px.ticker, m.share_class_figi, px.close, px.adj_close, ",
    "px.volume, px.close * px.volume AS dollar_volume, px.div_cash, px.split_factor, ",
    "c.day_index FROM px JOIN map m ON px.ticker = m.ticker ",
    "JOIN ", calendar, " c ON px.date = c.date"
  )
}

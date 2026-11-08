#' SQL for the daily SPY total return, the broad-market benchmark
#'
#' SPY is an ETF, so it is not in the silver price tables (US common stocks only); its
#' Tiingo adjusted close (dividends reinvested) is read from bronze.
#'
#' @param bronze_prices Character scalar, SQL relation of `bronze_tiingo_prices`.
#'
#' @return Character scalar, a SELECT statement with one row per SPY trading day after
#'   the first: `date` and `bench_return` (total return, percentage points).
#' @noRd
sql_gold_benchmark_d <- function(bronze_prices) {
  paste0(
    "SELECT date, bench_return FROM (SELECT date, 100 * (adj_close / lag(adj_close) ",
    "OVER (ORDER BY date) - 1) AS bench_return FROM ", bronze_prices,
    " WHERE ticker = 'SPY' AND status = '200' AND adj_close > 0) ",
    "WHERE bench_return IS NOT NULL ORDER BY date"
  )
}

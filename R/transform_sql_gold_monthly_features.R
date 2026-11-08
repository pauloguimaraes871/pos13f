#' SQL for monthly price features of each instrument, from data before each day
#'
#' Computes on every daily price row trailing features that end the day before (so a
#' feature dated t never uses the price, volume or return of t), then keeps each
#' instrument's last row of every calendar month: the features known at that month's
#' last trading day.
#'
#' @param prices_d Character scalar, SQL relation of `silver_prices_d` (`id`,
#'   `instrument_id`, `date`, `day_index`, `close`, `dollar_volume`).
#' @param returns_d Character scalar, SQL relation of `silver_returns_d` (`id`, `ret`).
#'
#' @return Character scalar, a SELECT statement with one row per `instrument_id` and
#'   `month` (Date, first day of the calendar month): `date` (the row's trading day),
#'   `close_prev` (USD), `mean_volfin_3m` and `median_volfin_3m` (USD per day, previous
#'   63 trading days), `presence` (percent of the previous 63 trading days with a
#'   price), `daily_vol` and `vol_3m` (standard deviation of daily returns over the
#'   previous 21 and 63 trading days, percentage points), `mom_12_1` (return from 252
#'   to 22 trading days before, percentage points), `rev_1m` (return over the previous
#'   21 trading days, percentage points), `amihud` (mean of absolute return over dollar
#'   volume over the previous 63 trading days with volume, per million USD) and `n_mom`
#'   (returns in the momentum window).
#'
#' @details
#' Windows are RANGE frames on the trading-day index, so they span calendar trading
#' days, not available prices: a stock that did not trade has fewer observations, which
#' `presence` and `n_mom` record. Momentum uses the 12-1 month convention of later
#' momentum factors \[mem\]; Jegadeesh and Titman (1993, p. 68) [src] rank on 1 to 4
#' past quarters and skip one week. Illiquidity follows Amihud (2002, p. 34, eq. 1)
#' [src]: the average over days of absolute return over dollar volume, here over 63
#' trading days instead of a year, and only over days with positive dollar volume
#' (Amihud averages over days with volume). Found by the toy-sample comparison on
#' 2026-11-08: averaging zero-volume days gave `NaN` (0/0) or `Inf`, and the `NaN`
#' alone caused every `missing_features` exclusion in the first gold run.
#' @noRd
sql_gold_monthly_features <- function(prices_d, returns_d) {
  w <- function(from, to) {
    paste0(
      "OVER (PARTITION BY instrument_id ORDER BY day_index RANGE BETWEEN ", from,
      " PRECEDING AND ", to, " PRECEDING)"
    )
  }
  paste0(
    "WITH d AS (SELECT p.instrument_id, p.date, p.day_index, p.close, p.dollar_volume, r.ret ",
    "FROM ", prices_d, " p LEFT JOIN ", returns_d, " r ON p.id = r.id), ",
    "f AS (SELECT instrument_id, date, date_trunc('month', date)::DATE AS month, ",
    "lag(close) OVER (PARTITION BY instrument_id ORDER BY day_index) AS close_prev, ",
    "avg(dollar_volume) ", w(63, 1), " AS mean_volfin_3m, ",
    "median(dollar_volume) ", w(63, 1), " AS median_volfin_3m, ",
    "100.0 * count(*) ", w(63, 1), " / 63 AS presence, ",
    "100 * stddev_samp(ret) ", w(21, 1), " AS daily_vol, ",
    "100 * stddev_samp(ret) ", w(63, 1), " AS vol_3m, ",
    "100 * (exp(sum(ln(1 + ret)) ", w(252, 22), ") - 1) AS mom_12_1, ",
    "count(ret) ", w(252, 22), " AS n_mom, ",
    "100 * (exp(sum(ln(1 + ret)) ", w(21, 1), ") - 1) AS rev_1m, ",
    ## Amihud averages over days that traded: a zero-volume day has no defined ratio
    "1e6 * avg(CASE WHEN dollar_volume > 0 THEN abs(ret) / dollar_volume END) ", w(63, 1),
    " AS amihud FROM d) ",
    "SELECT * EXCLUDE (rk) FROM (SELECT *, row_number() OVER ",
    "(PARTITION BY instrument_id, month ORDER BY date DESC) AS rk FROM f) WHERE rk = 1"
  )
}

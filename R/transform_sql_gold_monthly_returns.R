#' SQL for calendar-month total returns of each instrument, compounded from daily
#'
#' A buy-and-hold position over a month earns the product of its daily total returns,
#' so compounding loses nothing for monthly or quarterly portfolios. A return spanning
#' a gap belongs to the month of the day it is observed.
#'
#' @param returns_d Character scalar, SQL relation of `silver_returns_d` (`instrument_id`,
#'   `date`, `ret`, a fraction).
#'
#' @return Character scalar, a SELECT statement with one row per `instrument_id` and
#'   `month` (Date, first day of the month): `ret_m` (fraction), `n_days` (daily returns
#'   compounded) and `last_date` (last trading day with a return in the month).
#' @noRd
sql_gold_monthly_returns <- function(returns_d) {
  paste0(
    "SELECT instrument_id, date_trunc('month', date)::DATE AS month, ",
    "exp(sum(ln(1 + ret))) - 1 AS ret_m, count(*) AS n_days, max(date) AS last_date ",
    "FROM ", returns_d, " GROUP BY instrument_id, month"
  )
}

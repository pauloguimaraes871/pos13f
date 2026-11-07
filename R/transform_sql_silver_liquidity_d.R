#' SQL for the point-in-time illiquidity flag of each instrument and day
#'
#' Flags an instrument as illiquid on day t from data up to t-1 only: the previous
#' close and the median dollar volume of the previous 63 available prices (about three
#' months). Silver uses the flag to measure data quality on the stocks that matter;
#' gold applies the plan's universe rule (top 1,000 by dollar volume) on top.
#'
#' @param prices_d Character scalar, SQL relation of silver daily prices.
#' @param min_price Numeric scalar, USD; below it the stock is illiquid (plan 1.8).
#' @param min_dollar_volume Numeric scalar, USD per day, nominal.
#' @param min_days Numeric scalar, fewest prices among the previous 63 needed to judge.
#'
#' @return Character scalar, a SELECT statement with one row per `instrument_id` and
#'   `date`: `id`, `instrument_id`, `date`, `ticker`, `close_prev` (USD),
#'   `median_dollar_volume_63` (USD), `n_days_63` and `is_illiquid` (lgl).
#'
#' @details
#' Look-ahead: the window ends one row before t (`1 PRECEDING`), so the flag never
#' uses the price or volume of day t. The window counts available prices, so after a
#' gap it reaches further back in time. Thresholds are nominal dollars, not deflated.
#' @noRd
sql_silver_liquidity_d <- function(prices_d, min_price = 5, min_dollar_volume = 1e6,
                                   min_days = 42) {
  # Validate inputs
  for (x in list(min_price, min_dollar_volume, min_days)) {
    if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 0) {
      stop("min_price, min_dollar_volume and min_days must be single non-negative numbers.")
    }
  }

  # Previous close and trailing median, both ending the day before
  frame <- paste(
    "OVER (PARTITION BY instrument_id ORDER BY date",
    "ROWS BETWEEN 63 PRECEDING AND 1 PRECEDING)"
  )
  paste0(
    "SELECT *, (close_prev IS NULL OR n_days_63 < ", min_days, " OR close_prev < ",
    min_price, " OR median_dollar_volume_63 < ", format(min_dollar_volume, scientific = FALSE),
    ") AS is_illiquid FROM (SELECT id, instrument_id, date, ticker, ",
    "lag(close) OVER (PARTITION BY instrument_id ORDER BY date) AS close_prev, ",
    "median(dollar_volume) ", frame, " AS median_dollar_volume_63, ",
    "count(*) ", frame, " AS n_days_63 FROM ", prices_d, ")"
  )
}

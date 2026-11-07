#' Compute the figures the silver prices suite checks, inside duckdb
#'
#' The price, return and liquidity tables have about 28 million rows each, so the suite
#' reads these aggregates instead.
#'
#' @param con A duckdb connection.
#' @param rel Named list of SQL relations: `prices`, `returns`, `liquidity`, `terminal`,
#'   `identity`, and `calendar` (with `date`).
#'
#' @return A one-row tibble: `n_prices`, `n_duplicate_ids`, `n_nonpositive`,
#'   `n_off_calendar`, `n_tickers_multi`, `n_returns`, `n_liquidity`,
#'   `n_missing_flags`, `share_big_liquid` (share of liquid one-day returns beyond
#'   +-50%), `share_total_return_agree` (share of dividend and split days where the
#'   adjusted return equals the total return rebuilt from raw close, dividend and split
#'   factor, within 0.1 percentage point), `aapl_split_ret` (AAPL return on 2020-08-31,
#'   its 4-for-1 split day),
#'   `n_terminal`, `n_instruments`, `n_after_calendar` and `share_match_liquid` (share
#'   of liquid CUSIP-quarters held by 5 or more filers where most filers' implied price
#'   agrees with the close).
#' @noRd
summarise_silver_prices_checks <- function(con, rel) {
  one <- function(sql) DBI::dbGetQuery(con, sql)[[1]]
  r <- rel
  tibble::tibble(
    n_prices = one(paste("SELECT count(*) FROM", r$prices)),
    n_duplicate_ids = one(paste("SELECT count(*) - count(DISTINCT id) FROM", r$prices)),
    n_nonpositive = one(paste(
      "SELECT count(*) FROM", r$prices, "WHERE NOT (close > 0 AND adj_close > 0)"
    )),
    n_off_calendar = one(paste(
      "SELECT count(*) FROM", r$prices, "WHERE date NOT IN (SELECT date FROM", r$calendar, ")"
    )),
    n_tickers_multi = one(paste(
      "SELECT count(*) FROM (SELECT ticker FROM", r$prices,
      "GROUP BY ticker HAVING count(DISTINCT instrument_id) > 1)"
    )),
    n_returns = one(paste("SELECT count(*) FROM", r$returns)),
    n_liquidity = one(paste("SELECT count(*) FROM", r$liquidity)),
    n_missing_flags = one(paste("SELECT count(*) FROM", r$liquidity, "WHERE is_illiquid IS NULL")),
    share_big_liquid = one(paste(
      "SELECT avg((abs(t.ret) > 0.5)::INT) FROM", r$returns, "t JOIN", r$liquidity,
      "l ON t.id = l.id WHERE t.gap_days = 1 AND NOT l.is_illiquid"
    )),
    ## On dividend and split days, the adjusted return must equal the total return
    ## rebuilt from raw data: (close + dividend) / (previous close / split factor) - 1
    share_total_return_agree = one(paste(
      "SELECT avg((abs((adj_close / prev_adj - 1) - ((close + div_cash) /",
      "(prev_close / split_factor) - 1)) < 0.001)::INT) FROM (SELECT close, adj_close,",
      "div_cash, split_factor, lag(close) OVER w AS prev_close, lag(adj_close) OVER w AS",
      "prev_adj, day_index - lag(day_index) OVER w AS gap FROM", r$prices,
      "WINDOW w AS (PARTITION BY instrument_id ORDER BY date))",
      "WHERE gap = 1 AND (div_cash > 0 OR split_factor <> 1)"
    )),
    aapl_split_ret = one(paste(
      "SELECT any_value(ret) FROM", r$returns, "WHERE ticker = 'AAPL' AND date = DATE '2020-08-31'"
    )),
    n_terminal = one(paste("SELECT count(*) FROM", r$terminal)),
    n_instruments = one(paste("SELECT count(DISTINCT instrument_id) FROM", r$prices)),
    n_after_calendar = one(paste(
      "SELECT count(*) FROM", r$terminal, "WHERE last_date > (SELECT max(date) FROM",
      r$calendar, ")"
    )),
    share_match_liquid = one(paste(
      "SELECT avg((i.n_match > i.n_filers / 2)::INT) FROM", r$identity, "i JOIN",
      r$liquidity, "l ON i.instrument_id = l.instrument_id AND i.price_date = l.date",
      "WHERE i.n_filers >= 5 AND NOT l.is_illiquid"
    ))
  )
}

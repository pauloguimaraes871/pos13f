#' SQL for the implied-price check: do 13F values agree with Tiingo prices?
#'
#' Each filer's `value_usd / shares` is the price it reported at quarter end. Divided
#' by Tiingo's raw close on the last trading day up to quarter end, it should be close
#' to 1. When most filers of a CUSIP and quarter disagree with the price, the ticker's
#' prices belong to another company (ticker reuse); a ratio near 1,000 or 0.001 is a
#' unit error of that filer.
#'
#' @param state Character scalar, SQL relation of `silver_holdings_state`.
#' @param master Character scalar, SQL relation of the security master.
#' @param prices_d Character scalar, SQL relation of silver daily prices.
#'
#' @return Character scalar, a SELECT statement with one row per `cusip` and
#'   `period_of_report` with a price within 7 days before quarter end:
#'   `instrument_id`, `price_date`, `close` (raw, USD), `n_filers`, `n_match` (ratio
#'   within 0.5 to 2), `n_thousand` (ratio within 500 to 2,000) and `n_thousandth`
#'   (ratio within 0.0005 to 0.002).
#'
#' @details
#' Uses each quarter's latest version (`valid_to IS NULL`): a diagnostic of identity
#' and units, not a signal input. The raw close is the right comparison because 13F
#' share counts are not split-adjusted. Counts instead of a median keep the query
#' streaming over 77 million rows.
#' @noRd
sql_silver_identity_checks <- function(state, master, prices_d) {
  ratio <- "h.value_usd / h.shares / p.close"
  band <- function(lo, hi) {
    paste0("count(*) FILTER (WHERE ", ratio, " BETWEEN ", lo, " AND ", hi, ")")
  }
  paste0(
    "WITH h AS (SELECT s.cusip, s.period_of_report, s.value_usd, s.shares, m.instrument_id ",
    "FROM ", state, " s JOIN ", master, " m ON s.cusip = m.cusip ",
    "WHERE s.valid_to IS NULL AND s.shares > 0 AND m.has_price_source), ",
    "quarters AS (SELECT DISTINCT period_of_report FROM h), ",
    "grid AS (SELECT i.instrument_id, q.period_of_report FROM ",
    "(SELECT DISTINCT instrument_id FROM h) i CROSS JOIN quarters q), ",
    "px AS (SELECT instrument_id, date, close FROM ", prices_d, "), ",
    ## ASOF: the last price on or before quarter end; a stale one is not comparable
    "qp AS (SELECT g.instrument_id, g.period_of_report, px.date AS price_date, px.close ",
    "FROM grid g ASOF JOIN px ON g.instrument_id = px.instrument_id ",
    "AND g.period_of_report >= px.date ",
    "WHERE px.date >= g.period_of_report - INTERVAL 7 DAY) ",
    "SELECT h.cusip, h.period_of_report, any_value(h.instrument_id) AS instrument_id, ",
    "any_value(p.price_date) AS price_date, any_value(p.close) AS close, ",
    "count(*) AS n_filers, ", band(0.5, 2), " AS n_match, ", band(500, 2000),
    " AS n_thousand, ", band(0.0005, 0.002), " AS n_thousandth ",
    "FROM h JOIN qp p ON h.instrument_id = p.instrument_id ",
    "AND h.period_of_report = p.period_of_report GROUP BY h.cusip, h.period_of_report"
  )
}

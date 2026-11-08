#' SQL for breadth counts per formation and instrument, from the cohort only
#'
#' Counts, for each formation t and instrument, the cohort filers holding it in q-1 and
#' in q, using the holdings versions current at t. A filer holding two CUSIPs of one
#' instrument counts once. Also counts how many quarter-q holders report a value per
#' share agreeing with Tiingo's raw close at quarter end, the point-in-time identity
#' acceptance of memo deviation D2.
#'
#' @param state Character scalar, SQL relation of `silver_holdings_state`.
#' @param formations Character scalar, SQL relation of `build_formation_dates()`.
#' @param cohort Character scalar, SQL relation of `select_gold_cohort()` (uses
#'   `formation_date`, `cik`, `in_cohort`).
#' @param master Character scalar, SQL relation of `silver_security_master`.
#' @param prices_d Character scalar, SQL relation of `silver_prices_d`.
#'
#' @return Character scalar, a SELECT statement with one row per `formation_date` and
#'   `instrument_id` held by the cohort in q-1 or q: `n_prev`, `n_curr`, `n_init`
#'   (holders in q but not q-1), `n_exit` (holders in q-1 but not q) and `n_match`
#'   (quarter-q holders whose value per share is within 0.5 to 2 times the close).
#'
#' @details
#' Point in time: only versions with `valid_from <= t` and `valid_to > t` (or open) are
#' read, so an amendment filed after t never changes the count at t. The quarter-end
#' close predates t (the quarter ended at least two months before), so the identity
#' check uses no later price.
#' @noRd
sql_gold_breadth <- function(state, formations, cohort, master, prices_d) {
  paste0(
    "WITH s AS (SELECT f.formation_date, f.report_quarter, h.cik, m.instrument_id, ",
    "h.period_of_report = f.report_quarter AS is_curr, h.value_usd, h.shares ",
    "FROM ", state, " h JOIN ", formations, " f ",
    "ON h.period_of_report IN (f.report_quarter, f.prev_quarter) ",
    "AND h.valid_from <= f.formation_date ",
    "AND (h.valid_to IS NULL OR h.valid_to > f.formation_date) ",
    "JOIN ", cohort, " c ON c.formation_date = f.formation_date AND c.cik = h.cik ",
    "AND c.in_cohort JOIN ", master, " m ON m.cusip = h.cusip WHERE h.shares > 0), ",
    "holder AS (SELECT formation_date, instrument_id, cik, any_value(report_quarter) AS q, ",
    "bool_or(is_curr) AS in_curr, bool_or(NOT is_curr) AS in_prev, ",
    "sum(CASE WHEN is_curr THEN value_usd END) AS v, ",
    "sum(CASE WHEN is_curr THEN shares END) AS sh ",
    "FROM s GROUP BY formation_date, instrument_id, cik), ",
    "grid AS (SELECT DISTINCT instrument_id, q FROM holder), ",
    "px AS (SELECT instrument_id, date, close FROM ", prices_d, "), ",
    ## The last close on or before quarter end, at most 7 days stale
    "qp AS (SELECT g.instrument_id, g.q, px.close FROM grid g ASOF JOIN px ",
    "ON g.instrument_id = px.instrument_id AND g.q >= px.date ",
    "WHERE px.date >= g.q - INTERVAL 7 DAY) ",
    "SELECT h.formation_date, h.instrument_id, ",
    "count(*) FILTER (WHERE h.in_prev) AS n_prev, ",
    "count(*) FILTER (WHERE h.in_curr) AS n_curr, ",
    "count(*) FILTER (WHERE h.in_curr AND NOT h.in_prev) AS n_init, ",
    "count(*) FILTER (WHERE h.in_prev AND NOT h.in_curr) AS n_exit, ",
    "count(*) FILTER (WHERE h.in_curr AND h.v / h.sh / qp.close BETWEEN 0.5 AND 2) AS n_match ",
    "FROM holder h LEFT JOIN qp ON h.instrument_id = qp.instrument_id AND h.q = qp.q ",
    "GROUP BY h.formation_date, h.instrument_id"
  )
}

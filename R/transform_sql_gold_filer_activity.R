#' SQL for the filer activity of each formation: positions and turnover, point in time
#'
#' For each formation t and filer, reads the holdings versions of quarters q-1 and q
#' that were current at t (`valid_from <= t < valid_to`) and counts positions in each
#' quarter and positions held in both. Whether a filer had a report for each quarter at
#' t is decided from the version table, not here (an empty report has no state rows), so
#' a missing report is never read as an exit.
#'
#' @param state Character scalar, SQL relation of `silver_holdings_state`.
#' @param formations Character scalar, SQL relation with `formation_date`,
#'   `report_quarter` and `prev_quarter` (`build_formation_dates()`).
#'
#' @return Character scalar, a SELECT statement with one row per `formation_date` and
#'   `cik` holding at least one position in q-1 or q at t: `n_prev`, `n_curr` (distinct
#'   CUSIPs in q-1 and q), `n_both` and `value_curr` (USD, quarter q).
#'
#' @details
#' Positions are counted by CUSIP: the turnover measure of plan 1.9 is price-free and
#' needs no identity mapping. Turnover is `(n_prev + n_curr - 2 n_both) /
#' (n_prev + n_curr - n_both)`, computed in R from these counts.
#' @noRd
sql_gold_filer_activity <- function(state, formations) {
  paste0(
    "WITH s AS (SELECT f.formation_date, h.cik, h.cusip, h.value_usd, ",
    "h.period_of_report = f.report_quarter AS is_curr FROM ", state, " h JOIN ", formations,
    " f ON h.period_of_report IN (f.report_quarter, f.prev_quarter) ",
    ## Point in time: the version current at the formation date
    "AND h.valid_from <= f.formation_date ",
    "AND (h.valid_to IS NULL OR h.valid_to > f.formation_date) WHERE h.shares > 0), ",
    "p AS (SELECT formation_date, cik, cusip, bool_or(is_curr) AS in_curr, ",
    "bool_or(NOT is_curr) AS in_prev, sum(CASE WHEN is_curr THEN value_usd END) AS value ",
    "FROM s GROUP BY formation_date, cik, cusip) ",
    "SELECT formation_date, cik, count(*) FILTER (WHERE in_prev) AS n_prev, ",
    "count(*) FILTER (WHERE in_curr) AS n_curr, count(*) FILTER (WHERE in_prev AND in_curr) ",
    "AS n_both, coalesce(sum(value), 0) AS value_curr FROM p GROUP BY formation_date, cik"
  )
}

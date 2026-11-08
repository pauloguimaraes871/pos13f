#' Recompute the activity counts of a random sample of cohort filers, in dplyr
#'
#' Draws `n_filers` rows of the cohort table at random (fixed seed), pulls from duckdb
#' only those filers' holdings in the two compared quarters, and recomputes their
#' position counts with `reference_gold_activity()`. Every cohort label (few positions,
#' index-like, turnover) rests on these counts, which otherwise come only from
#' `sql_gold_filer_activity()`.
#'
#' @param con A duckdb connection.
#' @param cohort Tibble from `select_gold_cohort()`.
#' @param formations Tibble from `build_formation_dates()`.
#' @param rel Function returning the SQL relation of a pin name.
#' @param n_filers Numeric scalar, cohort rows drawn.
#' @param seed Integer scalar, the random seed.
#'
#' @return A tibble with the sampled `formation_date` and `cik`, the recomputed
#'   `n_prev`, `n_curr`, `n_both` and `value_curr`.
#' @noRd
build_gold_activity_reference <- function(con, cohort, formations, rel, n_filers = 20,
                                          seed = 20261108) {
  targets <- withr::with_seed(seed, dplyr::slice_sample(cohort, n = min(n_filers, nrow(cohort)))) |>
    dplyr::select("formation_date", "cik") |>
    dplyr::inner_join(dplyr::select(formations, "formation_date", "report_quarter", "prev_quarter"),
      by = dplyr::join_by("formation_date"), relationship = "many-to-one"
    )
  pairs <- dplyr::bind_rows(
    dplyr::select(targets, "cik", period_of_report = "report_quarter"),
    dplyr::select(targets, "cik", period_of_report = "prev_quarter")
  ) |>
    dplyr::distinct()
  duckdb::duckdb_register(con, "chk_filers", pairs)
  on.exit(duckdb::duckdb_unregister(con, "chk_filers"), add = TRUE)
  state <- tibble::as_tibble(DBI::dbGetQuery(con, paste0(
    "SELECT h.cik, h.cusip, h.period_of_report, h.valid_from, h.valid_to, h.shares, ",
    "h.value_usd FROM ", rel("silver_holdings_state"), " h JOIN chk_filers p ",
    "ON h.cik = p.cik AND h.period_of_report = p.period_of_report"
  )))
  reference_gold_activity(state, targets)
}

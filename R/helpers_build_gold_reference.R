#' Recompute a sample of gold panel rows from silver, in dplyr
#'
#' Pulls from duckdb only the silver rows the sampled panel rows depend on (holdings of
#' their CUSIPs in the two report quarters, and the full price and return history of
#' their instruments), then runs the dplyr references. The gold suite compares the
#' result with the panels ([run_test_gold()]).
#'
#' @param con A duckdb connection.
#' @param sample Tibble from `sample_gold_check_rows()`.
#' @param formations Tibble from `build_formation_dates()`.
#' @param cohort Tibble from `select_gold_cohort()`.
#' @param master Tibble pinned as `silver_security_master`.
#' @param terminal Tibble pinned as `silver_terminal_events`.
#' @param rel Function returning the SQL relation of a pin name (`silver_holdings_state`,
#'   `silver_prices_d`, `silver_returns_d`).
#' @param last_month Date scalar, first day of the calendar's unfinished last month.
#' @param activity_pool Tibble, the cohort rows the activity sample is drawn from (the
#'   whole cohort in production; filers with complete holdings in the toy tests).
#'
#' @return A named list: `sample` (with `formation_date` and `label_date`), `breadth`
#'   (`reference_gold_breadth()` rows of the sampled formations and instruments),
#'   `row_features` (features as of each row's date), `signal_features` (features as of
#'   each row's formation label), `returns` (`reference_gold_fwd_returns()`),
#'   `activity` (`build_gold_activity_reference()`, a random sample of cohort filers)
#'   and `available_strata` (strata that had rows to draw from).
#' @noRd
build_gold_reference <- function(con, sample, formations, cohort, master, terminal, rel,
                                 last_month, activity_pool = cohort) {
  available_strata <- attr(sample, "available_strata")
  # Formation of each sampled row: its holding month minus the signal's age
  sample <- sample |>
    dplyr::mutate(label_date = formation_label_of(.data$dates, .data$signal_age_months)) |>
    dplyr::inner_join(dplyr::select(formations, "formation_date", "label_date"),
      by = dplyr::join_by("label_date"), relationship = "many-to-one",
      unmatched = c("error", "drop")
    )
  sampled_formations <- dplyr::semi_join(formations, sample, by = dplyr::join_by("formation_date"))

  # Only the silver rows these sampled rows depend on
  pairs <- master |>
    dplyr::filter(.data$instrument_id %in% sample$tickers) |>
    dplyr::distinct(.data$cusip) |>
    dplyr::cross_join(tibble::tibble(period_of_report = unique(c(
      sampled_formations$report_quarter, sampled_formations$prev_quarter
    ))))
  duckdb::duckdb_register(con, "chk_pairs", pairs)
  duckdb::duckdb_register(con, "chk_inst", tibble::tibble(instrument_id = unique(sample$tickers)))
  on.exit(
    purrr::walk(c("chk_pairs", "chk_inst"), \(t) duckdb::duckdb_unregister(con, t)),
    add = TRUE
  )
  get <- function(sql) tibble::as_tibble(DBI::dbGetQuery(con, sql))
  state <- get(paste0(
    "SELECT h.cik, h.cusip, h.period_of_report, h.valid_from, h.valid_to, h.shares, ",
    "h.value_usd FROM ", rel("silver_holdings_state"), " h JOIN chk_pairs p ",
    "ON h.cusip = p.cusip AND h.period_of_report = p.period_of_report"
  ))
  of_sample <- function(name) {
    get(paste0("SELECT t.* FROM ", rel(name), " t JOIN chk_inst USING (instrument_id)"))
  }
  prices <- of_sample("silver_prices_d")
  returns <- of_sample("silver_returns_d")

  # References
  breadth <- reference_gold_breadth(state, sampled_formations, cohort, master, prices) |>
    dplyr::semi_join(dplyr::distinct(sample, .data$formation_date, instrument_id = .data$tickers),
      by = dplyr::join_by("formation_date", "instrument_id")
    )
  list(
    sample = sample,
    breadth = breadth,
    row_features = reference_gold_features(
      dplyr::transmute(sample, instrument_id = .data$tickers, as_of = .data$dates), prices, returns
    ),
    signal_features = reference_gold_features(
      dplyr::transmute(sample, instrument_id = .data$tickers, as_of = .data$label_date),
      prices, returns
    ),
    returns = reference_gold_fwd_returns(sample, returns, terminal, last_month),
    activity = build_gold_activity_reference(con, activity_pool, formations, rel),
    available_strata = available_strata
  )
}

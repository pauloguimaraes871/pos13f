#' Build every table and figure input of the report and memo from the pins
#'
#' The report and memo only render; every number they show is computed here, from
#' pinned tables, by tested functions. The result is a target, so `tar_quarto()`
#' re-renders whenever an upstream pin changes.
#'
#' @param board A `pins_board` from [init_pipeline_board()].
#'
#' @return A named list of tibbles and lists: `coverage`, `filing_delay`, `universe`,
#'   `ladder`, `costs`, `persistence`, `momentum`, `summary`, `rank_ic`,
#'   `rank_ic_by_formation`, `monotonicity`, `inference`, `spreads`, `benchmark`,
#'   `extremes`, `data_quality`, `file_rows`, `audit` (the filing-date audit rows),
#'   `cohort_exclusions`, `combination`, `leave_one_year_out`, `capm` (CAPM alpha of
#'   each spread), `uncertainty` (interval of each mean and minimum detectable effects)
#'   and `lineage`.
#' @export
build_report_tables <- function(board) {
  # Validate inputs
  if (!inherits(board, "pins_board")) stop("board must be a pins board (init_pipeline_board()).")

  read <- function(name) tibble::as_tibble(pins::pin_read(board, name))
  meta <- function(name) pins::pin_meta(board, name)$user
  filings <- read("silver_filings")
  signals <- read("gold_signals_m_df")
  ladder <- read("backtests_ladder_returns")
  spreads <- read("backtests_spreads")
  summary <- read("backtests_summary")
  cohort <- read("gold_cohort")
  list(
    coverage = report_filing_coverage(filings),
    filing_delay = report_filing_delay(filings),
    universe = report_universe_composition(read("gold_universe_diagnostics"), cohort),
    cohort_exclusions = report_cohort_exclusions(cohort),
    ladder = report_ladder_table(ladder),
    costs = report_cost_summary(ladder, spreads),
    persistence = report_signal_persistence(signals),
    momentum = report_momentum_context(read("bronze_ff_factors_m")),
    summary = summary,
    uncertainty = report_spread_uncertainty(summary),
    capm = report_capm_alphas(spreads, read("bronze_ff_factors_m")),
    rank_ic = read("backtests_rank_ic"),
    rank_ic_by_formation = read("backtests_rank_ic_by_formation"),
    monotonicity = read("backtests_monotonicity"),
    inference = read("backtests_inference"),
    spreads = spreads,
    benchmark = read("backtests_benchmark"),
    extremes = report_signal_extremes(signals, read("silver_security_master")),
    data_quality = report_data_quality(meta),
    file_rows = report_pin_rows(
      board, c("bronze_tiingo_prices", "silver_prices_d", "gold_daily_returns_d")
    ),
    audit = read("silver_filing_date_audit"),
    combination = report_momentum_combination(spreads),
    leave_one_year_out = report_leave_one_year_out(spreads),
    lineage = report_pin_lineage(board)
  )
}

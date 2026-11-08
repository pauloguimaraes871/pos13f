#' Deploy the backtests: quantile ladders, spreads, double sort and their evaluation
#'
#' For each signal and weighting method, runs the quintile ladder in factoRverse
#' (`run_quantile_ladder()`), builds the long-short spread under the cost and borrow
#' scenarios, and the momentum double sort under `cs`. Then evaluates every series with
#' `summarise_backtest_returns()`, the rank IC, the monotonicity test and the spread
#' inference, runs [run_test_backtests()] and pins the tables.
#'
#' @param board A `pins_board` from [init_pipeline_board()].
#' @param signals Character vector, signal columns of the gold signals panel.
#' @param methods Character vector, factoRverse weighting methods. `cs` is the
#'   pre-registered construction; `ew` and `rp` are robustness checks (a result counts as
#'   robust only if it holds under all three).
#' @param double_sort Character pair `c(signal, conditioner)` for the dependent double
#'   sort (memo Section 8, verdict 3), run under `cs`.
#' @param reference_signals Character vector, signals backtested under `cs` as a
#'   reference for reading the results (design `"reference"`; momentum by default), not
#'   as tested hypotheses.
#' @param log_path Character scalar, folder for the data-quality log.
#' @param verbose Logical; print progress.
#'
#' @return A tibble with one row per pinned table: `table` and `version`.
#'
#' @details
#' Every quintile cohort is also pinned whole, as the factoRverse `port_backtest_cohort`
#' S4 object (`backtests_cohort_<signal>_<method>_<design>`), so its slots and plot and
#' summary methods can be used directly; each keeps its two latest versions.
#' The terminal-stress scenario (plan 1.6: the month of a stock's last price compounded
#' with -30%) reruns each signal's `cs` ladder on `fwd_return_1m_stress`, as design
#' `"terminal_stress"`.
#' @export
deploy_backtests <- function(board, signals = "f1_breadth_change", methods = c("cs", "ew", "rp"),
                             double_sort = c("f1_breadth_change", "mom_12_1"),
                             reference_signals = "mom_12_1",
                             log_path = file.path("logs", "tests"), verbose = TRUE) {
  # Validate inputs
  if (!inherits(board, "pins_board")) stop("board must be a pins board (init_pipeline_board()).")

  inputs <- build_backtest_inputs(board, double_sorts = list(double_sort))
  factors <- pins::pin_read(board, "bronze_ff_factors_m")

  # Quintile ladders: every signal under every method
  grid <- tidyr::expand_grid(signal = signals, method = methods)
  ## Every cohort is also kept whole: its factoRverse S4 object (weights, transactions
  ## log, costs, workflow, plot and summary methods) is pinned next to the tidy tables
  ## An environment collects them from inside the map calls below
  cohorts <- new.env()
  keep <- function(cohort, signal, method, design) {
    assign(paste(signal, method, design, sep = "_"), cohort, envir = cohorts)
    extract_ladder_returns(cohort) |>
      dplyr::mutate(signal = .env$signal, method = .env$method, design = .env$design)
  }
  ladders <- purrr::pmap(grid, function(signal, method) {
    if (verbose) message("Backtest: ", signal, " under ", method)
    keep(run_quantile_ladder(inputs, signal, method), signal, method, "unconditional")
  })

  # Dependent double sort: the within-tercile rank, restricted to one tercile at a time
  rank_col <- paste0(double_sort[1], "_in_", double_sort[2])
  group_col <- paste0(double_sort[2], "_group")
  double <- purrr::map(1:3, function(k) {
    if (verbose) message("Backtest: ", rank_col, " in tercile ", k)
    rule <- build_group_rule(inputs$signals, group_col, k)
    label <- paste0(rank_col, "_t", k)
    keep(
      run_quantile_ladder(inputs, rank_col, "cs", and_rule = rule, label = label),
      rank_col, "cs", paste0("tercile_", k)
    )
  })
  # Terminal stress: the same cs ladders on the stressed forward returns
  stressed_inputs <- build_backtest_inputs(board,
    double_sorts = list(double_sort), fwd_col = "fwd_return_1m_stress"
  )
  stress <- purrr::map(signals, function(signal) {
    if (verbose) message("Backtest: ", signal, " under cs, terminal stress")
    label <- paste(signal, "cs_stress", sep = "_")
    keep(
      run_quantile_ladder(stressed_inputs, signal, "cs", label = label),
      signal, "cs", "terminal_stress"
    )
  })
  # Reference: momentum on the same universe and machinery, to read F1 against
  reference <- purrr::map(reference_signals, function(signal) {
    if (verbose) message("Backtest: reference ", signal, " under cs")
    keep(run_quantile_ladder(inputs, signal, "cs"), signal, "cs", "reference")
  })
  ladder <- dplyr::bind_rows(ladders, double, stress, reference)

  # Evaluation, test, pins
  results <- evaluate_backtests(ladder, inputs, factors, board, signals)
  ok <- run_logged_test(function() run_test_backtests(ladder, results$spreads, results$benchmark),
    log_file = "test_backtests", log_path = log_path
  )
  check_test_success(ok)
  pinned <- write_backtest_pins(board, c(list(ladder_returns = ladder), results), verbose)
  cohort_names <- sort(ls(cohorts))
  dplyr::bind_rows(pinned, tibble::tibble(
    table = paste0("cohort_", cohort_names),
    version = purrr::map_chr(cohort_names, function(name) {
      write_backtest_cohort(board, get(name, envir = cohorts), name, verbose)
    })
  ))
}

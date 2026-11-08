test_that("a cohort is pinned whole and old versions are pruned", {
  inputs <- toy_backtest_inputs()
  ## Three different cohorts written under one name: pins skips unchanged content
  cohorts <- list(
    run_quantile_ladder(inputs, "f1_breadth_change", "ew", n_quantiles = 2),
    run_quantile_ladder(inputs, "f1_breadth_change", "cs", n_quantiles = 2),
    run_quantile_ladder(inputs, "mom_12_1", "ew", n_quantiles = 2)
  )
  board <- init_pipeline_board(withr::local_tempdir())
  for (p in c("gold_signals_m_df", "gold_fwd_1m_returns_m_df", "gold_liquidity_m_df")) {
    save_local_pin(board, tibble::tibble(x = 1), "gold", sub("^gold_", "", p), verbose = FALSE)
  }
  for (cohort in cohorts) {
    write_backtest_cohort(board, cohort, "f1_ew_unconditional", verbose = FALSE, keep_versions = 2)
  }
  cohort <- pins::pin_read(board, "backtests_cohort_f1_ew_unconditional")
  expect_s4_class(cohort, "port_backtest_cohort")
  expect_equal(nrow(pins::pin_versions(board, "backtests_cohort_f1_ew_unconditional")), 2)
  expect_error(write_backtest_cohort(board, list(), "x", verbose = FALSE), "port_backtest_cohort")
})

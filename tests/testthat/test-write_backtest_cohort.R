test_that("a cohort is pinned whole and old versions are pruned", {
  inputs <- toy_backtest_inputs()
  ## Three cohorts written under one name; versions are pruned to two
  cohorts <- list(
    run_quantile_ladder(inputs, "f1_breadth_change", "ew", n_quantiles = 2),
    run_quantile_ladder(inputs, "f1_breadth_change", "cs", n_quantiles = 2),
    run_quantile_ladder(inputs, "mom_12_1", "ew", n_quantiles = 2)
  )
  board <- init_pipeline_board(withr::local_tempdir())
  sources <- c(
    "gold_signals_m_df", "gold_fwd_1m_returns_m_df", "gold_liquidity_m_df",
    "gold_volatility_m_df", "gold_daily_returns_d"
  )
  for (p in sources) {
    save_local_pin(board, tibble::tibble(x = 1), "gold", sub("^gold_", "", p), verbose = FALSE)
  }
  for (cohort in cohorts) {
    write_backtest_cohort(board, cohort, "f1_ew_unconditional", verbose = FALSE, keep_versions = 2)
  }
  cohort <- pins::pin_read(board, "backtests_cohort_f1_ew_unconditional")
  expect_s4_class(cohort, "port_backtest_cohort")
  expect_equal(nrow(pins::pin_versions(board, "backtests_cohort_f1_ew_unconditional")), 2)
  ## Lineage lists every input, and an identical cohort is still written
  meta <- pins::pin_meta(board, "backtests_cohort_f1_ew_unconditional")$user
  expect_setequal(names(meta$inputs), sources)
  before <- pins::pin_meta(board, "backtests_cohort_f1_ew_unconditional")$local$version
  Sys.sleep(1.1)
  write_backtest_cohort(board, cohorts[[3]], "f1_ew_unconditional", verbose = FALSE)
  after <- pins::pin_meta(board, "backtests_cohort_f1_ew_unconditional")$local$version
  expect_false(identical(before, after))
  expect_error(write_backtest_cohort(board, list(), "x", verbose = FALSE), "port_backtest_cohort")
})

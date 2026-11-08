backtest_suite_passes <- function(ladder, env = parent.frame()) {
  spreads <- compute_spread_returns(dplyr::filter(ladder, !is.na(.data$raw_return))) |>
    dplyr::mutate(signal = "f1_breadth_change", method = "cs", design = "unconditional")
  ## A made-up benchmark keeps the toy self-contained; the correlation bound is relaxed
  benchmark <- ladder |>
    dplyr::filter(!is.na(.data$raw_return)) |>
    dplyr::summarise(ret = mean(.data$raw_return), .by = "dates") |>
    dplyr::mutate(series = "spy")
  suppressMessages(run_logged_test(
    function() {
      run_test_backtests(ladder, spreads, benchmark, min_holdings = 4, max_spread_corr = 1)
    },
    log_file = "test_backtests", log_path = withr::local_tempdir(.local_envir = env)
  ))
}

test_that("the backtest suite passes on the toy ladder", {
  expect_true(backtest_suite_passes(toy_ladder()))
})

test_that("the suite fails on an unsorted ladder, broken accounting or thin portfolios", {
  ladder <- toy_ladder()
  swapped <- dplyr::mutate(ladder,
    signal_level = dplyr::if_else(.data$quantile == 1, 1, .data$signal_level)
  )
  expect_false(backtest_suite_passes(swapped))
  broken <- dplyr::mutate(ladder, net_return = .data$net_return + 0.01)
  expect_false(backtest_suite_passes(broken))
  thin <- dplyr::mutate(ladder, n_holdings = 2)
  expect_false(backtest_suite_passes(thin))
})

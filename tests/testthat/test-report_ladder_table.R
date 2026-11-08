test_that("finished months are annualised per portfolio with signal and size levels", {
  out <- report_ladder_table(report_ladder_fixture())
  expect_equal(out$quantile, 1:2)
  expect_equal(out$ann_return, 12 * c(mean(c(1, -1, 2, 0, 1)), mean(c(2, 0, 3, 1, 2))))
  expect_equal(out$liquidity_level, c(0.4, 0.7))
  expect_true(out$signal_level[2] > out$signal_level[1])
})

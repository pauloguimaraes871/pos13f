test_that("the combination is the average of the two spreads, with its correlation", {
  months <- seq(as.Date("2015-01-01"), by = "month", length.out = 6)
  spreads <- tibble::tibble(
    signal = rep(c("mom_12_1", "f1_breadth_change"), each = 6),
    method = "cs", design = rep(c("reference", "unconditional"), each = 6),
    cost_bps = 0, borrow_bps = 0, dates = rep(months, 2),
    gross_return = c(2, -1, 3, 0, 1, 2, 1, 1, -1, 2, 0, 1)
  )
  ## A second F1 design on the same months must not enter the combination
  stress <- dplyr::mutate(spreads[7:12, ], design = "terminal_stress", gross_return = 50)
  out <- report_momentum_combination(dplyr::bind_rows(spreads, stress))
  expect_equal(out$table$portfolio, c("Momentum alone", "F1 alone", "50% momentum + 50% F1"))
  expect_equal(out$table$ann_return[3], 12 * mean(c(1.5, 0, 1, 1, 0.5, 1.5)))
  expect_equal(out$correlation, stats::cor(c(2, -1, 3, 0, 1, 2), c(1, 1, -1, 2, 0, 1)))
})

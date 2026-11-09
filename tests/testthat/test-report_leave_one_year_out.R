## One spread over three partial years (5, 12 and 3 months), gross and at a cost
loyo_fixture <- function() {
  months <- seq(as.Date("2014-08-01"), by = "month", length.out = 20)
  gross <- c(rep(1, 5), rep(0.5, 12), rep(-4, 3))
  tibble::tibble(
    signal = "f1", method = "cs", design = "unconditional",
    cost_bps = rep(c(0, 25), each = 20), borrow_bps = 0, dates = rep(months, 2),
    gross_return = rep(gross, 2)
  )
}

test_that("each year is set against the mean of every other month", {
  out <- report_leave_one_year_out(loyo_fixture())
  expect_equal(out$year, c(2014, 2015, 2016))
  expect_equal(out$n_months, c(5, 12, 3))
  expect_equal(out$year_mean, c(12, 6, -48))
  ## Without 2016: 5 months at 1 and 12 at 0.5
  expect_equal(out$mean_without[3], 12 * (5 + 6) / 17)
  expect_equal(unique(out$mean_all), 12 * (5 + 6 - 12) / 20)
})

test_that("a cost scenario with no rows or a missing column is refused", {
  expect_error(report_leave_one_year_out(loyo_fixture(), cost_bps = 10), "no rows")
  expect_error(report_leave_one_year_out(loyo_fixture()[, -7]), "gross_return")
})

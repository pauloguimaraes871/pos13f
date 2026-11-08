test_that("filings, filers, amendments and the 45-day share are counted per quarter", {
  out <- report_filing_coverage(report_filings_fixture())
  expect_equal(out$period_of_report, as.Date(c("2019-03-31", "2019-06-30")))
  expect_equal(out$n_filings, c(3, 2))
  expect_equal(out$n_filers, c(3, 2))
  expect_equal(out$n_amendments, c(0, 1))
  ## March: 40 and 46 days are usable by day 45 only for the first; June: 46 and 45 days
  expect_equal(out$share_by_day_45, c(1 / 3, 1 / 2))
})

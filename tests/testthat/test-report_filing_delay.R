test_that("the usable share rises with days and ignores amendments", {
  filings <- dplyr::mutate(report_filings_fixture(),
    period_of_report = .data$period_of_report + 365
  )
  out <- report_filing_delay(filings, days = c(40, 46, 200))
  expect_false(is.unsorted(out$share_usable))
  expect_equal(out$share_usable[3], 1)
  expect_equal(nrow(out), 3)
})

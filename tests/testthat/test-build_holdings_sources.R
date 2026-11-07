test_that("each version lists its base filing and the appends after it", {
  sources <- build_holdings_sources(order_holdings_events(holdings_filings_fixture()))
  q3 <- quarter_of(sources, "0000000101", "2022-09-30")

  by_version <- split(q3$accession_number, q3$valid_from)
  expect_equal(by_version[["2022-11-15"]], "0000000101-22-000001")
  expect_equal(by_version[["2022-12-02"]], c("0000000101-22-000001", "0000000101-22-000002"))
  ## The restatement drops the original and its first append
  expect_equal(by_version[["2023-02-02"]], "0000000101-23-000001")
  expect_equal(by_version[["2023-02-13"]], c("0000000101-23-000001", "0000000101-23-000002"))
  expect_equal(q3$is_base, c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE))
})

test_that("an invisible same-day version has no sources", {
  sources <- build_holdings_sources(order_holdings_events(holdings_filings_fixture()))
  expect_false("0000000202-23-000003" %in% sources$accession_number)
  expect_true("0000000202-23-000004" %in% sources$accession_number)
})

test_that("build_holdings_sources names missing columns", {
  events <- dplyr::select(order_holdings_events(holdings_filings_fixture()), -"base_no")
  expect_error(build_holdings_sources(events), "base_no")
})

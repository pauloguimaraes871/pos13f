test_that("order_holdings_events chains replaces and appends within each quarter", {
  events <- order_holdings_events(holdings_filings_fixture())
  q3 <- quarter_of(events, "0000000101", "2022-09-30")

  expect_equal(q3$kind, c("replace", "append", "replace", "append"))
  ## The restatement (event 3) is the base of itself and of the append after it
  expect_equal(q3$base_no, c(1L, 1L, 3L, 3L))
  expect_equal(q3$valid_from, as.Date(c("2022-11-15", "2022-12-02", "2023-02-02", "2023-02-13")))
  expect_equal(q3$valid_to, as.Date(c("2022-12-02", "2023-02-02", "2023-02-13", NA)))
})

test_that("order_holdings_events ignores amendments without a type and counts them", {
  events <- order_holdings_events(holdings_filings_fixture())
  expect_false("0000000101-23-000004" %in% events$accession_number)
  expect_equal(attr(events, "n_untyped_ignored"), 1)
  ## The original of 2022Q4 stays current
  q4 <- quarter_of(events, "0000000101", "2022-12-31")
  expect_equal(q4$valid_to, as.Date(NA))
})

test_that("an append before any original starts from an empty state", {
  events <- order_holdings_events(holdings_filings_fixture())
  b <- quarter_of(events, "0000000202", "2022-12-31")
  expect_equal(b$kind, c("append", "replace"))
  expect_equal(b$base_no, c(0L, 2L))
})

test_that("of two filings usable the same day only the later accession is visible", {
  events <- order_holdings_events(holdings_filings_fixture())
  same_day <- dplyr::filter(events, .data$period_of_report == as.Date("2023-03-31"))
  expect_equal(same_day$accession_number, c("0000000202-23-000003", "0000000202-23-000004"))
  expect_equal(same_day$is_visible, c(FALSE, TRUE))
})

test_that("a late report updates its own quarter from its own usable date", {
  ## Filed 2023-02-09, placed by SEC in the 2023q1 archive, reporting 2022Q2
  events <- order_holdings_events(holdings_filings_fixture())
  late <- dplyr::filter(events, .data$accession_number == "0000000303-23-000001")
  expect_equal(late$period_of_report, as.Date("2022-06-30"))
  expect_equal(late$valid_from, as.Date("2023-02-10"))
})

test_that("order_holdings_events names missing columns and rejects NA usable dates", {
  no_dates <- dplyr::select(holdings_filings_fixture(), -"usable_date")
  expect_error(order_holdings_events(no_dates), "usable_date")
  bad <- dplyr::mutate(holdings_filings_fixture(), usable_date = dplyr::if_else(
    dplyr::row_number() == 1, as.Date(NA), .data$usable_date
  ))
  expect_error(order_holdings_events(bad), "NA")
})

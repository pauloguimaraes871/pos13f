holdings_suite_passes <- function(sources = NULL, filings = holdings_filings_fixture(),
                                  checks = checks_fixture()) {
  events <- order_holdings_events(holdings_filings_fixture())
  if (is.null(sources)) sources <- build_holdings_sources(events)
  suppressMessages(run_logged_test(
    function() run_test_silver_holdings(events, sources, filings, checks),
    log_file = "test_silver_holdings",
    log_path = withr::local_tempdir(.local_envir = parent.frame())
  ))
}

fixture_sources <- function() {
  build_holdings_sources(order_holdings_events(holdings_filings_fixture()))
}
restatement <- "0000000101-23-000001"
late_report <- "0000000303-23-000001"

test_that("the holdings suite passes on the scenario fixture", {
  expect_true(holdings_suite_passes())
})

test_that("the suite fails when a version starts before its filing is usable", {
  ## Look-ahead: the restatement's positions shown one day before it was public
  sources <- dplyr::mutate(fixture_sources(), valid_from = dplyr::if_else(
    .data$accession_number == restatement, as.Date("2023-02-01"), .data$valid_from
  ))
  expect_false(holdings_suite_passes(sources = sources))
})

test_that("the suite fails when a quarter is taken from the wrong filing period", {
  ## As if the late 2022Q2 report counted for the quarter of its archive
  sources <- dplyr::mutate(fixture_sources(), period_of_report = dplyr::if_else(
    .data$accession_number == late_report, as.Date("2022-12-31"), .data$period_of_report
  ))
  expect_false(holdings_suite_passes(sources = sources))
})

test_that("the suite fails when an applied filing never reaches the state", {
  sources <- dplyr::filter(fixture_sources(), .data$accession_number != late_report)
  expect_false(holdings_suite_passes(sources = sources))
})

test_that("the suite fails on duplicated or lost state rows", {
  state <- holdings_state_fixture()
  expect_false(holdings_suite_passes(checks = checks_fixture(dplyr::bind_rows(state, state[1, ]))))
  expect_false(holdings_suite_passes(checks = checks_fixture(state[-1, ])))
})

test_that("the suite fails when a filing lost its whole information table", {
  ## One of twelve filings has no bronze row left: its count is missing, not small
  checks <- checks_fixture()
  checks$entry_counts <- dplyr::filter(
    checks$entry_counts, .data$accession_number != "0000000202-23-000002"
  )
  expect_false(holdings_suite_passes(checks = checks))
})

test_that("the suite fails when bronze row counts disagree with many summary pages", {
  filings <- dplyr::mutate(holdings_filings_fixture(), table_entry_total = 50)
  expect_false(holdings_suite_passes(filings = filings))
})

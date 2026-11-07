## Positions of one filer, quarter and version, as cusip = shares
held <- function(state, filer, period, from) {
  rows <- dplyr::filter(
    state,
    .data$cik == .env$filer, .data$period_of_report == as.Date(.env$period),
    .data$valid_from == as.Date(.env$from)
  )
  stats::setNames(rows$shares, rows$cusip)
}

## Real CUSIPs of the fixture
aapl <- "037833100"
msft <- "594918104"
tsla <- "88160R101"

test_that("original, new holdings and restatement give the expected state over time", {
  state <- holdings_state_fixture()
  q3 <- function(from) held(state, "0000000101", "2022-09-30", from)
  expect_equal(q3("2022-11-15"), stats::setNames(c(900, 800), c(aapl, msft)))
  ## NEW HOLDINGS adds TSLA to the original
  expect_equal(q3("2022-12-02"), stats::setNames(c(900, 800, 100), c(aapl, msft, tsla)))
  ## The restatement replaces everything; the next NEW HOLDINGS adds MSFT back
  expect_equal(q3("2023-02-02"), stats::setNames(900, aapl))
  expect_equal(q3("2023-02-13"), stats::setNames(c(900, 800), c(aapl, msft)))
})

test_that("an ignored untyped amendment leaves the original state current", {
  state <- holdings_state_fixture()
  q4 <- quarter_of(state, "0000000101", "2022-12-31")
  expect_equal(q4$cusip, aapl)
  expect_equal(q4$valid_to, as.Date(NA))
})

test_that("values are in dollars whatever the regime of each filing", {
  state <- quarter_of(holdings_state_fixture(), "0000000101", "2022-09-30")
  v1 <- dplyr::filter(state, .data$valid_from == as.Date("2022-11-15"))
  expect_equal(sum(v1$value_usd), 350000)
  v4 <- dplyr::filter(state, .data$valid_from == as.Date("2023-02-13"))
  expect_equal(sum(v4$value_usd), 330000)
})

test_that("an original after an early append replaces it; same-day versions collapse", {
  state <- holdings_state_fixture()
  expect_equal(held(state, "0000000202", "2022-12-31", "2023-02-02"), stats::setNames(200, tsla))
  expect_equal(held(state, "0000000202", "2022-12-31", "2023-02-15"), stats::setNames(250, msft))
  ## Original and restatement usable 2023-05-16: only the restatement is ever seen
  same_day <- dplyr::filter(state, .data$period_of_report == as.Date("2023-03-31"))
  expect_equal(same_day$shares, 260)
})

test_that("a late report enters its own quarter only from its usable date", {
  state <- holdings_state_fixture()
  late <- quarter_of(state, "0000000303", "2022-06-30")
  expect_equal(late$valid_from, as.Date("2023-02-10"))
  expect_equal(late$shares, 180)
  ## At a formation on 2022-11-30 the filer's latest known quarter is 2022Q3, not 2022Q2
  filer <- dplyr::filter(state, .data$cik == "0000000303")
  known <- dplyr::filter(filer, .data$valid_from <= as.Date("2022-11-30"))
  expect_equal(unique(known$period_of_report), as.Date("2022-09-30"))
})

test_that("a NEW HOLDINGS position already reported is summed and counted in n_sources", {
  filings <- holdings_filings_fixture()
  infotable <- dplyr::bind_rows(holdings_infotable_fixture(), tibble::tibble(
    ACCESSION_NUMBER = "0000000101-22-000002", CUSIP = aapl, VALUE = 20,
    SSHPRNAMT = 100, SSHPRNAMTTYPE = "SH", PUTCALL = NA
  ))
  positions <- summarise_holdings_positions(infotable, list_valid_position_cusips(infotable),
    regimes = dplyr::select(filings, "accession_number", "value_regime")
  )
  sources <- build_holdings_sources(order_holdings_events(filings))
  state <- build_silver_holdings_state(positions, sources)
  added <- dplyr::filter(state, .data$valid_from == as.Date("2022-12-02"), .data$cusip == aapl)
  expect_equal(added$shares, 1000)
  expect_equal(added$n_sources, 2L)
})

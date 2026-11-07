replies_fixture <- tibble::tibble(
  cusip = c(
    "037833100", "084670702", "018490102", "464286731", "99999X999", "02079K305",
    "02079K305"
  ),
  status = c("found", "found", "found", "found", "No identifier found.", "found", "found"),
  ticker = c("AAPL", "BRK/B", "1284849D", "EWT", NA, "GOOGL", "GOOGL"),
  exch_code = "US",
  security_type = c(
    "Common Stock", "Common Stock", "Common Stock", "ETP", NA,
    "Common Stock", "Common Stock"
  )
)

test_that("list_tiingo_tickers keeps US common stocks in Tiingo's format, plus SPY", {
  tickers <- list_tiingo_tickers(replies_fixture)

  ## BRK/B is BRK-B at Tiingo; the ETF is out of scope; GOOGL is requested once
  expect_equal(tickers$ticker, c("AAPL", "BRK-B", "GOOGL", "SPY"))
})

test_that("list_tiingo_tickers skips Bloomberg codes for dead securities", {
  ## Tiingo answered 404 for every such code in the G1 spike (1284849D is Allergan)
  tickers <- list_tiingo_tickers(replies_fixture)
  expect_false("1284849D" %in% tickers$ticker)
})

positions_fixture <- function() {
  infotable <- holdings_infotable_fixture()
  summarise_holdings_positions(infotable,
    valid_cusips = list_valid_position_cusips(infotable),
    regimes = dplyr::select(holdings_filings_fixture(), "accession_number", "value_regime")
  )
}

test_that("share rows of one filing and CUSIP are summed; options and bonds are not", {
  first <- dplyr::filter(positions_fixture(), .data$accession_number == "0000000101-22-000001")
  expect_equal(sort(first$cusip), c("037833100", "594918104"))
  aapl <- dplyr::filter(first, .data$cusip == "037833100")
  expect_equal(aapl$shares, 900)
  expect_equal(aapl$n_rows, 2L)
})

test_that("VALUE is converted to dollars by the filing's regime", {
  positions <- positions_fixture()
  ## Thousands before 2023-01-03: 150 thousand dollars
  aapl_2022 <- dplyr::filter(
    positions,
    .data$accession_number == "0000000101-22-000001", .data$cusip == "037833100"
  )
  expect_equal(aapl_2022$value_usd, 150000)
  ## The old-quarter restatement filed in 2023 is already in dollars
  aapl_2023 <- dplyr::filter(positions, .data$accession_number == "0000000101-23-000001")
  expect_equal(aapl_2023$value_usd, 140000)
})

test_that("CUSIPs are trimmed and upper-cased; malformed ones are dropped", {
  q4 <- dplyr::filter(positions_fixture(), .data$accession_number == "0000000101-23-000003")
  expect_equal(q4$cusip, "037833100")
  expect_equal(attr(list_valid_position_cusips(holdings_infotable_fixture()), "n_dropped"), 1)
})

test_that("positions of filings outside silver are left out", {
  regimes <- dplyr::filter(
    dplyr::select(holdings_filings_fixture(), "accession_number", "value_regime"),
    .data$accession_number != "0000000303-23-000001"
  )
  infotable <- holdings_infotable_fixture()
  valid <- list_valid_position_cusips(infotable)
  positions <- summarise_holdings_positions(infotable, valid, regimes)
  expect_false("0000000303-23-000001" %in% positions$accession_number)
})

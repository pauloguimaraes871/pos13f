test_that("silver prices keep positive prices on trading days of attributable tickers", {
  con <- prices_con_fixture()
  prices <- query_fixture(con, "SELECT * FROM prices ORDER BY instrument_id, date")

  ## ABAT names two instruments, so its prices are not attributed to either
  expect_setequal(unique(prices$ticker), c("AAA", "BBB"))
  aaa <- dplyr::filter(prices, .data$ticker == "AAA")
  ## 88 weekdays, less the adjusted close of 0; the Saturday row is not a trading day
  expect_equal(nrow(aaa), nrow(prices_calendar_fixture()) - 1)
  expect_false(as.Date("2020-06-06") %in% aaa$date)
  expect_equal(aaa$id[1], "FIGI_AAA_2020-06-01")
  expect_equal(aaa$dollar_volume[1], 1e7)
})

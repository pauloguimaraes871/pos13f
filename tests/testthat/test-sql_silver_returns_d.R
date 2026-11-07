test_that("returns use the adjusted close and flag gaps", {
  con <- prices_con_fixture()
  ## A 4-for-1 split: raw close falls by 75%, adjusted close rises 1%
  DBI::dbExecute(con, paste(
    "UPDATE prices SET close = 2.5, adj_close = 10.1",
    "WHERE instrument_id = 'FIGI_AAA' AND date = DATE '2020-08-31'"
  ))
  sql <- paste(sql_silver_returns_d("prices"), "ORDER BY instrument_id, date")
  returns <- query_fixture(con, sql)

  split_day <- dplyr::filter(returns, .data$ticker == "AAA", .data$date == as.Date("2020-08-31"))
  expect_equal(split_day$ret, 0.01, tolerance = 1e-9)
  ## BBB misses five weekdays: the next return spans six trading days
  bbb <- dplyr::filter(returns, .data$ticker == "BBB")
  expect_equal(max(bbb$gap_days), 6)
  ## No return on an instrument's first price
  expect_equal(nrow(returns), query_fixture(con, "SELECT count(*) - 2 AS n FROM prices")$n)
})

liquidity <- function(con) {
  query_fixture(con, paste(sql_silver_liquidity_d("prices"), "ORDER BY instrument_id, date"))
}

test_that("a cheap stock is illiquid and a liquid one is not, after enough history", {
  flags <- liquidity(prices_con_fixture())
  aaa <- dplyr::filter(flags, .data$ticker == "AAA")
  ## Too little history at first, then 10 USD and 10 million USD a day
  expect_true(aaa$is_illiquid[1])
  expect_false(aaa$is_illiquid[nrow(aaa)])
  bbb <- dplyr::filter(flags, .data$ticker == "BBB")
  expect_true(all(bbb$is_illiquid))
})

test_that("the flag on day t ignores the price and volume of day t", {
  con <- prices_con_fixture()
  before <- dplyr::filter(liquidity(con), .data$ticker == "AAA")
  last_day <- max(before$date)
  ## A crash to 1 cent on the last day must not change that day's flag
  DBI::dbExecute(con, paste0(
    "UPDATE prices SET close = 0.01, volume = 1, dollar_volume = 0.01 ",
    "WHERE instrument_id = 'FIGI_AAA' AND date = DATE '", format(last_day), "'"
  ))
  after <- dplyr::filter(liquidity(con), .data$ticker == "AAA")
  expect_equal(after$is_illiquid, before$is_illiquid)
  expect_equal(after$close_prev, before$close_prev)
})

test_that("sql_silver_liquidity_d rejects invalid thresholds", {
  expect_error(sql_silver_liquidity_d("prices", min_price = -1), "non-negative")
})

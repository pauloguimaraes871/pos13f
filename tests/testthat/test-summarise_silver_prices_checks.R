## The figures of the fixture tables, built the way deploy_silver_prices() builds them
fixture_checks <- function(con) {
  last_day <- max(prices_calendar_fixture()$date)
  for (x in list(
    c("returns", sql_silver_returns_d("prices")),
    c("liquidity", sql_silver_liquidity_d("prices")),
    c("terminal", sql_silver_terminal_events("prices", last_day))
  )) {
    DBI::dbExecute(con, paste("CREATE OR REPLACE TABLE", x[1], "AS", x[2]))
  }
  DBI::dbExecute(con, paste(
    "CREATE OR REPLACE TABLE identity AS SELECT 'x' AS instrument_id,",
    "DATE '2020-06-30' AS price_date, 1 AS n_filers, 1 AS n_match"
  ))
  rel <- list(
    prices = "prices", returns = "returns", liquidity = "liquidity", terminal = "terminal",
    identity = "identity", calendar = "calendar"
  )
  summarise_silver_prices_checks(con, rel)
}

test_that("summarise_silver_prices_checks reads the figures of the fixture tables", {
  checks <- fixture_checks(prices_con_fixture())

  expect_equal(checks$n_duplicate_ids, 0)
  expect_equal(checks$n_instruments, 2)
  expect_equal(checks$n_returns, checks$n_prices - 2)
  expect_equal(checks$n_terminal, 2)
  ## No AAPL in the fixture
  expect_true(is.na(checks$aapl_split_ret))
})

test_that("the total-return figure tells dividend-adjusted from price-only prices", {
  ## AAA pays 0.10 on 2020-07-01: the raw close drops to 9.90, the total return is 0
  con <- prices_con_fixture()
  DBI::dbExecute(con, paste(
    "UPDATE prices SET close = 9.9, div_cash = 0.1",
    "WHERE instrument_id = 'FIGI_AAA' AND date = DATE '2020-07-01'"
  ))
  expect_equal(fixture_checks(con)$share_total_return_agree, 1)
  ## A price-only series would drop the adjusted close with the dividend
  DBI::dbExecute(con, paste(
    "UPDATE prices SET adj_close = 9.9",
    "WHERE instrument_id = 'FIGI_AAA' AND date = DATE '2020-07-01'"
  ))
  expect_equal(fixture_checks(con)$share_total_return_agree, 0)
})

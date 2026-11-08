test_that("members stay for the holding months with the formation's signal frozen", {
  signals <- panels_fixture()$signals_m_df
  expect_equal(signals$id, c(
    "FIGI_A-2019-09-01", "FIGI_A-2019-10-01", "FIGI_A-2019-11-01",
    "FIGI_B-2019-09-01", "FIGI_B-2019-10-01", "FIGI_B-2019-11-01"
  ))
  expect_equal(signals$signal_age_months, c(0, 1, 2, 0, 1, 2))
  expect_equal(unique(signals$f1_breadth_change[signals$tickers == "FIGI_A"]), (27 - 30) / 100)
  expect_identical(names(signals)[1:3], c("id", "tickers", "dates"))
})

test_that("forward returns are monthly, in percentage points, NA in the unfinished month", {
  returns <- panels_fixture()$fwd_1m_returns_m_df
  expect_equal(returns$fwd_return_1m, c(2, -1, NA, 5, 3, NA))
  expect_equal(returns$id, panels_fixture()$signals_m_df$id)
})

test_that("a stock that stops trading earns 0 afterwards; stress hits its last month", {
  terminal <- tibble::tibble(
    instrument_id = c("FIGI_A", "FIGI_B"),
    last_date = as.Date(c("2019-09-20", "2019-11-15")), is_live = c(FALSE, TRUE)
  )
  returns <- panels_fixture(terminal)$fwd_1m_returns_m_df
  a <- dplyr::filter(returns, .data$tickers == "FIGI_A")
  expect_equal(a$fwd_return_1m, c(2, 0, NA))
  expect_equal(a$fwd_return_1m_stress[1], 100 * (1.02 * 0.7 - 1))
})

test_that("liquidity and volatility come from the month before each row", {
  panels <- panels_fixture()
  liquidity <- dplyr::filter(panels$liquidity_m_df, .data$tickers == "FIGI_A")
  ## September row: August features; October and November: September's (no later row)
  expect_equal(liquidity$close_prev, c(20, 21, 21))
  volatility <- dplyr::filter(panels$volatility_m_df, .data$tickers == "FIGI_A")
  ## A missing 21-day volatility falls back to the 63-day one
  expect_equal(volatility$daily_vol, c(1, 1.3, 1.3))
})

test_that("read_trading_calendar returns the SPY dates in order", {
  board <- setup_silver_board()

  calendar <- read_trading_calendar(board)

  expect_equal(calendar$date, silver_calendar)
  expect_named(attr(calendar, "inputs"), "bronze_tiingo_prices")
})

test_that("read_trading_calendar stops without bronze prices", {
  empty <- init_pipeline_board(withr::local_tempdir())
  expect_error(read_trading_calendar(empty), "deploy_bronze_tiingo")
})

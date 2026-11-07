tiingo_rows <- list(
  list(
    date = "2014-01-02T00:00:00.000Z", close = 553.13, high = 557.03, low = 552.02,
    open = 555.68, volume = 8398851, adjClose = 17.27, adjHigh = 17.39, adjLow = 17.23,
    adjOpen = 17.35, adjVolume = 268966400, divCash = 0, splitFactor = 1
  ),
  list(
    date = "2014-01-03T00:00:00.000Z", close = 540.98, high = 553.70, low = 540.43,
    open = 552.86, volume = 9058007, adjClose = 16.89, adjHigh = 17.29, adjLow = 16.88,
    adjOpen = 17.26, adjVolume = 289900000, divCash = 0, splitFactor = 1
  )
)

test_that("download_tiingo_prices returns typed daily rows with status 200", {
  httr2::local_mocked_responses(function(req) httr2::response_json(body = tiingo_rows))

  px <- download_tiingo_prices("AAPL", api_key = "k")

  expect_equal(px$date, as.Date(c("2014-01-02", "2014-01-03")))
  expect_equal(px$close, c(553.13, 540.98))
  expect_equal(px$adj_close, c(17.27, 16.89))
  expect_equal(unique(px$ticker), "AAPL")
  expect_equal(unique(px$status), "200")
})

test_that("download_tiingo_prices records an unknown ticker as a 404 row", {
  httr2::local_mocked_responses(function(req) httr2::response(status_code = 404))

  px <- download_tiingo_prices("1284849D", api_key = "k")

  expect_equal(nrow(px), 1L)
  expect_equal(px$status, "404")
  expect_true(is.na(px$date))
})

test_that("download_tiingo_prices records a ticker without prices as empty", {
  httr2::local_mocked_responses(function(req) httr2::response_json(body = list()))
  expect_equal(download_tiingo_prices("ZZZZ", api_key = "k")$status, "empty")
})

test_that("download_tiingo_prices stops on an authorisation error instead of recording it", {
  ## A bad key would otherwise turn every ticker into a recorded failure
  httr2::local_mocked_responses(function(req) httr2::response(status_code = 401))
  expect_error(download_tiingo_prices("AAPL", api_key = "bad"), "401")
})

# Board with a fake bronze_openfigi pin: two common stocks, one ETF, one dead code
setup_tiingo <- function(env = parent.frame()) {
  board <- init_pipeline_board(withr::local_tempdir(.local_envir = env))
  save_local_pin(board, tibble::tibble(
    cusip = c("037833100", "966837106", "464286731", "018490102"),
    status = "found", ticker = c("AAPL", "ZZZZ", "EWT", "1284849D"), exch_code = "US",
    security_type = c("Common Stock", "Common Stock", "ETP", "Common Stock")
  ), "bronze", "openfigi", verbose = FALSE)
  list(
    board = board, cache_dir = withr::local_tempdir(.local_envir = env),
    log_path = withr::local_tempdir(.local_envir = env)
  )
}

# Tiingo mock: ZZZZ is unknown (404), every other ticker gets two days of prices
mock_tiingo <- function(env = parent.frame()) {
  calls <- new.env()
  calls$n <- 0
  httr2::local_mocked_responses(function(req) {
    calls$n <- calls$n + 1
    if (grepl("/ZZZZ/", req$url)) {
      return(httr2::response(status_code = 404))
    }
    day <- function(date, close) {
      list(
        date = date, close = close, high = close, low = close, open = close, volume = 100,
        adjClose = close * 0.9, adjHigh = close * 0.9, adjLow = close * 0.9,
        adjOpen = close * 0.9, adjVolume = 100, divCash = 0, splitFactor = 1
      )
    }
    httr2::response_json(body = list(
      day("2014-01-02T00:00:00.000Z", 10), day("2014-01-03T00:00:00.000Z", 11)
    ))
  }, env = env)
  calls
}

deploy <- function(s, window_end = as.Date("2026-08-31"), ...) {
  suppressMessages(deploy_bronze_tiingo(s$board,
    window_end = window_end,
    cache_dir = s$cache_dir, api_key = "k", log_path = s$log_path,
    min_found_share = 0.5, min_spy_rows = 2, verbose = FALSE, ...
  ))
}

test_that("deploy_bronze_tiingo pins prices for common stocks and SPY with lineage", {
  s <- setup_tiingo()
  mock_tiingo()

  deploy(s)

  path <- pins::pin_download(s$board, "bronze_tiingo_prices")
  px <- nanoparquet::read_parquet(path)
  ## AAPL and SPY priced, ZZZZ recorded as 404; the ETF and the dead code not requested
  expect_setequal(unique(px$ticker), c("AAPL", "SPY", "ZZZZ"))
  expect_equal(px$status[px$ticker == "ZZZZ"], "404")
  meta <- pins::pin_meta(s$board, "bronze_tiingo_prices")$user
  expect_named(meta$inputs, "bronze_openfigi")
})

test_that("deploy_bronze_tiingo resumes from its cache without asking again", {
  s <- setup_tiingo()
  calls <- mock_tiingo()
  deploy(s)
  first_calls <- calls$n

  deploy(s)

  expect_equal(calls$n, first_calls)
})

test_that("deploy_bronze_tiingo stops and pins nothing when the suite fails", {
  ## SPY is required to cover the window: with a floor of 1,000 rows it does not
  s <- setup_tiingo()
  mock_tiingo()

  expect_error(
    suppressMessages(deploy_bronze_tiingo(s$board,
      window_end = as.Date("2026-08-31"),
      cache_dir = s$cache_dir, api_key = "k", log_path = s$log_path,
      min_found_share = 0.5, min_spy_rows = 1000, verbose = FALSE
    )),
    "Tests failed"
  )
  expect_false("bronze_tiingo_prices" %in% pins::pin_list(s$board))
})

test_that("deploy_bronze_tiingo records the window and stops on a later one", {
  ## A new SEC batch must not append new adjusted prices to old ones
  s <- setup_tiingo()
  calls <- mock_tiingo()
  deploy(s)
  expect_equal(
    pins::pin_meta(s$board, "bronze_tiingo_prices")$user$window_end, "2026-08-31"
  )
  first_calls <- calls$n

  expect_error(deploy(s, window_end = as.Date("2026-11-30")), "force = TRUE")
  expect_equal(calls$n, first_calls)
})

test_that("deploy_bronze_tiingo with force = TRUE redownloads every ticker", {
  s <- setup_tiingo()
  calls <- mock_tiingo()
  deploy(s)
  first_calls <- calls$n
  ## pins names versions by the second: identical content needs a new second
  Sys.sleep(1.1)

  deploy(s, window_end = as.Date("2026-11-30"), force = TRUE)

  ## Every ticker requested again, and the new window recorded
  expect_equal(calls$n, 2 * first_calls)
  expect_equal(
    pins::pin_meta(s$board, "bronze_tiingo_prices")$user$window_end, "2026-11-30"
  )
})

summary_fixture <- tibble::tibble(
  ticker = c("AAPL", "SPY", "WFM", "ZZZZ"),
  status = c("200", "200", "200", "404"),
  n_rows = c(3200, 3300, 900, 1),
  n_dates = c(3200, 3300, 900, 0),
  n_bad_close = c(0, 0, 0, 0),
  n_bad_adj = c(0, 0, 0, 0)
)
tickers_fixture <- tibble::tibble(ticker = c("AAPL", "SPY", "WFM", "ZZZZ"))

tiingo_suite_passes <- function(summary, min_spy_rows = 2500, max_bad_adj_share = 0.001) {
  suppressMessages(run_logged_test(
    function() {
      run_test_bronze_tiingo(summary, tickers_fixture,
        min_found_share = 0.5, min_spy_rows = min_spy_rows,
        max_bad_adj_share = max_bad_adj_share
      )
    },
    log_file = "test_bronze_tiingo", log_path = withr::local_tempdir(.local_envir = parent.frame())
  ))
}

test_that("the Tiingo suite passes on complete, clean prices", {
  expect_true(tiingo_suite_passes(summary_fixture))
})

test_that("the Tiingo suite fails on a requested ticker without a result", {
  expect_false(tiingo_suite_passes(summary_fixture[-3, ]))
})

test_that("the Tiingo suite fails on duplicated dates or a bad raw close", {
  dup <- dplyr::mutate(summary_fixture,
    n_dates = dplyr::if_else(.data$ticker == "AAPL", 3199, .data$n_dates)
  )
  expect_false(tiingo_suite_passes(dup))
  bad <- dplyr::mutate(summary_fixture,
    n_bad_close = dplyr::if_else(.data$ticker == "WFM", 1, .data$n_bad_close)
  )
  expect_false(tiingo_suite_passes(bad))
})

test_that("the Tiingo suite tolerates a few zero adjusted closes, not many", {
  ## 2026-11-07: 9,680 of 28 million rows (13 OTC tickers) had adj_close 0 with a
  ## positive close; bronze keeps them, silver drops them
  few <- dplyr::mutate(summary_fixture,
    n_bad_adj = dplyr::if_else(.data$ticker == "WFM", 5, .data$n_bad_adj)
  )
  expect_true(tiingo_suite_passes(few, max_bad_adj_share = 0.001))
  expect_false(tiingo_suite_passes(few, max_bad_adj_share = 0.0001))
})

test_that("the Tiingo suite fails when SPY does not cover the window", {
  expect_false(tiingo_suite_passes(summary_fixture, min_spy_rows = 5000))
})

## Figures of a clean run (shape of summarise_silver_prices_checks())
good_checks <- function() {
  tibble::tibble(
    n_prices = 1000, n_duplicate_ids = 0, n_nonpositive = 0, n_off_calendar = 0,
    n_tickers_multi = 0, n_returns = 990, n_liquidity = 1000, n_missing_flags = 0,
    share_big_liquid = 1e-4, share_total_return_agree = 0.999, aapl_split_ret = 0.0339,
    n_terminal = 10, n_instruments = 10,
    n_after_calendar = 0, share_match_liquid = 0.999
  )
}

prices_suite_passes <- function(checks) {
  suppressMessages(run_logged_test(
    function() run_test_silver_prices(build_silver_security_master(replies_fixture()), checks),
    log_file = "test_silver_prices", log_path = withr::local_tempdir(.local_envir = parent.frame())
  ))
}

test_that("the prices suite passes on clean figures", {
  expect_true(prices_suite_passes(good_checks()))
})

test_that("the suite fails when raw prices slip into returns", {
  ## AAPL on its 4-for-1 split day: -74% raw instead of +3.4% adjusted
  expect_false(prices_suite_passes(dplyr::mutate(good_checks(), aapl_split_ret = -0.7415)))
})

test_that("the suite fails when adjusted prices leave out dividends", {
  expect_false(prices_suite_passes(dplyr::mutate(good_checks(), share_total_return_agree = 0.03)))
})

test_that("the suite fails on duplicated ids or lost returns", {
  expect_false(prices_suite_passes(dplyr::mutate(good_checks(), n_duplicate_ids = 1)))
  expect_false(prices_suite_passes(dplyr::mutate(good_checks(), n_returns = 980)))
})

test_that("the suite fails when many liquid returns are extreme or prices disagree", {
  expect_false(prices_suite_passes(dplyr::mutate(good_checks(), share_big_liquid = 0.01)))
  expect_false(prices_suite_passes(dplyr::mutate(good_checks(), share_match_liquid = 0.9)))
})

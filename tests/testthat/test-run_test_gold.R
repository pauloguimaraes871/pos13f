## The whole gold suite on the real-data toy build, with a flat benchmark over its days
gold_suite_passes <- function(build, env = parent.frame()) {
  days <- sort(unique(build$toy$prices_d$date))
  bench_d <- tibble::tibble(date = days, bench_return = 0.01)
  bench_m <- bench_d |>
    dplyr::mutate(dates = lubridate::floor_date(.data$date, "month")) |>
    dplyr::summarise(
      bench_return_1m = 100 * (prod(1 + .data$bench_return / 100) - 1), .by = "dates"
    )
  toy <- build$toy
  suppressMessages(run_logged_test(
    function() {
      run_test_gold(build$panels, toy$cohort, bench_d, bench_m, toy$formations, toy$filings,
        toy$versions, toy$relations, build$reference,
        n_universe = 8, min_history_days = 300, max_membership_turnover = 1,
        max_reentry_share = 1
      )
    },
    log_file = "test_gold", log_path = withr::local_tempdir(.local_envir = env)
  ))
}

test_that("the gold suite passes on the real-data toy build", {
  expect_true(gold_suite_passes(toy_gold_build()))
})

test_that("the suite fails on unsorted ids or a missing or infinite liquidity value", {
  build <- toy_gold_build()
  broken <- build
  broken$panels$liquidity_m_df <- dplyr::arrange(
    broken$panels$liquidity_m_df, dplyr::desc(.data$id)
  )
  expect_false(gold_suite_passes(broken))
  for (bad in c(NA_real_, Inf)) {
    broken <- build
    broken$panels$liquidity_m_df <- dplyr::mutate(broken$panels$liquidity_m_df,
      presence = dplyr::if_else(dplyr::row_number() == 1, bad, .data$presence)
    )
    expect_false(gold_suite_passes(broken))
  }
})

test_that("the suite fails when a signal changes within its holding months", {
  build <- toy_gold_build()
  build$panels$signals_m_df <- dplyr::mutate(build$panels$signals_m_df,
    f1_breadth_change = dplyr::if_else(.data$signal_age_months == 1, 0.5, .data$f1_breadth_change)
  )
  expect_false(gold_suite_passes(build))
})

test_that("the suite fails when forward returns are missing before the last month", {
  build <- toy_gold_build()
  build$panels$fwd_1m_returns_m_df <- dplyr::mutate(build$panels$fwd_1m_returns_m_df,
    fwd_return_1m = dplyr::if_else(dplyr::row_number() == 1, NA_real_, .data$fwd_return_1m)
  )
  expect_false(gold_suite_passes(build))
})

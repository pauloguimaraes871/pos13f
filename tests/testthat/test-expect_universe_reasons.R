## Toy universe: the real-data toy run through the gold builders
toy_universe <- function() {
  toy <- toy_silver_gold()
  con <- toy_gold_con(toy)
  breadth <- toy_gold_sql(con, sql_gold_breadth(
    "silver_holdings_state", "formations", "cohort", "master", "silver_prices_d"
  ))
  features <- toy_gold_sql(con, sql_gold_monthly_features("silver_prices_d", "silver_returns_d"))
  ## The toy prices start in June 2018; ask for 300 days of history so screens bind
  build_gold_universe(toy$formations, breadth, features, toy$terminal, toy$master,
    n_universe = 8, min_history_days = 300
  )
}

test_that("independent reasons agree with build_gold_universe on the real-data toy", {
  universe <- toy_universe()
  expected <- expect_universe_reasons(universe, n_universe = 8, min_history_days = 300) |>
    dplyr::arrange(.data$formation_date, .data$instrument_id)
  expect_identical(expected$expected_reason, universe$excluded_reason)
  expect_equal(as.numeric(expected$expected_rank), as.numeric(universe$liquidity_rank))
  ## The toy exercises the price, identity and rank screens; with 400 sampled filers no
  ## stock reaches the 20 holders of D9, which the hand-made case below covers
  screens <- c("below_liquidity_rank", "price_below_min", "identity_unverifiable")
  expect_true(all(screens %in% universe$excluded_reason))
})

test_that("the first failing screen wins, and missing values fail no screen", {
  row <- tibble::tibble(
    formation_date = as.Date("2020-02-28"), instrument_id = c("A", "B", "C"),
    ticker = c("AAA", "BBB", "CCC"), n_prev = 30, n_curr = 30, n_init = c(25, 0, 0),
    n_match = c(5, 30, 30), date = as.Date("2020-02-28"), close_prev = c(2, NA, 50),
    first_date = as.Date("2010-01-04"), median_volfin_3m = 1e7, mom_12_1 = 1, rev_1m = 1,
    vol_3m = 1, amihud = 1
  )
  out <- expect_universe_reasons(row, n_universe = 10)
  ## A fails identity, D9 and price: identity comes first; B's missing close is caught as
  ## a missing feature, not as a low price; C passes
  expect_equal(out$expected_reason, c("identity_rejected", "missing_features", NA))
  expect_equal(out$expected_rank, c(NA, NA, 1L))
})

## One instrument on 300 consecutive trading days with known values: dollar volume
## rises by 1 per day, every return is 1%, and day 290 is missing (no trade)
features_daily <- function() {
  days <- seq_len(300)
  dates <- as.Date("2019-01-01") + days
  prices <- tibble::tibble(
    id = paste0("FIGI_A-", dates), instrument_id = "FIGI_A", date = dates, day_index = days,
    close = 10 * 1.01^days, dollar_volume = as.numeric(days)
  ) |>
    dplyr::filter(.data$day_index != 290)
  list(prices = prices, returns = dplyr::transmute(prices, .data$id, ret = 0.01))
}

test_that("features use only trading days before the anchor, in the stated windows", {
  d <- features_daily()
  ## The anchor is the last price day before as_of: day 300
  out <- reference_gold_features(
    tibble::tibble(instrument_id = "FIGI_A", as_of = as.Date("2019-01-01") + 301),
    d$prices, d$returns
  )
  expect_equal(out$anchor_date, as.Date("2019-01-01") + 300)
  expect_equal(out$close_prev, 10 * 1.01^299)
  ## Days 237 to 299 minus the missing day 290: 62 trading days of 63
  window <- setdiff(237:299, 290)
  expect_equal(out$mean_volfin_3m, mean(window))
  expect_equal(out$median_volfin_3m, stats::median(window))
  expect_equal(out$presence, 100 * 62 / 63)
  ## Constant returns: no volatility; compounding counts only days with a return
  expect_equal(out$vol_3m, 0)
  expect_equal(out$rev_1m, 100 * (1.01^20 - 1))
  expect_equal(out$n_mom, 231)
  expect_equal(out$mom_12_1, 100 * (1.01^231 - 1))
  expect_equal(out$amihud, 1e6 * mean(0.01 / window))
})

test_that("a target before any price has no features, and duplicates collapse", {
  d <- features_daily()
  targets <- tibble::tibble(instrument_id = "FIGI_A", as_of = as.Date(rep("2018-06-01", 2)))
  out <- reference_gold_features(targets, d$prices, d$returns)
  expect_equal(nrow(out), 1)
  expect_true(is.na(out$anchor_date))
  expect_true(is.na(out$mean_volfin_3m))
})

test_that("the dplyr reference reproduces sql_gold_monthly_features on the real-data toy", {
  toy <- toy_silver_gold()
  con <- toy_gold_con(toy)
  sql <- toy_gold_sql(con, sql_gold_monthly_features("silver_prices_d", "silver_returns_d")) |>
    ## The SQL row of month M is the reference as of the first day of month M + 1
    dplyr::mutate(as_of = add_months(lubridate::floor_date(.data$date, "month"), 1)) |>
    dplyr::filter(.data$date >= as.Date("2019-07-01")) |>
    dplyr::arrange(.data$instrument_id, .data$as_of)
  targets <- dplyr::select(sql, "instrument_id", "as_of")
  ref <- reference_gold_features(targets, toy$prices_d, toy$returns_d) |>
    dplyr::arrange(.data$instrument_id, .data$as_of)
  expect_gt(nrow(ref), 300)
  expect_equal(ref$anchor_date, sql$date)
  cols <- c(
    "close_prev", "mean_volfin_3m", "median_volfin_3m", "presence", "daily_vol", "vol_3m",
    "mom_12_1", "n_mom", "rev_1m", "amihud"
  )
  for (col in cols) {
    expect_equal(as.numeric(ref[[col]]), as.numeric(sql[[col]]), tolerance = 1e-8, label = col)
  }
})

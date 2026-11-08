features_con_fixture <- function(env = parent.frame()) {
  con <- prices_con_fixture(env)
  DBI::dbExecute(con, paste("CREATE TABLE returns AS", sql_silver_returns_d("prices")))
  con
}

monthly_features <- function(con) {
  tibble::as_tibble(DBI::dbGetQuery(con, paste(
    sql_gold_monthly_features("prices", "returns"), "ORDER BY instrument_id, month"
  )))
}

test_that("monthly features come from each month's last trading day", {
  features <- monthly_features(features_con_fixture())
  aaa <- dplyr::filter(features, .data$instrument_id == "FIGI_AAA")
  expect_equal(aaa$date, as.Date(c("2020-06-30", "2020-07-31", "2020-08-31", "2020-09-30")))
  expect_equal(aaa$mean_volfin_3m[4], 1e7)
  expect_equal(aaa$presence[4], 100)
})

test_that("a feature dated t ignores the price, volume and return of t", {
  con <- features_con_fixture()
  before <- dplyr::filter(monthly_features(con), .data$instrument_id == "FIGI_AAA")
  DBI::dbExecute(con, paste(
    "UPDATE prices SET close = 1, dollar_volume = 1 WHERE instrument_id = 'FIGI_AAA'",
    "AND date = DATE '2020-09-30'"
  ))
  DBI::dbExecute(con, "UPDATE returns SET ret = -0.9 WHERE id = 'FIGI_AAA_2020-09-30'")
  after <- dplyr::filter(monthly_features(con), .data$instrument_id == "FIGI_AAA")
  cols <- c(
    "close_prev", "mean_volfin_3m", "median_volfin_3m", "daily_vol", "vol_3m", "rev_1m",
    "amihud"
  )
  expect_equal(after[4, cols], before[4, cols])
})

test_that("Amihud skips days without volume instead of turning NaN or infinite", {
  con <- features_con_fixture()
  before <- dplyr::filter(monthly_features(con), .data$instrument_id == "FIGI_AAA")
  ## A zero-volume day inside the window of the September row (0/0 or r/0 before the fix)
  updated <- DBI::dbExecute(con, paste(
    "UPDATE prices SET dollar_volume = 0 WHERE instrument_id = 'FIGI_AAA'",
    "AND date = DATE '2020-09-15'"
  ))
  expect_equal(updated, 1)
  after <- dplyr::filter(monthly_features(con), .data$instrument_id == "FIGI_AAA")
  expect_true(all(is.finite(after$amihud[3:4])))
  expect_equal(after$amihud[1:3], before$amihud[1:3])
})

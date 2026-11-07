test_that("implied prices are counted as matches, thousand errors and misses", {
  con <- prices_con_fixture()
  ## Four filers of AAA at 2020-06-30 (close 10): two agree, one reports VALUE a
  ## thousand times too large, one disagrees; an old version is ignored
  state <- tibble::tibble(
    cik = c("1", "2", "3", "4", "1"),
    period_of_report = as.Date("2020-06-30"),
    cusip = "000000AA1",
    valid_from = as.Date(c("2020-08-03", "2020-08-04", "2020-08-05", "2020-08-06", "2020-07-01")),
    valid_to = as.Date(c(NA, NA, NA, NA, "2020-08-03")),
    shares = 100,
    value_usd = c(1000, 1010, 1e6, 3000, 99)
  )
  duckdb::duckdb_register(con, "state", state)
  checks <- query_fixture(con, sql_silver_identity_checks("state", "master", "prices"))

  expect_equal(nrow(checks), 1)
  expect_equal(checks$close, 10)
  expect_equal(checks$n_filers, 4)
  expect_equal(checks$n_match, 2)
  expect_equal(checks$n_thousand, 1)
})

test_that("a quarter whose latest price is stale is not compared", {
  con <- prices_con_fixture()
  DBI::dbExecute(con, paste(
    "DELETE FROM prices WHERE date > DATE '2020-06-15' AND date <= DATE '2020-06-30'"
  ))
  state <- tibble::tibble(
    cik = "1", period_of_report = as.Date("2020-06-30"), cusip = "000000AA1",
    valid_from = as.Date("2020-08-03"), valid_to = as.Date(NA), shares = 100, value_usd = 1000
  )
  duckdb::duckdb_register(con, "state", state)
  expect_equal(nrow(query_fixture(con, sql_silver_identity_checks("state", "master", "prices"))), 0)
})

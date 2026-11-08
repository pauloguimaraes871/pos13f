test_that("monthly returns compound the daily returns of each calendar month", {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  withr::defer(DBI::dbDisconnect(con))
  daily <- tibble::tibble(
    instrument_id = "FIGI_A",
    date = as.Date(c("2020-06-29", "2020-06-30", "2020-07-01")),
    ret = c(0.1, -0.1, 0.05)
  )
  duckdb::duckdb_register(con, "returns", daily)
  monthly <- tibble::as_tibble(DBI::dbGetQuery(con, paste(
    sql_gold_monthly_returns("returns"), "ORDER BY month"
  )))

  expect_equal(monthly$ret_m, c(1.1 * 0.9 - 1, 0.05), tolerance = 1e-12)
  expect_equal(monthly$n_days, c(2, 1))
  expect_equal(monthly$last_date, as.Date(c("2020-06-30", "2020-07-01")))
})

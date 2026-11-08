test_that("the benchmark is SPY's daily total return in percentage points", {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  withr::defer(DBI::dbDisconnect(con))
  bronze <- tibble::tibble(
    ticker = c("SPY", "SPY", "SPY", "AAA"), status = "200",
    date = as.Date(c("2020-06-30", "2020-06-29", "2020-07-01", "2020-06-30")),
    adj_close = c(102, 100, 101, 5)
  )
  duckdb::duckdb_register(con, "bronze", bronze)
  bench <- tibble::as_tibble(DBI::dbGetQuery(con, sql_gold_benchmark_d("bronze")))

  expect_equal(bench$date, as.Date(c("2020-06-30", "2020-07-01")))
  expect_equal(bench$bench_return, c(2, 100 * (101 / 102 - 1)))
})

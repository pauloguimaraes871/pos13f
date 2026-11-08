## Holdings state rows around the 2019-08-30 formation
state_fixture <- function() {
  tibble::tibble(
    cik = c("1", "1", "1", "1", "2", "2"),
    period_of_report = as.Date(rep(c("2019-03-31", "2019-06-30"), c(2, 4))),
    cusip = c("AAA", "BBB", "AAA", "CCC", "AAA", "AAA"),
    ## Filer 2's quarter-q position was restated after t: the old version is current at t
    valid_from = as.Date(rep(c("2019-05-10", "2019-08-10", "2019-09-10"), c(2, 3, 1))),
    valid_to = as.Date(c(NA, NA, NA, NA, "2019-09-10", NA)),
    shares = c(100, 50, 120, 10, 70, 80),
    value_usd = c(1000, 500, 1200, 100, 700, 800)
  )
}

test_that("activity counts positions per quarter from versions current at t", {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  withr::defer(DBI::dbDisconnect(con))
  duckdb::duckdb_register(con, "state", state_fixture())
  duckdb::duckdb_register(con, "formations", gold_formations())
  activity <- tibble::as_tibble(DBI::dbGetQuery(con, paste(
    sql_gold_filer_activity("state", "formations"), "ORDER BY cik"
  )))

  expect_equal(activity$cik, c("1", "2"))
  expect_equal(activity$n_prev, c(2, 0))
  expect_equal(activity$n_curr, c(2, 1))
  expect_equal(activity$n_both, c(1, 0))
  ## Filer 2's value is the version current at t (700), not the later restatement
  expect_equal(activity$value_curr, c(1300, 700))
})

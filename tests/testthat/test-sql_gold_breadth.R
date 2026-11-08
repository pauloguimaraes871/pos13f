test_that("breadth counts cohort holders per instrument, once per filer", {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  withr::defer(DBI::dbDisconnect(con))
  ## Two CUSIPs of one instrument (AAA, AA2); filer 3 is not in the cohort
  state <- tibble::tibble(
    cik = c("1", "1", "2", "2", "3"),
    period_of_report = as.Date(c("2019-03-31", rep("2019-06-30", 4))),
    cusip = c("AAA", "AAA", "AAA", "AA2", "AAA"),
    valid_from = as.Date("2019-08-10"), valid_to = as.Date(NA),
    shares = c(100, 100, 50, 50, 10), value_usd = c(1000, 1000, 500, 5e5, 100)
  )
  cohort <- tibble::tibble(
    formation_date = as.Date("2019-08-30"), cik = c("1", "2", "3"),
    in_cohort = c(TRUE, TRUE, FALSE)
  )
  master <- tibble::tibble(cusip = c("AAA", "AA2"), instrument_id = "FIGI_A")
  prices <- tibble::tibble(instrument_id = "FIGI_A", date = as.Date("2019-06-28"), close = 10)
  for (x in list(
    list("state", state), list("formations", gold_formations()),
    list("cohort", cohort), list("master", master), list("prices", prices)
  )) {
    duckdb::duckdb_register(con, x[[1]], x[[2]])
  }
  breadth <- tibble::as_tibble(DBI::dbGetQuery(con, sql_gold_breadth(
    "state", "formations", "cohort", "master", "prices"
  )))

  expect_equal(breadth$n_prev, 1)
  expect_equal(breadth$n_curr, 2)
  expect_equal(breadth$n_init, 1)
  expect_equal(breadth$n_exit, 0)
  ## Filer 2's summed value per share (505,000 / 100) disagrees with the close of 10
  expect_equal(breadth$n_match, 1)
})

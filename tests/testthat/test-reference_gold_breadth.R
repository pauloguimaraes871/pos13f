## Two filers around the 2019-08-30 formation (q-1 = 2019-03-31, q = 2019-06-30)
breadth_state <- function() {
  tibble::tibble(
    cik = c("1", "1", "1", "2", "2", "2", "3"),
    cusip = c("C1", "C1", "C2", "C1", "C1", "C3", "C1"),
    period_of_report = as.Date(c(
      "2019-03-31", "2019-06-30", "2019-06-30", "2019-03-31", "2019-06-30", "2019-06-30",
      "2019-06-30"
    )),
    ## Filer 2's quarter-q position in C1 appears only in an amendment filed after t
    valid_from = as.Date(c(
      "2019-05-10", "2019-08-10", "2019-08-10", "2019-05-10", "2019-09-10", "2019-08-10",
      "2019-08-10"
    )),
    valid_to = as.Date(NA),
    shares = c(10, 10, 5, 20, 20, 0, 7),
    value_usd = c(1000, 1000, 500, 2000, 2000, 0, 700)
  )
}
breadth_cohort <- function() {
  tibble::tibble(
    formation_date = as.Date("2019-08-30"), cik = c("1", "2", "3"),
    in_cohort = c(TRUE, TRUE, FALSE)
  )
}
## C1 and C2 are two CUSIPs of one instrument (a share-class change); C3 another
breadth_master <- function() {
  tibble::tibble(cusip = c("C1", "C2", "C3"), instrument_id = c("FIGI_A", "FIGI_A", "FIGI_B"))
}
breadth_prices <- function() {
  tibble::tibble(instrument_id = "FIGI_A", date = as.Date("2019-06-28"), close = 100)
}

test_that("breadth counts cohort holders from versions current at t", {
  out <- reference_gold_breadth(
    breadth_state(), gold_formations(), breadth_cohort(),
    breadth_master(), breadth_prices()
  )
  ## Filer 2 exits A at t (its quarter-q position is only in a later amendment); filer 1
  ## holds two CUSIPs of A and counts once; filer 3 is not in the cohort; zero shares of
  ## C3 are not a holding, so B never appears
  expect_equal(out$instrument_id, "FIGI_A")
  expect_equal(c(out$n_prev, out$n_curr, out$n_init, out$n_exit), c(2, 1, 0, 1))
  ## Filer 1's value per share (1500 / 15 = 100) matches the close
  expect_equal(out$n_match, 1)
})

test_that("a stale or missing quarter-end close never counts as an identity match", {
  stale <- dplyr::mutate(breadth_prices(), date = as.Date("2019-06-10"))
  out <- reference_gold_breadth(
    breadth_state(), gold_formations(), breadth_cohort(),
    breadth_master(), stale
  )
  expect_equal(out$n_match, 0)
})

test_that("the dplyr reference reproduces sql_gold_breadth on the real-data toy", {
  toy <- toy_silver_gold()
  con <- toy_gold_con(toy)
  sql <- toy_gold_sql(con, sql_gold_breadth(
    "silver_holdings_state", "formations", "cohort", "master", "silver_prices_d"
  )) |>
    dplyr::arrange(.data$formation_date, .data$instrument_id)
  ref <- reference_gold_breadth(toy$state, toy$formations, toy$cohort, toy$master, toy$prices_d)
  expect_gt(nrow(ref), 30)
  expect_equal(ref$instrument_id, sql$instrument_id)
  for (col in c("n_prev", "n_curr", "n_init", "n_exit", "n_match")) {
    expect_equal(as.numeric(ref[[col]]), as.numeric(sql[[col]]), label = col)
  }
})

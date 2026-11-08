test_that("activity counts distinct CUSIPs per quarter from versions current at t", {
  ## Reuses the state of the breadth reference test: filer 2's q position in C1 is only
  ## in an amendment filed after t, and its C3 position has zero shares
  state <- tibble::tibble(
    cik = c("1", "1", "1", "2", "2", "2"),
    cusip = c("C1", "C1", "C2", "C1", "C1", "C3"),
    period_of_report = as.Date(c(
      "2019-03-31", "2019-06-30", "2019-06-30", "2019-03-31", "2019-06-30", "2019-06-30"
    )),
    valid_from = as.Date(c(
      "2019-05-10", "2019-08-10", "2019-08-10", "2019-05-10", "2019-09-10", "2019-08-10"
    )),
    valid_to = as.Date(NA),
    shares = c(10, 10, 5, 20, 20, 0),
    value_usd = c(1000, 1000, 500, 2000, 2000, 0)
  )
  targets <- dplyr::mutate(
    dplyr::select(gold_formations(), "formation_date", "report_quarter", "prev_quarter"),
    cik = list(c("1", "2", "9"))
  ) |>
    tidyr::unnest("cik")
  out <- reference_gold_activity(state, targets)
  expect_equal(out$cik, c("1", "2", "9"))
  expect_equal(out$n_prev, c(1, 1, 0))
  expect_equal(out$n_curr, c(2, 0, 0))
  expect_equal(out$n_both, c(1, 0, 0))
  expect_equal(out$value_curr, c(1500, 0, 0))
})

test_that("the reference reproduces sql_gold_filer_activity on the real-data toy filers", {
  toy <- toy_silver_gold()
  con <- toy_gold_con(toy)
  ## The toy state holds only the toy instruments' CUSIPs, so both sides read that subset
  sql <- toy_gold_sql(con, sql_gold_filer_activity("silver_holdings_state", "formations")) |>
    dplyr::arrange(.data$formation_date, .data$cik)
  targets <- dplyr::inner_join(dplyr::distinct(sql, .data$formation_date, .data$cik),
    toy$formations,
    by = dplyr::join_by("formation_date"), relationship = "many-to-one"
  )
  ref <- reference_gold_activity(toy$state, targets) |>
    dplyr::arrange(.data$formation_date, .data$cik)
  expect_gt(nrow(ref), 100)
  for (col in c("n_prev", "n_curr", "n_both", "value_curr")) {
    expect_equal(ref[[col]], as.numeric(sql[[col]]), label = col)
  }
})

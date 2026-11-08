test_that("only filers with both reports public at t are candidates", {
  cohort <- gold_cohort_fixture()
  ## Filer 2's q report became usable on 2019-09-05, after the 2019-08-30 formation
  expect_equal(cohort$cik, sprintf("%010d", c(1, 3, 4, 5, 6)))
})

test_that("few positions, index-like and overlap filers leave the cohort", {
  cohort <- gold_cohort_fixture()
  ## Terciles among filers with enough positions: positions >= 12 and turnover <= 1/3
  ## make filer 1 index-like; filer 5 is in filer 4's report and smaller
  expect_equal(cohort$excluded_reason, c("index_like", "few_positions", NA, "overlap", NA))
  expect_equal(cohort$in_cohort, c(FALSE, FALSE, TRUE, FALSE, TRUE))
  expect_equal(cohort$turnover[cohort$cik == "0000000004"], (12 + 12 - 8) / (12 + 12 - 4))
})

test_that("a filer with empty reports is a candidate with zero positions", {
  ## Filer 2 files its q report before t but reports no position: no activity row
  versions <- dplyr::mutate(gold_versions(), valid_from = dplyr::if_else(
    .data$cik == "0000000002", as.Date("2019-08-10"), .data$valid_from
  ))
  cohort <- select_gold_cohort(gold_formations(), versions, gold_activity(),
    relations = gold_relations()
  )
  two <- dplyr::filter(cohort, .data$cik == "0000000002")
  expect_equal(two$n_curr, 0)
  expect_equal(two$excluded_reason, "few_positions")
})

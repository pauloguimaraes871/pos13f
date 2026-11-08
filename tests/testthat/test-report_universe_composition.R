test_that("sizes and reasons are counted per formation", {
  universe <- tibble::tibble(
    formation_date = as.Date(rep(c("2019-08-30", "2019-11-29"), c(3, 2))),
    in_universe = c(TRUE, FALSE, FALSE, TRUE, TRUE),
    excluded_reason = c(NA, "price_below_min", "low_presence", NA, NA)
  )
  cohort <- tibble::tibble(
    formation_date = as.Date(rep(c("2019-08-30", "2019-11-29"), c(2, 3))),
    in_cohort = c(TRUE, FALSE, TRUE, TRUE, TRUE)
  )
  out <- report_universe_composition(universe, cohort)
  expect_equal(out$sizes$n_candidates, c(3, 2))
  expect_equal(out$sizes$n_members, c(1, 2))
  expect_equal(out$sizes$cohort_size, c(1, 3))
  expect_setequal(out$reasons$reason, c("member", "price_below_min", "low_presence"))
  expect_equal(sum(out$reasons$n), 5)
})

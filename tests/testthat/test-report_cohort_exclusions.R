## Two formations: a large index-like filer, an overlap and two members; the second
## formation has no excluded filer at all
exclusions_fixture <- function() {
  tibble::tibble(
    formation_date = as.Date(c(rep("2020-02-28", 4), rep("2020-05-29", 2))),
    cik = c("0000102909", "0000093751", "0001364742", "0001067983", "0001364742", "0001067983"),
    value_curr = c(600, 100, 200, 100, 300, 100),
    excluded_reason = c("index_like", "overlap", NA, NA, NA, NA)
  )
}

test_that("counts and shares are averaged over formations", {
  out <- report_cohort_exclusions(exclusions_fixture())
  member <- out[out$outcome == "member", ]
  index <- out[out$outcome == "index_like", ]
  expect_equal(member$mean_filers, 2)
  expect_equal(member$share_filers, mean(c(2 / 4, 1)))
  expect_equal(member$share_value, mean(c(300 / 1000, 1)))
  ## A reason absent from a formation counts as zero there
  expect_equal(index$share_value, 0.6 / 2)
  expect_equal(index$mean_filers, 0.5)
})

test_that("a missing column is refused", {
  expect_error(report_cohort_exclusions(exclusions_fixture()[, -4]), "excluded_reason")
})

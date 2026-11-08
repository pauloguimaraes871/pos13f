test_that("add_months shifts dates and rolls back to existing days", {
  shifted <- add_months(as.Date(c("2020-09-01", "2020-03-31")), 3)
  expect_equal(shifted, as.Date(c("2020-12-01", "2020-06-30")))
  expect_equal(add_months(as.Date("2020-01-01"), -1), as.Date("2019-12-01"))
})

test_that("add_months rejects invalid arguments", {
  expect_error(add_months("2020-01-01", 1), "Date")
  expect_error(add_months(as.Date("2020-01-01"), 1.5), "whole")
})

test_that("the label is the holding month minus the signal's age, across years", {
  dates <- as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-03-01"))
  expect_equal(
    formation_label_of(dates, c(1, 2, 0, 2)),
    as.Date(c("2019-12-01", "2019-12-01", "2020-03-01", "2020-01-01"))
  )
})

test_that("dates that are not first days, or fractional ages, are refused", {
  expect_error(formation_label_of(as.Date("2020-01-15"), 0), "first days of months")
  expect_error(formation_label_of(as.Date("2020-01-01"), 0.5), "whole numbers")
})

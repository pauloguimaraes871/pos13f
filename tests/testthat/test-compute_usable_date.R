calendar <- as.Date(c("2022-12-29", "2022-12-30", "2023-01-03", "2023-01-04"))

test_that("a filing is usable from the next trading day, never the same day", {
  ## Thursday filing: Friday. Friday filing: Tuesday, over a weekend and a holiday
  filed <- as.Date(c("2022-12-29", "2022-12-30"))
  expect_equal(compute_usable_date(filed, calendar), as.Date(c("2022-12-30", "2023-01-03")))
})

test_that("a filing on a non-trading day is usable from the next trading day", {
  expect_equal(compute_usable_date(as.Date("2023-01-01"), calendar), as.Date("2023-01-03"))
})

test_that("a missing date or a filing past the calendar gives NA", {
  filed <- as.Date(c(NA, "2023-01-04", "2023-02-01"))
  expect_equal(compute_usable_date(filed, calendar), as.Date(c(NA, NA, NA)))
})

test_that("compute_usable_date rejects an invalid calendar", {
  filed <- as.Date("2022-12-29")
  expect_error(compute_usable_date("2022-12-29", calendar), "filing_date")
  expect_error(compute_usable_date(filed, rev(calendar)), "calendar")
  expect_error(compute_usable_date(filed, c(calendar, calendar[4])), "calendar")
  expect_error(compute_usable_date(filed, as.Date(character(0))), "calendar")
})

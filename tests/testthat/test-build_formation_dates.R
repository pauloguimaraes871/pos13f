test_that("formations are the last trading days of Feb, May, Aug and Nov", {
  formations <- build_formation_dates(gold_calendar(), first_quarter = as.Date("2019-03-31"))

  ## May 2019 would need q-1 = 2018Q4, before the first quarter allowed
  expect_equal(formations$formation_date, as.Date(c("2019-08-30", "2019-11-29")))
  expect_equal(formations$report_quarter, as.Date(c("2019-06-30", "2019-09-30")))
  expect_equal(formations$prev_quarter, as.Date(c("2019-03-31", "2019-06-30")))
  expect_equal(formations$label_date, as.Date(c("2019-09-01", "2019-12-01")))
})

test_that("an unfinished last month gives no formation", {
  ## The calendar stops on 2020-02-14: end-February 2020 is not known yet
  calendar <- gold_calendar()
  calendar <- calendar[calendar <= as.Date("2020-02-14")]
  formations <- build_formation_dates(calendar, first_quarter = as.Date("2019-03-31"))
  expect_false(any(formations$formation_date >= as.Date("2020-02-01")))
})

test_that("build_formation_dates rejects an invalid calendar", {
  expect_error(build_formation_dates("2019-01-02"), "calendar")
})

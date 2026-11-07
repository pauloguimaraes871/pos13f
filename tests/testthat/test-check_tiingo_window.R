test_that("the same or a shorter window reuses the cache", {
  expect_equal(check_tiingo_window(as.Date("2026-08-31"), as.Date("2026-08-31")), "reuse")
  expect_equal(check_tiingo_window(as.Date("2026-05-31"), as.Date("2026-08-31")), "reuse")
})

test_that("without a recorded window the cache is reused", {
  ## First run, or the first pin of 2026-11-07, which predates the record
  expect_equal(check_tiingo_window(as.Date("2026-08-31"), NULL), "reuse")
  expect_equal(check_tiingo_window(as.Date("2026-08-31"), as.Date(NA)), "reuse")
})

test_that("a later window stops unless force = TRUE", {
  ## The next SEC batch (Sep to Nov 2026) would mix two vintages of adjusted prices
  expect_error(
    check_tiingo_window(as.Date("2026-11-30"), as.Date("2026-08-31")),
    "force = TRUE"
  )
  expect_equal(
    check_tiingo_window(as.Date("2026-11-30"), as.Date("2026-08-31"), force = TRUE),
    "refresh"
  )
})

test_that("force = TRUE refreshes even for an unchanged window", {
  expect_equal(
    check_tiingo_window(as.Date("2026-08-31"), as.Date("2026-08-31"), force = TRUE),
    "refresh"
  )
})

test_that("check_tiingo_window rejects invalid arguments", {
  expect_error(check_tiingo_window("2026-08-31", NULL), "window_end")
  expect_error(check_tiingo_window(as.Date(NA), NULL), "window_end")
  expect_error(check_tiingo_window(as.Date("2026-08-31"), NULL, force = NA), "force")
})

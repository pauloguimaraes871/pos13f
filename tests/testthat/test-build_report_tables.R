test_that("a non-board argument is refused", {
  expect_error(build_report_tables(list()), "must be a pins board")
})

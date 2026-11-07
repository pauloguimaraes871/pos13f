test_that("read_window_end returns the recorded date, or NA without a record", {
  path <- file.path(withr::local_tempdir(), "window_end.txt")
  expect_equal(read_window_end(path), as.Date(NA))

  writeLines("2026-08-31", path)

  expect_equal(read_window_end(path), as.Date("2026-08-31"))
})

test_that("read_window_end stops on a damaged record", {
  ## Read as "no record", a damaged file would let a stale cache pass the window check
  path <- file.path(withr::local_tempdir(), "window_end.txt")
  writeLines("31/08/2026", path)
  expect_error(read_window_end(path), "YYYY-MM-DD")
  writeLines(c("2026-08-31", "2026-11-30"), path)
  expect_error(read_window_end(path), "one date")
})

test_that("read_window_end rejects an invalid path", {
  expect_error(read_window_end(c("a", "b")), "path")
})

test_that("write_window_end creates the folder and overwrites the record", {
  path <- file.path(withr::local_tempdir(), "manifest", "window_end.txt")

  write_window_end(as.Date("2026-08-31"), path)
  expect_equal(readLines(path), "2026-08-31")

  write_window_end(as.Date("2026-11-30"), path)
  expect_equal(read_window_end(path), as.Date("2026-11-30"))
})

test_that("write_window_end rejects invalid arguments", {
  path <- file.path(withr::local_tempdir(), "window_end.txt")
  expect_error(write_window_end("2026-08-31", path), "window_end")
  expect_error(write_window_end(as.Date(NA), path), "window_end")
  expect_error(write_window_end(as.Date("2026-08-31"), NA_character_), "path")
})

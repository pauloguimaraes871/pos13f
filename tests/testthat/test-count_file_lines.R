test_that("count_file_lines counts newline-terminated lines", {
  path <- withr::local_tempfile(fileext = ".tsv")
  writeLines(c("A\tB", "1\t2", "3\t4"), path)

  expect_equal(count_file_lines(path), 3)
})

test_that("count_file_lines counts a last line without a trailing newline", {
  path <- withr::local_tempfile(fileext = ".tsv")
  cat("A\tB\n1\t2", file = path)

  expect_equal(count_file_lines(path), 2)
})

test_that("count_file_lines returns 0 for an empty file and errors on a missing one", {
  path <- withr::local_tempfile(fileext = ".tsv")
  file.create(path)
  expect_equal(count_file_lines(path), 0)
  expect_error(count_file_lines(file.path(tempdir(), "nope.tsv")), "path")
})

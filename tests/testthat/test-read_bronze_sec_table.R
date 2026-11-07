test_that("read_bronze_sec_table stacks one table of the archives asked for", {
  board <- setup_silver_board()

  submission <- read_bronze_sec_table(board, "SUBMISSION", labels = c("2022q4", "2023q1"))

  expect_equal(nrow(submission), 4)
  expect_setequal(submission$source_archive, c("2022q4", "2023q1"))
  expect_named(attr(submission, "inputs"), c("bronze_sec_2022q4", "bronze_sec_2023q1"))
})

test_that("read_bronze_sec_table ignores pins outside the sample window", {
  ## A shortened window leaves older pins on the board; they must not enter silver
  board <- setup_silver_board()

  submission <- read_bronze_sec_table(board, "SUBMISSION", labels = "2023q1")

  expect_equal(unique(submission$source_archive), "2023q1")
  expect_named(attr(submission, "inputs"), "bronze_sec_2023q1")
})

test_that("read_bronze_sec_table stops on an archive of the window without a pin", {
  board <- setup_silver_board()
  expect_error(
    read_bronze_sec_table(board, "SUBMISSION", labels = c("2022q3", "2022q4")),
    "bronze_sec_2022q3"
  )
})

test_that("read_bronze_sec_table refuses INFOTABLE and invalid labels", {
  board <- setup_silver_board()
  expect_error(read_bronze_sec_table(board, "INFOTABLE", labels = "2022q4"), "table must be")
  expect_error(read_bronze_sec_table(board, "SUBMISSION", labels = character(0)), "labels")
  expect_error(read_bronze_sec_table(board, "SUBMISSION", labels = c("2022q4", "2022q4")), "labels")
})

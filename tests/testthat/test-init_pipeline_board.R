test_that("init_pipeline_board creates a versioned folder board at the given path", {
  path <- file.path(withr::local_tempdir(), "data", "prod")

  board <- init_pipeline_board(path)

  expect_s3_class(board, "pins_board_folder")
  expect_true(board$versioned)
  expect_true(dir.exists(path))
})

test_that("init_pipeline_board reuses an existing board without losing pins", {
  path <- withr::local_tempdir()
  board <- init_pipeline_board(path)
  pins::pin_write(board, data.frame(x = 1), "bronze_example", type = "rds")

  board_again <- init_pipeline_board(path)

  expect_true("bronze_example" %in% pins::pin_list(board_again))
})

test_that("init_pipeline_board rejects an invalid path", {
  expect_error(init_pipeline_board(""), "path")
  expect_error(init_pipeline_board(c("a", "b")), "path")
  expect_error(init_pipeline_board(NA_character_), "path")
})

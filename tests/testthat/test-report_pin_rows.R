test_that("row counts are read from the parquet metadata of file pins", {
  board <- init_pipeline_board(withr::local_tempdir())
  path <- file.path(withr::local_tempdir(), "TABLE.parquet")
  nanoparquet::write_parquet(data.frame(x = 1:7), path)
  save_local_pin(board, path, "silver", "example", verbose = FALSE)
  out <- report_pin_rows(board, "silver_example")
  expect_equal(out$n_rows, 7)
})

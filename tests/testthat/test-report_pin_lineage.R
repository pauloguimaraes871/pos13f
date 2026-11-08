test_that("every pin is listed with its version and number of inputs", {
  board <- init_pipeline_board(withr::local_tempdir())
  save_local_pin(board, tibble::tibble(x = 1), "silver", "example", verbose = FALSE)
  save_local_pin(board, tibble::tibble(y = 2), "gold", "example",
    inputs = list(silver_example = "v1"), verbose = FALSE
  )
  out <- report_pin_lineage(board)
  expect_equal(out$pin, c("gold_example", "silver_example"))
  expect_equal(out$n_inputs, c(1, 0))
  expect_false(anyNA(out$version))
})

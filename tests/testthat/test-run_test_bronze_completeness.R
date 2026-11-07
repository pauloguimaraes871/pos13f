fake_bronze_pin <- function(board, label, size_bytes) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  files <- file.path(dir, paste0(c(names(sec_table_schema()), "SOURCE"), ".parquet"))
  purrr::walk(files, function(f) nanoparquet::write_parquet(data.frame(x = 1), f))
  save_local_pin(board, files, "bronze", paste0("sec_", label),
    extra = list(source_bytes = size_bytes, source_last_modified = "Mon, 14 Apr 2014"),
    verbose = FALSE
  )
}

archives_fixture <- tibble::tibble(
  label = c("2014q1", "2014q2"),
  size_bytes = c(38064793, 38411120),
  last_modified = "Mon, 14 Apr 2014"
)

completeness_passes <- function(board) {
  suppressMessages(run_logged_test(
    function() run_test_bronze_completeness(board, archives_fixture),
    log_file = "test_bronze_completeness",
    log_path = withr::local_tempdir(.local_envir = parent.frame())
  ))
}

test_that("completeness passes when every wanted archive is pinned from its source", {
  board <- init_pipeline_board(withr::local_tempdir())
  fake_bronze_pin(board, "2014q1", 38064793)
  fake_bronze_pin(board, "2014q2", 38411120)

  expect_true(completeness_passes(board))
})

test_that("completeness fails when an archive is missing", {
  board <- init_pipeline_board(withr::local_tempdir())
  fake_bronze_pin(board, "2014q1", 38064793)

  expect_false(completeness_passes(board))
})

test_that("completeness fails when a pin comes from a different source file", {
  board <- init_pipeline_board(withr::local_tempdir())
  fake_bronze_pin(board, "2014q1", 38064793)
  fake_bronze_pin(board, "2014q2", 1)

  expect_false(completeness_passes(board))
})

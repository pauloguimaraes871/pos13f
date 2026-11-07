#' Data-quality suite: every wanted SEC archive is pinned, complete, from its source
#'
#' Run at the end of [deploy_bronze()]. Answers "did we download and pin everything we
#' wanted?": one pin per archive, six table files plus `SOURCE.parquet` per pin, and
#' the pin built from the source SEC lists today (same size and Last-Modified date).
#'
#' @param board A `pins_board` from [init_pipeline_board()].
#' @param archives Data frame of the wanted archives with `label`, `size_bytes` and
#'   `last_modified`.
#'
#' @return `invisible(TRUE)` when every check passes.
#' @export
run_test_bronze_completeness <- function(board, archives) {
  wanted <- paste0("bronze_sec_", archives$label)

  testthat::test_that("every wanted SEC archive has a bronze pin", {
    missing <- setdiff(wanted, pins::pin_list(board))
    testthat::expect_length(missing, 0)
  })

  testthat::test_that("each bronze pin has six tables plus SOURCE, from its source file", {
    expected_files <- paste0(c(names(sec_table_schema()), "SOURCE"), ".parquet")
    for (i in seq_len(nrow(archives))) {
      meta <- pins::pin_meta(board, wanted[i])
      testthat::expect_setequal(meta$file, expected_files)
      testthat::expect_equal(as.numeric(meta$user$source_bytes), archives$size_bytes[i],
        label = paste(wanted[i], "source bytes")
      )
      testthat::expect_identical(meta$user$source_last_modified, archives$last_modified[i],
        label = paste(wanted[i], "source revision date")
      )
    }
  })

  invisible(TRUE)
}

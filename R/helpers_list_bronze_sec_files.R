#' Paths of one table's parquet files in the bronze SEC pins of the sample
#'
#' @param board A `pins_board` from [init_pipeline_board()].
#' @param table Character scalar, a table of `sec_table_schema()`, for example
#'   `"SUBMISSION"` or `"INFOTABLE"`.
#' @param labels Character vector, labels of the archives in the sample window (column
#'   `label` of [list_sec_archives()]). Pins of archives outside the window may remain
#'   on the board; they are not listed.
#'
#' @return Character vector of local file paths, named by archive label. Attribute
#'   `inputs`: named list of the bronze pin versions (lineage).
#' @noRd
list_bronze_sec_files <- function(board, table, labels) {
  # Validate inputs
  if (!is.character(table) || length(table) != 1 || !table %in% names(sec_table_schema())) {
    stop("table must be one of: ", paste(names(sec_table_schema()), collapse = ", "), ".")
  }
  if (!is.character(labels) || length(labels) == 0 || anyNA(labels) || anyDuplicated(labels)) {
    stop("labels must be a non-empty character vector of distinct archive labels.")
  }
  pins_sec <- paste0("bronze_sec_", labels)
  ## An archive of the window without a pin would silently shrink the sample
  missing_pins <- setdiff(pins_sec, pins::pin_list(board))
  if (length(missing_pins) > 0) {
    stop(
      "The board has no pin for: ", paste(missing_pins, collapse = ", "),
      ". Run deploy_bronze() on the same window first."
    )
  }

  # Current version of each pin
  paths <- purrr::map_chr(pins_sec, function(p) {
    files <- pins::pin_download(board, p)
    files[basename(files) == paste0(table, ".parquet")]
  })
  inputs <- stats::setNames(
    purrr::map(pins_sec, function(p) pins::pin_meta(board, p)$local$version),
    pins_sec
  )
  structure(stats::setNames(paths, labels), inputs = inputs)
}

#' Read one table of the bronze SEC archives of the sample into a single tibble
#'
#' For the small filing-level tables (SUBMISSION, COVERPAGE, SUMMARYPAGE, OTHERMANAGER,
#' OTHERMANAGER2: about 400,000 rows each at most). INFOTABLE (about 150 million rows)
#' must stay in duckdb and is never read with this function.
#'
#' @param board A `pins_board` from [init_pipeline_board()].
#' @param table Character scalar, one of the filing-level tables of
#'   `sec_table_schema()` (not `"INFOTABLE"`).
#' @param labels Character vector, labels of the archives to read (column `label` of
#'   [list_sec_archives()], for example `"2014q1"` or `"01jun2026_31aug2026"`). Pins of
#'   archives outside the sample window may remain on the board; they are not read.
#'
#' @return A tibble with the columns of `table` plus `source_archive` (character, the
#'   archive label). Attribute `inputs`: named list of the bronze pin versions read
#'   (lineage).
#' @noRd
read_bronze_sec_table <- function(board, table, labels) {
  # Validate inputs
  allowed <- setdiff(names(sec_table_schema()), "INFOTABLE")
  if (!is.character(table) || length(table) != 1 || !table %in% allowed) {
    stop("table must be one of: ", paste(allowed, collapse = ", "), ".")
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

  # Read the current version of each archive
  out <- purrr::map(pins_sec, function(p) {
    files <- pins::pin_download(board, p)
    nanoparquet::read_parquet(files[basename(files) == paste0(table, ".parquet")]) |>
      tibble::as_tibble() |>
      dplyr::mutate(source_archive = sub("^bronze_sec_", "", p))
  }) |>
    purrr::list_rbind()
  inputs <- stats::setNames(
    purrr::map(pins_sec, function(p) pins::pin_meta(board, p)$local$version),
    pins_sec
  )
  structure(out, inputs = inputs)
}

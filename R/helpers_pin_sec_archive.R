#' Pin one validated SEC archive as a bronze pin with its source identity
#'
#' Writes the six tables plus `SOURCE.parquet` to parquet files and pins them as
#' `bronze_sec_<label>`. SOURCE records where the data came from: url, SEC's
#' `Last-Modified` date, and the size and MD5 hash measured on the downloaded zip.
#' Because SOURCE is part of the pinned content, a republished archive always gets a
#' new version that names its new source, even when the tables are unchanged.
#'
#' @param board A `pins_board` from [init_pipeline_board()].
#' @param tables Named list of six tibbles from [read_sec_archive()], already checked
#'   by [run_test_bronze_sec_archive()].
#' @param archive One-row data frame with `url`, `label` and `last_modified`.
#' @param zip_path Character scalar, path of the downloaded zip the tables came from.
#' @param verbose Logical; print pins messages.
#'
#' @return Character scalar, the pin version.
#' @noRd
pin_sec_archive <- function(board, tables, archive, zip_path, verbose = TRUE) {
  # Source identity, measured on the file actually read
  source_info <- tibble::tibble(
    source_url = archive$url,
    source_last_modified = archive$last_modified,
    source_bytes = file.size(zip_path),
    source_md5 = unname(tools::md5sum(zip_path))
  )

  # Write the parquet files and pin them
  out_dir <- tempfile("bronze_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  to_write <- c(tables, list(SOURCE = source_info))
  files <- file.path(out_dir, paste0(names(to_write), ".parquet"))
  purrr::walk2(to_write, files, function(df, f) nanoparquet::write_parquet(df, f))
  ## The same identity goes into the metadata, where deploy_bronze() reads it to
  ## decide quickly whether an archive is already current
  save_local_pin(board, files, "bronze", paste0("sec_", archive$label),
    extra = as.list(source_info), verbose = verbose
  )
}

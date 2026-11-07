#' Save a validated table as a versioned pin of a pipeline layer
#'
#' Deploy functions call this after their data-quality suite has passed. The pin name
#' is `<stage>_<table>`. A data frame is written as parquet with
#' [pins::pin_write()]; character paths to existing parquet files (large tables written
#' by duckdb) are versioned with [pins::pin_upload()] without being read into R.
#'
#' @param board A `pins_board` from [init_pipeline_board()].
#' @param data A data frame, or a character vector of paths to existing `.parquet`
#'   files.
#' @param stage Character scalar, the layer: one of `"bronze"`, `"silver"`, `"gold"`,
#'   `"backtests"`, `"reports"`.
#' @param table Character scalar, snake_case table name (lower-case letters, digits,
#'   underscores), for example `"sec_2014q1"` or `"holdings"`.
#' @param inputs Named list mapping each input pin name to the version it was built
#'   from (lineage). Empty list for source data.
#' @param extra Named list of further metadata, for example the source url and byte
#'   size of a bronze archive.
#' @param verbose Logical; print pins messages.
#'
#' @return Character scalar, the version of the pin after the call.
#'
#' @details
#' pins compares a content hash before writing: identical content creates no new
#' version and the existing version is returned. Metadata records stage, table, row
#' count (data frames only), input versions and the package version.
#' @export
save_local_pin <- function(board, data, stage, table, inputs = list(), extra = list(),
                           verbose = TRUE) {
  # Validate inputs
  allowed_stages <- c("bronze", "silver", "gold", "backtests", "reports")
  if (!inherits(board, "pins_board")) stop("board must be a pins board (init_pipeline_board()).")
  if (!is.character(stage) || length(stage) != 1 || !stage %in% allowed_stages) {
    stop("stage must be one of: ", paste(allowed_stages, collapse = ", "), ".")
  }
  if (!is.character(table) || length(table) != 1 || !grepl("^[a-z0-9_]+$", table)) {
    stop("table must be a snake_case string (lower-case letters, digits, underscores).")
  }
  is_files <- is.character(data) && length(data) > 0 && all(file.exists(data)) &&
    all(grepl("\\.parquet$", data))
  if (!is.data.frame(data) && !is_files) {
    stop("data must be a data frame or paths to existing .parquet files.")
  }
  if (!is.list(inputs) || (length(inputs) > 0 && is.null(names(inputs)))) {
    stop("inputs must be a named list of input pin versions (empty list for sources).")
  }
  if (!is.list(extra) || (length(extra) > 0 && is.null(names(extra)))) {
    stop("extra must be a named list of metadata fields.")
  }

  # Write the pin
  name <- paste(stage, table, sep = "_")
  metadata <- c(list(
    stage = stage, table = table, inputs = inputs,
    n_rows = if (is.data.frame(data)) nrow(data) else NA_integer_,
    pos13f_version = as.character(getNamespaceVersion("pos13f"))
  ), extra)
  ## pins stores metadata as YAML with 7 significant digits, so a double such as
  ## 100719015 would come back as 100719020. Doubles are stored as exact text instead
  ## (integers are written exactly and stay integers); readers use as.numeric().
  metadata <- purrr::map(metadata, function(x) {
    if (is.double(x)) format(x, scientific = FALSE, digits = 15, trim = TRUE) else x
  })
  ## pins skips the write (and keeps the current version) when the content hash is
  ## unchanged, so reruns of an unchanged layer do not duplicate data on disk. When the
  ## content is identical but was built from different input versions, a new version
  ## is forced so the lineage stays true (Codex B04).
  old_meta <- if (name %in% pins::pin_list(board)) pins::pin_meta(board, name)$user
  old_inputs <- old_meta$inputs
  new_lineage <- !identical(unname(unlist(old_inputs)), unname(unlist(inputs))) ||
    !identical(names(unlist(old_inputs)), names(unlist(inputs)))
  ## The same holds for the extra metadata: a record such as the sample window must not
  ## be lost because the content happens to be unchanged
  new_extra <- length(extra) > 0 &&
    !identical(old_meta[names(extra)], metadata[names(extra)])
  new_version <- new_lineage || new_extra
  ## pins::pin_upload() stores a new version on every call, so for files the same rule
  ## is applied here: an MD5 fingerprint of the files is kept in the metadata, and
  ## identical files with the same inputs and extra metadata return the current version
  if (!is.data.frame(data)) {
    metadata$content_md5 <- paste(sort(paste(basename(data), tools::md5sum(data))),
      collapse = ";"
    )
    if (!new_version && identical(old_meta$content_md5, metadata$content_md5)) {
      return(pins::pin_meta(board, name)$local$version)
    }
  }
  write <- function() {
    if (is.data.frame(data)) {
      pins::pin_write(board, data, name,
        type = "parquet", metadata = metadata,
        versioned = TRUE, force_identical_write = new_version
      )
    } else {
      pins::pin_upload(board, data, name, metadata = metadata, versioned = TRUE)
    }
  }
  if (verbose) write() else suppressMessages(write())

  pins::pin_meta(board, name)$local$version
}

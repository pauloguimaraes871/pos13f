#' Read the six tables of one SEC Form 13F data set archive
#'
#' Bronze reader: values are kept exactly as SEC delivered them (no case changes, no
#' filtering of options or bonds); only types are set, from `sec_table_schema()`. The
#' zip is extracted to a temporary folder that is deleted on exit.
#'
#' @param zip_path Character scalar, path to a downloaded `*_form13f.zip` archive.
#' @param verbose Logical; print one progress line per table.
#'
#' @return Named list of six tibbles (SUBMISSION, COVERPAGE, SUMMARYPAGE, OTHERMANAGER,
#'   OTHERMANAGER2, INFOTABLE). Each carries attributes `source_lines` (lines in the
#'   source file, header included) and `n_problems` (readr parsing problems), which the
#'   bronze data-quality suite checks.
#'
#' @details
#' Fields are read with `quote = ""`: issuer names such as `"BROOKFIELD" INC` contain
#' quote characters that are data, not delimiters. Empty fields become `NA`. A header
#' that differs from the schema stops the read, because a silent column shift would
#' corrupt every later layer.
#' @export
read_sec_archive <- function(zip_path, verbose = TRUE) {
  # Validate inputs
  if (!is.character(zip_path) || length(zip_path) != 1 || !file.exists(zip_path)) {
    stop("zip_path must be the path of an existing SEC 13F zip archive.")
  }

  # Extract the tables
  schema <- sec_table_schema()
  exdir <- tempfile("sec_archive_")
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  ## Tables sit at the zip root up to early 2025 and inside a folder named after the
  ## archive from 01jun2025-31aug2025, so they are located by file name
  entries <- utils::unzip(zip_path, list = TRUE)$Name
  wanted <- paste0(names(schema), ".tsv")
  found <- entries[basename(entries) %in% wanted]
  missing <- setdiff(wanted, basename(found))
  if (length(missing) > 0) {
    stop(basename(zip_path), " lacks table(s): ", paste(missing, collapse = ", "), ".")
  }
  ## junkpaths drops the folder, so every table lands directly in exdir
  utils::unzip(zip_path, files = found, exdir = exdir, junkpaths = TRUE)

  # Read each table with its expected header and types
  purrr::imap(schema, function(spec, tbl) {
    path <- file.path(exdir, paste0(tbl, ".tsv"))
    header <- strsplit(readLines(path, n = 1, warn = FALSE), "\t")[[1]]
    if (!identical(header, names(spec$cols))) {
      stop(tbl, " header differs from the expected schema: SEC changed the table, ",
        "update sec_table_schema() after checking the readme.",
        call. = FALSE
      )
    }
    df <- readr::read_delim(path,
      delim = "\t", col_types = spec, quote = "",
      na = "", progress = FALSE, show_col_types = FALSE
    )
    if (verbose) message(basename(zip_path), " ", tbl, ": ", nrow(df), " rows")
    structure(df,
      source_lines = count_file_lines(path),
      n_problems = nrow(readr::problems(df))
    )
  })
}

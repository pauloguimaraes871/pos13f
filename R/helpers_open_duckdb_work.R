#' Open an in-memory duckdb connection set up for large silver builds
#'
#' The silver layers join and window tens of millions of rows, more than fits in the
#' memory limit, so duckdb must spill to a folder on the data disk. Two threads and no
#' insertion-order preservation keep memory per operator low (plan Section 1.0).
#'
#' @param memory_limit Character scalar, duckdb memory limit, for example `"3GB"`.
#' @param temp_dir Character scalar, folder for duckdb's spill files; created if needed.
#'
#' @return A `duckdb_connection`. The caller disconnects it.
#' @noRd
open_duckdb_work <- function(memory_limit = "3GB", temp_dir) {
  # Validate inputs
  valid_limit <- is.character(memory_limit) && length(memory_limit) == 1 &&
    grepl("^[0-9.]+ ?[KMGT]?B$", memory_limit)
  if (!valid_limit) {
    stop("memory_limit must be a size such as \"3GB\".")
  }
  if (!is.character(temp_dir) || length(temp_dir) != 1) stop("temp_dir must be a folder path.")

  # Connect and configure
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  ## shared_home = FALSE: no extensions or secrets written to ~/.duckdb
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  settings <- c(
    paste0("SET memory_limit = '", memory_limit, "'"), "SET threads = 2",
    paste0("SET temp_directory = '", normalizePath(temp_dir, winslash = "/"), "'"),
    "SET preserve_insertion_order = false"
  )
  purrr::walk(settings, function(s) DBI::dbExecute(con, s))
  con
}

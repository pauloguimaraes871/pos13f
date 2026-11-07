#' Deploy the silver holdings state: positions per filer and quarter, point in time
#'
#' Orders the silver filings into amendment events, sums the share positions of every
#' bronze INFOTABLE row inside duckdb, combines them into the holdings state, runs
#' `run_test_silver_holdings()` and pins `silver_holdings_state` (one parquet file,
#' about 116 million rows) and `silver_holdings_versions` (the event table, one row per
#' applied filing) only if every check passes.
#'
#' @param board A `pins_board` from [init_pipeline_board()].
#' @param archives Data frame of the SEC archives in the sample window (`label`), the
#'   same as given to [deploy_silver_filings()].
#' @param log_path Character scalar, folder for the data-quality log.
#' @param memory_limit Character scalar, duckdb memory limit (for example `"3GB"`).
#' @param temp_dir Character scalar, folder where duckdb spills to disk; on the data
#'   disk, because the join of 116 million positions does not fit in memory.
#' @param verbose Logical; print progress and pins messages.
#'
#' @return A tibble with one row per pinned table: `table` and `version`.
#'
#' @details
#' Metadata records the counts the memo discloses: untyped amendments ignored, share
#' rows dropped for a malformed CUSIP, and positions of filings that are not silver
#' filings (notice filings with an information table).
#' @export
deploy_silver_holdings <- function(board, archives, log_path = file.path("logs", "tests"),
                                   memory_limit = "3GB",
                                   temp_dir = file.path("data-raw", "cache", "duckdb_tmp"),
                                   verbose = TRUE) {
  # Validate inputs
  if (!inherits(board, "pins_board")) stop("board must be a pins board (init_pipeline_board()).")
  if (!is.data.frame(archives) || !"label" %in% names(archives) || nrow(archives) == 0) {
    stop("archives must be a non-empty data frame with a label column (list_sec_archives()).")
  }

  # Events and their sources, in R (one row per filing)
  filings <- pins::pin_read(board, "silver_filings")
  events <- order_holdings_events(filings)
  sources <- build_holdings_sources(events)

  # Positions and state, inside duckdb
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  settings <- c(
    paste0("SET memory_limit = '", memory_limit, "'"), "SET threads = 2",
    paste0("SET temp_directory = '", normalizePath(temp_dir, winslash = "/"), "'"),
    "SET preserve_insertion_order = false"
  )
  purrr::walk(settings, function(s) DBI::dbExecute(con, s))
  work_dir <- tempfile("holdings_", tmpdir = temp_dir)
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  ## Writes a lazy query to parquet inside duckdb and reads it back lazily
  materialise <- function(query, name) {
    path <- normalizePath(file.path(work_dir, paste0(name, ".parquet")),
      winslash = "/", mustWork = FALSE
    )
    DBI::dbExecute(con, paste0(
      "COPY (", dbplyr::sql_render(query), ") TO '", path, "' (FORMAT PARQUET)"
    ))
    path
  }
  paths <- list_bronze_sec_files(board, "INFOTABLE", archives$label)
  infotable <- dplyr::tbl(con, dplyr::sql(paste0(
    "SELECT * FROM read_parquet([",
    paste0("'", normalizePath(paths, winslash = "/"), "'", collapse = ", "), "])"
  )))
  valid <- list_valid_position_cusips(infotable)
  regimes <- dplyr::select(filings, "accession_number", "value_regime")
  copy <- function(df, name) dplyr::copy_to(con, df, name, temporary = TRUE)
  ## The positions feed both the state and the checks, so they are built once, on disk
  ## (116 million rows do not fit in the memory limit)
  positions_path <- summarise_holdings_positions(infotable, copy(valid, "valid_cusips"),
    regimes = copy(regimes, "regimes")
  ) |>
    materialise("positions")
  read <- function(path) {
    dplyr::tbl(con, dplyr::sql(paste0("SELECT * FROM read_parquet('", path, "')")))
  }
  positions <- read(positions_path)
  if (verbose) message("Silver holdings: positions summed")
  sources_db <- copy(sources, "sources")
  ## Written once; the checks then scan the file instead of recomputing the join
  out <- build_silver_holdings_state(positions, sources_db) |>
    materialise("HOLDINGS_STATE")
  state <- read(out)
  if (verbose) message("Silver holdings: state written")

  # Test, then pin (nothing is pinned when a check fails)
  checks <- summarise_holdings_checks(state, positions, sources_db, infotable)
  ok <- run_logged_test(
    function() run_test_silver_holdings(events, sources, filings, checks),
    log_file = "test_silver_holdings", log_path = log_path
  )
  check_test_success(ok)
  n_orphans <- dplyr::anti_join(checks$entry_counts, regimes,
    by = dplyr::join_by("accession_number")
  ) |> nrow()
  inputs <- c(
    list(silver_filings = pins::pin_meta(board, "silver_filings")$local$version),
    attr(paths, "inputs")
  )
  pin <- function(data, table, extra = list()) {
    save_local_pin(board, data, "silver", table, inputs = inputs, extra = extra, verbose = verbose)
  }
  tibble::tibble(
    table = c("holdings_state", "holdings_versions"),
    version = c(
      pin(out, "holdings_state", extra = list(
        n_state_rows = checks$totals$n_rows,
        n_untyped_amendments_ignored = attr(events, "n_untyped_ignored"),
        n_malformed_cusips = attr(valid, "n_dropped"),
        n_filings_with_rows_not_in_silver = n_orphans
      )),
      pin(tibble::as_tibble(structure(events, n_untyped_ignored = NULL)), "holdings_versions")
    )
  )
}

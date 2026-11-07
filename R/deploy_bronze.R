#' Deploy the bronze layer: SEC Form 13F archives as delivered, tested and pinned
#'
#' For each wanted archive: download (cached), read the six tables with their expected
#' types, run the bronze data-quality suite, and pin the tables as parquet files under
#' `bronze_sec_<label>`, together with `SOURCE.parquet` (url, Last-Modified date, size
#' and MD5 hash of the zip). Ends with the completeness suite over all wanted archives.
#'
#' @param board A `pins_board` from [init_pipeline_board()].
#' @param archives Data frame from [list_sec_archives()], possibly filtered to the
#'   analysis window: columns `file`, `url`, `label`, `window_start`, `window_end`,
#'   `size_bytes`, `last_modified`.
#' @param cache_dir Character scalar, folder for the downloaded zips.
#' @param user_agent Character scalar, `"Name email"` required by SEC.
#' @param log_path Character scalar, folder for the data-quality logs.
#' @param min_filings,min_holdings Numeric scalars, minimum SUBMISSION and INFOTABLE
#'   rows of a full archive (see [run_test_bronze_sec_archive()]).
#' @param partial_archives Character vector, labels of archives SEC publishes as
#'   partial. Default `"2013q2"`: the XML data sets start in May 2013 (SEC readme,
#'   section 2), so that quarter holds 166 filings and 87,715 holdings.
#' @param partial_min_filings,partial_min_holdings Numeric scalars, the lower floors
#'   applied to `partial_archives`; still high enough to reject an empty file.
#' @param verbose Logical; print progress.
#'
#' @return A tibble, one row per archive: `label`, `pin_name`, `status` (`"pinned"` or
#'   `"skipped"`) and `version`.
#'
#' @details
#' An archive already pinned from the source SEC lists today (same size and same
#' Last-Modified date) is skipped, so a rerun does not re-read 54 archives. When SEC
#' revised an archive, it is downloaded again (the cached zip may be stale) and
#' re-pinned as a new version. A failed check stops the deploy before that archive is
#' pinned; archives pinned earlier in the run stay valid.
#' @export
deploy_bronze <- function(board, archives, cache_dir = file.path("data-raw", "cache", "sec"),
                          user_agent = Sys.getenv("SEC_USER_AGENT"),
                          log_path = file.path("logs", "tests"),
                          min_filings = 3000, min_holdings = 1e6,
                          partial_archives = "2013q2", partial_min_filings = 100,
                          partial_min_holdings = 5e4, verbose = TRUE) {
  # Validate inputs
  required <- c(
    "file", "url", "label", "window_start", "window_end", "size_bytes",
    "last_modified"
  )
  if (!is.data.frame(archives) || !all(required %in% names(archives)) || nrow(archives) == 0) {
    stop(
      "archives must be a non-empty data frame with columns: ",
      paste(required, collapse = ", "), " (see list_sec_archives())."
    )
  }
  if (!inherits(board, "pins_board")) stop("board must be a pins board (init_pipeline_board()).")

  # Deploy archive by archive
  ## A for loop (not a map) keeps one archive in memory at a time and stops at the first
  ## failed check
  status <- vector("list", nrow(archives))
  for (i in seq_len(nrow(archives))) {
    archive <- archives[i, ]
    pin_name <- paste0("bronze_sec_", archive$label)

    ## Skip: already pinned, in the current format (six tables plus SOURCE), from the
    ## source SEC lists today (same size and same Last-Modified date)
    expected_files <- paste0(c(names(sec_table_schema()), "SOURCE"), ".parquet")
    is_pinned <- pin_name %in% pins::pin_list(board)
    meta <- if (is_pinned) pins::pin_meta(board, pin_name) else NULL
    is_current <- is_pinned &&
      isTRUE(as.numeric(meta$user$source_bytes) == archive$size_bytes) &&
      identical(meta$user$source_last_modified, archive$last_modified) &&
      setequal(meta$file, expected_files)
    if (is_current) {
      status[[i]] <- tibble::tibble(
        label = archive$label, pin_name = pin_name,
        status = "skipped",
        version = meta$local$version
      )
      next
    }

    ## Download, read, test
    ## An archive pinned before but no longer current was revised by SEC (or pinned in
    ## an older format): the cached zip may be stale, so it is downloaded again
    zip_path <- download_sec_archive(archive$url, cache_dir, user_agent,
      expected_bytes = archive$size_bytes, force = is_pinned, verbose = verbose
    )
    tables <- read_sec_archive(zip_path, verbose = verbose)
    is_partial <- archive$label %in% partial_archives
    floors <- if (is_partial) {
      c(partial_min_filings, partial_min_holdings)
    } else {
      c(min_filings, min_holdings)
    }
    ok <- run_logged_test(
      function() {
        run_test_bronze_sec_archive(tables, archive,
          min_filings = floors[1],
          min_holdings = floors[2]
        )
      },
      log_file = paste0("test_", pin_name), log_path = log_path
    )
    check_test_success(ok)

    ## Pin the tables with the verified source identity
    version <- pin_sec_archive(board, tables, archive, zip_path, verbose = verbose)
    status[[i]] <- tibble::tibble(
      label = archive$label, pin_name = pin_name,
      status = "pinned", version = version
    )
  }

  # Completeness over all wanted archives
  ok <- run_logged_test(function() run_test_bronze_completeness(board, archives),
    log_file = "test_bronze_completeness", log_path = log_path
  )
  check_test_success(ok)
  purrr::list_rbind(status)
}

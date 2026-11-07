#' Deploy the bronze OpenFIGI layer: FIGI replies for every held share CUSIP
#'
#' Maps the distinct share CUSIPs of the bronze SEC archives (`list_bronze_cusips()`)
#' with OpenFIGI in batches of 100, runs [run_test_bronze_openfigi()] and pins the
#' replies as delivered under `bronze_openfigi`, with the bronze SEC versions as inputs.
#'
#' @param board A `pins_board` from [init_pipeline_board()].
#' @param cache_dir Character scalar, folder where each batch reply is saved as soon as
#'   it arrives.
#' @param api_key Character scalar, OpenFIGI API key.
#' @param log_path Character scalar, folder for the data-quality log.
#' @param min_found_share Numeric scalar passed to [run_test_bronze_openfigi()].
#' @param verbose Logical; print progress.
#'
#' @return Character scalar, the version of `bronze_openfigi`.
#'
#' @details
#' Resumable: replies are cached per batch, and only CUSIPs without a cached answer are
#' requested, so an interrupted run (or a rerun after new SEC archives) asks OpenFIGI
#' only for what is missing. A cached "There was an error while processing this
#' request." is OpenFIGI failing, not an answer, so it is requested again; the newest
#' reply per CUSIP is pinned. The number of CUSIPs dropped for a wrong check digit is
#' recorded in the pin metadata (`n_malformed_cusips`). Which reply identifies which
#' instrument is decided in
#' silver (identity acceptance, G1 decisions D2 and D6), not here.
#' @export
deploy_bronze_openfigi <- function(board, cache_dir = file.path("data-raw", "cache", "openfigi"),
                                   api_key = Sys.getenv("OPENFIGI_API_KEY"),
                                   log_path = file.path("logs", "tests"),
                                   min_found_share = 0.3, verbose = TRUE) {
  # Validate inputs
  if (!inherits(board, "pins_board")) stop("board must be a pins board (init_pipeline_board()).")

  # CUSIPs to map, and those without an answer in the cache
  cusips <- list_bronze_cusips(board)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  ## Each cached row keeps the name of its batch file; names start with the run's time
  ## stamp, so a later file holds a newer reply
  read_cache <- function() {
    files <- list.files(cache_dir, pattern = "\\.parquet$", full.names = TRUE)
    if (length(files) == 0) {
      return(tibble::tibble(cusip = character(), status = character(), batch_file = character()))
    }
    purrr::map(files, function(f) {
      dplyr::mutate(nanoparquet::read_parquet(f), batch_file = basename(f))
    }) |>
      purrr::list_rbind()
  }
  ## "Error while processing" is OpenFIGI failing on its side, not an answer: ask again
  transient <- "error: There was an error while processing this request."
  cached <- read_cache()
  answered <- dplyr::filter(cached, .data$status != .env$transient)
  todo <- dplyr::filter(cusips, !.data$cusip %in% answered$cusip)

  # Request missing CUSIPs in batches of 100, caching each reply on arrival
  ## A for loop keeps each batch on disk before the next request, so a crash or a
  ## closed laptop loses at most one batch
  batches <- split(todo, ceiling(seq_len(nrow(todo)) / 100))
  ## Millisecond stamp: two runs (or a retry) in the same second must never reuse a
  ## file name and overwrite earlier replies; names still sort in time order
  stamp <- format(Sys.time(), "%Y%m%dT%H%M%OS3")
  for (i in seq_along(batches)) {
    replies <- map_openfigi_batch(batches[[i]], api_key = api_key)
    batch_file <- file.path(cache_dir, sprintf("batch_%s_%05d.parquet", stamp, i))
    nanoparquet::write_parquet(replies, batch_file)
    if (verbose && i %% 50 == 0) message("OpenFIGI: ", i, " of ", length(batches), " batches")
  }

  # Test and pin the newest reply of exactly the requested CUSIPs
  replies <- read_cache() |>
    dplyr::filter(.data$cusip %in% cusips$cusip) |>
    dplyr::filter(.data$batch_file == max(.data$batch_file), .by = "cusip") |>
    dplyr::select(-"batch_file")
  ok <- run_logged_test(
    function() run_test_bronze_openfigi(replies, cusips, min_found_share = min_found_share),
    log_file = "test_bronze_openfigi", log_path = log_path
  )
  check_test_success(ok)
  save_local_pin(board, replies, "bronze", "openfigi",
    inputs = attr(cusips, "inputs"),
    extra = list(n_malformed_cusips = attr(cusips, "n_malformed")), verbose = verbose
  )
}

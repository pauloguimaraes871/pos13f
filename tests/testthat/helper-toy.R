# Toy fixture: a reproducible sample of real bronze SEC data ----
#
# Built by data-raw/make_toy_fixtures.R (32 filers: 25 drawn at random with a fixed
# seed, 7 forced edge cases; see toy$selected$reason). Made-up fixtures test named
# cases; this one carries the noise of real filings.
toy_bronze_sec <- function() {
  readRDS(testthat::test_path("testdata", "toy_bronze_sec.rds"))
}

## The filer selected for an edge case, by its reason in toy$selected
toy_cik <- function(toy, reason) {
  toy$selected$cik[grepl(reason, toy$selected$reason, fixed = TRUE)][1]
}

## A board holding the toy sample as bronze pins (one per archive it touches) and the
## SPY calendar as bronze Tiingo prices; returns the board and its archives
setup_toy_board <- function(env = parent.frame()) {
  toy <- toy_bronze_sec()
  board <- init_pipeline_board(withr::local_tempdir(.local_envir = env))
  dir <- withr::local_tempdir(.local_envir = env)
  labels <- sort(unique(toy$SUBMISSION$source_archive))
  tables <- c(
    "SUBMISSION", "COVERPAGE", "SUMMARYPAGE", "OTHERMANAGER", "OTHERMANAGER2", "INFOTABLE"
  )
  for (label in labels) {
    accessions <- toy$SUBMISSION$ACCESSION_NUMBER[toy$SUBMISSION$source_archive == label]
    files <- purrr::map_chr(tables, function(table) {
      df <- toy[[table]] |>
        dplyr::filter(.data$ACCESSION_NUMBER %in% accessions) |>
        dplyr::select(-dplyr::any_of("source_archive"))
      path <- file.path(dir, label, paste0(table, ".parquet"))
      dir.create(dirname(path), showWarnings = FALSE)
      nanoparquet::write_parquet(df, path)
      path
    })
    save_local_pin(board, files, "bronze", paste0("sec_", label), verbose = FALSE)
  }
  prices <- tibble::tibble(ticker = "SPY", status = "200", date = toy$calendar)
  price_path <- file.path(dir, "TIINGO_PRICES.parquet")
  nanoparquet::write_parquet(prices, price_path)
  save_local_pin(board, price_path, "bronze", "tiingo_prices", verbose = FALSE)
  list(board = board, archives = dplyr::filter(toy$archives, .data$label %in% labels), toy = toy)
}

toy_silver_filings <- function(toy = toy_bronze_sec()) {
  build_silver_filings(toy$SUBMISSION, toy$COVERPAGE, toy$SUMMARYPAGE,
    calendar = toy$calendar
  )
}

## End to end on the toy sample of real filings: silver filings, then holdings in duckdb
deploy_toy_holdings <- function(s) {
  log_path <- withr::local_tempdir(.local_envir = parent.frame())
  suppressMessages(deploy_silver_filings(s$board, s$archives, log_path = log_path, verbose = FALSE))
  suppressMessages(deploy_silver_holdings(s$board, s$archives,
    log_path = log_path, temp_dir = withr::local_tempdir(.local_envir = parent.frame()),
    verbose = FALSE
  ))
}

test_that("deploy_silver_holdings pins the state and versions of real filings", {
  s <- setup_toy_board()

  out <- deploy_toy_holdings(s)

  expect_equal(out$table, c("holdings_state", "holdings_versions"))
  state <- nanoparquet::read_parquet(pins::pin_download(s$board, "silver_holdings_state"))
  ## The same build run locally in R gives the same state (duckdb and dplyr agree)
  filings <- pins::pin_read(s$board, "silver_filings")
  infotable <- s$toy$INFOTABLE
  local <- build_silver_holdings_state(
    summarise_holdings_positions(infotable, list_valid_position_cusips(infotable),
      regimes = dplyr::select(filings, "accession_number", "value_regime")
    ),
    build_holdings_sources(order_holdings_events(filings))
  )
  key <- c("cik", "period_of_report", "cusip", "valid_from")
  expect_equal(
    dplyr::arrange(tibble::as_tibble(state), dplyr::across(dplyr::all_of(key))),
    dplyr::arrange(local, dplyr::across(dplyr::all_of(key))),
    ignore_attr = TRUE
  )
  meta <- pins::pin_meta(s$board, "silver_holdings_state")$user
  expect_true("silver_filings" %in% names(meta$inputs))
  expect_equal(as.numeric(meta$n_untyped_amendments_ignored), 1)
})

test_that("on real filings, an untyped amendment leaves the original state current", {
  ## Filer 0001780067, 2020Q4: original with 7 rows, then an untyped 13F-HR/A
  s <- setup_toy_board()
  deploy_toy_holdings(s)
  state <- nanoparquet::read_parquet(pins::pin_download(s$board, "silver_holdings_state"))
  q4 <- quarter_of(state, "0001780067", "2020-12-31")
  expect_equal(unique(q4$valid_from), as.Date("2021-02-02"))
  expect_true(all(is.na(q4$valid_to)))
})

test_that("on real filings, a report filed two archives late keeps its quarter", {
  ## Filer 0001845783: its 2020Q3 report (deadline 2020-11-14, archive 2020q4) was filed
  ## 2021-02-16 and placed in archive 2021q1
  s <- setup_toy_board()
  deploy_toy_holdings(s)
  state <- nanoparquet::read_parquet(pins::pin_download(s$board, "silver_holdings_state"))
  filer <- dplyr::filter(state, .data$cik == "0001845783")
  late <- dplyr::filter(filer, .data$period_of_report == as.Date("2020-09-30"))
  expect_equal(nrow(late), 1)
  expect_equal(late$valid_from, as.Date("2021-02-17"))
  ## At the end-November 2020 formation, nothing of this filer is known yet
  expect_equal(sum(filer$valid_from <= as.Date("2020-11-30")), 0)
})

test_that("deploy_silver_holdings rejects archives without labels", {
  s <- setup_toy_board()
  expect_error(
    deploy_silver_holdings(s$board, tibble::tibble(x = 1), verbose = FALSE),
    "label"
  )
})

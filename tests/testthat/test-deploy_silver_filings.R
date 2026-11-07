## The board holds pins for 2022q4 and 2023q1, the archives of the sample window
board_archives <- dplyr::filter(silver_archives, .data$label != "2022q3")

deploy_silver <- function(board, archives = board_archives) {
  suppressMessages(deploy_silver_filings(board, archives,
    log_path = withr::local_tempdir(.local_envir = parent.frame()), verbose = FALSE
  ))
}

test_that("deploy_silver_filings pins filings, relations and calendar with lineage", {
  board <- setup_silver_board()

  out <- deploy_silver(board)

  expect_equal(out$table, c("filings", "filing_relations", "trading_calendar"))
  filings <- pins::pin_read(board, "silver_filings")
  expect_equal(nrow(filings), 3)
  expect_equal(filings$usable_date, as.Date(c("2022-11-15", "2023-01-04", "2023-02-13")))
  meta <- pins::pin_meta(board, "silver_filings")$user
  expect_setequal(
    names(meta$inputs), c("bronze_sec_2022q4", "bronze_sec_2023q1", "bronze_tiingo_prices")
  )
  calendar_meta <- pins::pin_meta(board, "silver_trading_calendar")$user
  expect_named(calendar_meta$inputs, "bronze_tiingo_prices")
})

test_that("deploy_silver_filings pins nothing when the suite fails", {
  ## A filing whose next trading day is past the calendar has no usable_date
  board <- setup_silver_board()
  prices <- tibble::tibble(ticker = "SPY", status = "200", date = silver_calendar[1:6])
  path <- file.path(withr::local_tempdir(), "TIINGO_PRICES.parquet")
  nanoparquet::write_parquet(prices, path)
  save_local_pin(board, path, "bronze", "tiingo_prices", verbose = FALSE)

  expect_error(deploy_silver(board), "Tests failed")
  expect_false("silver_filings" %in% pins::pin_list(board))
})

test_that("deploy_silver_filings rejects an archives table without windows", {
  board <- setup_silver_board()
  expect_error(
    deploy_silver(board, archives = dplyr::select(board_archives, "label")),
    "window_start"
  )
})

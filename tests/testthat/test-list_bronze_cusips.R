# Two fake bronze archives with realistic holding rows
pin_fake_infotable <- function(board, label, infotable) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  path <- file.path(dir, "INFOTABLE.parquet")
  nanoparquet::write_parquet(infotable, path)
  save_local_pin(board, path, "bronze", paste0("sec_", label), verbose = FALSE)
}

info_rows <- function(cusip, type = "SH", putcall = NA_character_) {
  tibble::tibble(CUSIP = cusip, SSHPRNAMTTYPE = type, PUTCALL = putcall)
}

test_that("list_bronze_cusips returns each share CUSIP once, upper-cased, across archives", {
  board <- init_pipeline_board(withr::local_tempdir())
  pin_fake_infotable(board, "2014q1", dplyr::bind_rows(
    info_rows(c("037833100", "594918104")),
    info_rows("38259p508"), ## lower case as typed by a filer
    info_rows("369604BD4", type = "PRN"), ## bond: not a share position
    info_rows("594918104", putcall = "CALL") ## option on a CUSIP also held as shares
  ))
  pin_fake_infotable(board, "2014q2", info_rows(c("037833100", "G4705A100")))

  cusips <- list_bronze_cusips(board)

  expect_equal(cusips$cusip, c("037833100", "38259P508", "594918104", "G4705A100"))
  ## A CUSIP starting with a letter is a CINS (issuer incorporated outside the US)
  expect_equal(cusips$id_type, c("ID_CUSIP", "ID_CUSIP", "ID_CUSIP", "ID_CINS"))
})

test_that("list_bronze_cusips drops malformed identifiers and records input versions", {
  board <- init_pipeline_board(withr::local_tempdir())
  bad_ids <- c("037833100", "12345", "000000000X", " 594918104")
  pin_fake_infotable(board, "2014q1", info_rows(bad_ids))

  cusips <- list_bronze_cusips(board)

  ## Surrounding spaces are trimmed; wrong lengths are not sent to OpenFIGI
  expect_equal(cusips$cusip, c("037833100", "594918104"))
  expect_named(attr(cusips, "inputs"), "bronze_sec_2014q1")
})

test_that("list_bronze_cusips drops codes with a wrong check digit and counts them", {
  ## Filler and padded tickers typed by filers cannot identify a security
  board <- init_pipeline_board(withr::local_tempdir())
  pin_fake_infotable(board, "2014q1", info_rows(c("037833100", "000000001", "00000AAPL")))

  cusips <- list_bronze_cusips(board)

  expect_equal(cusips$cusip, "037833100")
  expect_equal(attr(cusips, "n_malformed"), 2L)
})

test_that("list_bronze_cusips stops when the board has no bronze SEC pin", {
  board <- init_pipeline_board(withr::local_tempdir())
  expect_error(list_bronze_cusips(board), "bronze_sec")
})

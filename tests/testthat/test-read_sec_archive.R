# Zip the fabricated mini archive into a temporary SEC-style zip file
mini_zip <- function(env = parent.frame()) {
  zip_path <- withr::local_tempfile(fileext = "_form13f.zip", .local_envir = env)
  files <- list.files(test_path("fixtures", "sec_mini"), full.names = TRUE)
  zip::zip(zip_path, files = files, mode = "cherry-pick")
  zip_path
}

test_that("read_sec_archive returns the six tables with the schema's columns and types", {
  tables <- read_sec_archive(mini_zip(), verbose = FALSE)

  expect_named(tables, names(sec_table_schema()))
  expect_s3_class(tables$SUBMISSION$FILING_DATE, "Date")
  expect_equal(tables$SUBMISSION$FILING_DATE[1], as.Date("2014-02-14"))
  expect_type(tables$INFOTABLE$VALUE, "double")
  expect_type(tables$INFOTABLE$CUSIP, "character")
})

test_that("read_sec_archive keeps source values unchanged (bronze is as delivered)", {
  tables <- read_sec_archive(mini_zip(), verbose = FALSE)
  info <- tables$INFOTABLE

  ## Lower-case CUSIP, option and bond rows are kept; fixing them is silver's job
  expect_true("38259p508" %in% info$CUSIP)
  expect_true("CALL" %in% info$PUTCALL)
  expect_true("PRN" %in% info$SSHPRNAMTTYPE)
  ## A quote character inside a name is data, not a field delimiter
  expect_true("\"BROOKFIELD ASSET MGMT\" INC" %in% info$NAMEOFISSUER)
})

test_that("read_sec_archive records source line counts and parsing problems", {
  tables <- read_sec_archive(mini_zip(), verbose = FALSE)

  expect_equal(attr(tables$INFOTABLE, "source_lines"), 8)
  expect_equal(nrow(tables$INFOTABLE), 7L)
  expect_equal(attr(tables$INFOTABLE, "n_problems"), 0L)
})

test_that("read_sec_archive stops when SEC changes a table header", {
  dir <- withr::local_tempdir()
  file.copy(list.files(test_path("fixtures", "sec_mini"), full.names = TRUE), dir)
  sub <- readLines(file.path(dir, "SUBMISSION.tsv"))
  sub[1] <- sub("CIK", "FILER_CIK", sub[1])
  writeLines(sub, file.path(dir, "SUBMISSION.tsv"))
  zip_path <- withr::local_tempfile(fileext = ".zip")
  zip::zip(zip_path, files = list.files(dir, full.names = TRUE), mode = "cherry-pick")

  expect_error(read_sec_archive(zip_path, verbose = FALSE), "SUBMISSION.*header")
})

test_that("read_sec_archive finds tables inside a sub-folder of the zip (format from mid-2025)", {
  ## From 01jun2025-31aug2025 SEC nests the tables in a folder named after the archive
  root <- withr::local_tempdir()
  nested <- file.path(root, "01JUN2025-31AUG2025_form13f")
  dir.create(nested)
  file.copy(list.files(test_path("fixtures", "sec_mini"), full.names = TRUE), nested)
  zip_path <- withr::local_tempfile(fileext = "_form13f.zip")
  zip::zip(zip_path, files = "01JUN2025-31AUG2025_form13f", root = root, mode = "mirror")

  tables <- read_sec_archive(zip_path, verbose = FALSE)

  expect_equal(nrow(tables$INFOTABLE), 7L)
})

test_that("read_sec_archive names the table missing from an archive", {
  dir <- withr::local_tempdir()
  file.copy(list.files(test_path("fixtures", "sec_mini"), full.names = TRUE), dir)
  file.remove(file.path(dir, "SUMMARYPAGE.tsv"))
  zip_path <- withr::local_tempfile(fileext = ".zip")
  zip::zip(zip_path, files = list.files(dir, full.names = TRUE), mode = "cherry-pick")

  expect_error(read_sec_archive(zip_path, verbose = FALSE), "SUMMARYPAGE")
})

test_that("read_sec_archive rejects a missing file", {
  expect_error(read_sec_archive(file.path(tempdir(), "none.zip")), "zip_path")
})

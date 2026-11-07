test_that("sec_table_schema covers the six tables used by the pipeline", {
  schema <- sec_table_schema()
  expect_named(schema, c(
    "SUBMISSION", "COVERPAGE", "SUMMARYPAGE", "OTHERMANAGER",
    "OTHERMANAGER2", "INFOTABLE"
  ))
})

test_that("sec_table_schema column names match the fixture headers exactly, in order", {
  schema <- sec_table_schema()
  for (tbl in names(schema)) {
    header <- readLines(test_path("fixtures", "sec_mini", paste0(tbl, ".tsv")), n = 1)
    expect_equal(names(schema[[tbl]]$cols), strsplit(header, "\t")[[1]], label = tbl)
  }
})

test_that("sec_table_schema types dates, counts and amounts explicitly", {
  schema <- sec_table_schema()
  expect_s3_class(schema$SUBMISSION$cols$FILING_DATE, "collector_date")
  expect_s3_class(schema$INFOTABLE$cols$VALUE, "collector_double")
  expect_s3_class(schema$INFOTABLE$cols$SSHPRNAMT, "collector_double")
  ## CUSIP and CIK stay text: leading zeros are part of the identifier
  expect_s3_class(schema$INFOTABLE$cols$CUSIP, "collector_character")
  expect_s3_class(schema$SUBMISSION$cols$CIK, "collector_character")
})

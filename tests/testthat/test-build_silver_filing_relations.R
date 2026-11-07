test_that("build_silver_filing_relations stacks both relations of silver filings", {
  relations <- build_silver_filing_relations(silver_othermanager(), silver_othermanager2(),
    filings = silver_filings_fixture()
  )

  ## The notice filing's relation is dropped; the duplicated included manager kept once
  expect_equal(relations$accession_number, c("0000001001-22-000001", "0000001001-23-000001"))
  expect_equal(relations$relation, c("reports_for_me", "included_in_report"))
  expect_equal(relations$sequence_number, c(NA, 1))
  ## A manager without a CIK keeps its file number and name
  expect_equal(relations$related_cik, c(NA, "0001004"))
  expect_equal(relations$related_file_number, c("028-20001", "028-10004"))
})

test_that("build_silver_filing_relations names missing columns", {
  expect_error(
    build_silver_filing_relations(dplyr::select(silver_othermanager(), -"NAME"),
      silver_othermanager2(),
      filings = silver_filings_fixture()
    ),
    "othermanager"
  )
})

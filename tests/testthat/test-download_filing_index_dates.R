test_that("an empty user agent is refused before any request", {
  filings <- tibble::tibble(accession_number = "0000019617-21-000379", cik = "0000019617")
  expect_error(download_filing_index_dates(filings, user_agent = ""), "SEC_USER_AGENT")
})

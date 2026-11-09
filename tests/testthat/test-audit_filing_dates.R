## Four filings: accepted in the day; accepted after 5:30 pm (filed the next day); a
## made-up bad case accepted after the close of its usable date; and a filing date
## adjustment like accession 0001628280-20-013177 (filing date before acceptance)
audit_fixture <- function() {
  tibble::tibble(
    accession_number = c("a", "b", "c", "d"),
    filing_date = as.Date(c("2021-08-12", "2021-08-13", "2021-08-12", "2020-09-01")),
    usable_date = as.Date(c("2021-08-13", "2021-08-16", "2021-08-12", "2020-09-02")),
    edgar_filing_date = as.Date(c("2021-08-12", "2021-08-13", "2021-08-12", "2020-09-01")),
    edgar_accepted = c(
      "2021-08-12 11:55:03", "2021-08-12 18:02:41", "2021-08-12 17:10:00",
      "2020-09-02 09:01:00"
    )
  )
}

test_that("each check is evaluated per filing", {
  out <- audit_filing_dates(audit_fixture())
  expect_equal(out$rows$same_filing_date, c(TRUE, TRUE, TRUE, TRUE))
  ## Accepted after hours the day before its filing date: allowed
  expect_equal(out$rows$accepted_by_filing_date, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(out$rows$usable_after_acceptance, c(TRUE, TRUE, FALSE, FALSE))
  ## The adjusted filing was public at 9:01 on its usable date, before the close
  expect_equal(out$rows$public_before_trade, c(TRUE, TRUE, FALSE, TRUE))
  expect_equal(out$summary$share_public_before_trade, 3 / 4)
})

test_that("an unread page is excluded from the shares but counted as sampled", {
  x <- audit_fixture()
  x$edgar_accepted[3] <- NA
  out <- audit_filing_dates(x)
  expect_equal(out$summary$n_sampled, 4)
  expect_equal(out$summary$n_read, 3)
  expect_equal(out$summary$share_public_before_trade, 1)
})

test_that("is_valid_cusip accepts real CUSIPs and CINS codes", {
  ## Apple, Microsoft, Google class A (2013), ICON plc (CINS, incorporated in Ireland)
  expect_true(all(is_valid_cusip(c("037833100", "594918104", "38259P508", "G4705A100"))))
})

test_that("is_valid_cusip rejects the format problems found in 13F filings", {
  ## Examples from the 54 archives (2026-11-07): filler, padded ticker, typo in the check
  ## digit, issuer code + 000, a number padded with zeros
  bad <- c("000000001", "00000AAPL", "037833101", "002314000", "000005229")
  expect_false(any(is_valid_cusip(bad)))
})

test_that("is_valid_cusip rejects wrong lengths, lower case and missing values", {
  expect_equal(is_valid_cusip(c("03783310", "0378331000", "38259p508", NA)), rep(FALSE, 4))
})

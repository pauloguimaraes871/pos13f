test_that("build_silver_security_master keeps US common stocks with one FIGI", {
  master <- build_silver_security_master(replies_fixture())

  ## The ETF and the ambiguous CUSIP are out
  expect_equal(master$cusip, c("000000AA1", "000000BB2", "000000CC3", "000000DD4", "000000GG7"))
  expect_equal(attr(master, "n_ambiguous_cusips"), 1)
  expect_equal(master$ticker[master$cusip == "000000GG7"], "BRK-B")
})

test_that("a ticker naming two instruments has no price source", {
  master <- build_silver_security_master(replies_fixture())
  abat <- dplyr::filter(master, .data$ticker == "ABAT")
  expect_equal(abat$instrument_id, c("FIGI_ABAT1", "FIGI_ABAT2"))
  expect_true(all(abat$ticker_shared))
  expect_false(any(abat$has_price_source))
  expect_true(master$has_price_source[master$ticker == "AAA"])
})

test_that("build_silver_security_master names missing columns", {
  expect_error(
    build_silver_security_master(dplyr::select(replies_fixture(), -"composite_figi")),
    "composite_figi"
  )
})

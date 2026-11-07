jobs_fixture <- tibble::tibble(
  cusip = c("037833100", "99999X999", "G4705A100"),
  id_type = c("ID_CUSIP", "ID_CUSIP", "ID_CINS")
)

# OpenFIGI answers one element per job, in order: data, a warning, or an error
openfigi_reply <- function(req) {
  httr2::response_json(body = list(
    list(data = list(list(
      figi = "BBG000B9XRY4", compositeFIGI = "BBG000B9XRY4", shareClassFIGI = "BBG001S5N8V8",
      ticker = "AAPL", name = "APPLE INC", exchCode = "US", securityType = "Common Stock",
      securityType2 = "Common Stock", marketSector = "Equity", securityDescription = "AAPL"
    ))),
    list(warning = "No identifier found."),
    list(error = "Invalid idValue format")
  ))
}

test_that("map_openfigi_batch returns one row per record or per unmatched job", {
  httr2::local_mocked_responses(openfigi_reply)

  replies <- map_openfigi_batch(jobs_fixture, api_key = "test-key")

  expect_equal(replies$cusip, jobs_fixture$cusip)
  expect_equal(replies$status, c("found", "No identifier found.", "error: Invalid idValue format"))
  expect_equal(replies$composite_figi, c("BBG000B9XRY4", NA, NA))
  expect_equal(replies$ticker[1], "AAPL")
  expect_equal(replies$security_type[1], "Common Stock")
})

test_that("map_openfigi_batch sends US-only jobs that include delisted equities", {
  sent <- NULL
  httr2::local_mocked_responses(function(req) {
    sent <<- req$body$data
    openfigi_reply(req)
  })

  map_openfigi_batch(jobs_fixture, api_key = "test-key")

  expect_equal(purrr::map_chr(sent, "idType"), c("ID_CUSIP", "ID_CUSIP", "ID_CINS"))
  expect_true(all(purrr::map_chr(sent, "exchCode") == "US"))
  expect_true(all(purrr::map_lgl(sent, "includeUnlistedEquities")))
})

test_that("map_openfigi_batch rejects more than 100 jobs and a missing key", {
  many <- tibble::tibble(cusip = sprintf("%09d", 1:101), id_type = "ID_CUSIP")
  expect_error(map_openfigi_batch(many, api_key = "k"), "100")
  expect_error(map_openfigi_batch(jobs_fixture, api_key = ""), "api_key")
})

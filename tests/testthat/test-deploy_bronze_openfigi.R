# Board with one fake bronze archive holding three share CUSIPs
setup_openfigi <- function(env = parent.frame()) {
  board <- init_pipeline_board(withr::local_tempdir(.local_envir = env))
  dir <- withr::local_tempdir(.local_envir = env)
  path <- file.path(dir, "INFOTABLE.parquet")
  nanoparquet::write_parquet(tibble::tibble(
    CUSIP = c("037833100", "594918104", "99999A995"), SSHPRNAMTTYPE = "SH",
    PUTCALL = NA_character_
  ), path)
  save_local_pin(board, path, "bronze", "sec_2014q1", verbose = FALSE)
  list(
    board = board, cache_dir = withr::local_tempdir(.local_envir = env),
    log_path = withr::local_tempdir(.local_envir = env)
  )
}

# OpenFIGI mock: every job is found except the CUSIP 99999A995
mock_openfigi <- function(env = parent.frame()) {
  calls <- new.env()
  calls$n <- 0
  httr2::local_mocked_responses(function(req) {
    calls$n <- calls$n + 1
    httr2::response_json(body = purrr::map(req$body$data, function(job) {
      if (job$idValue == "99999A995") {
        return(list(warning = "No identifier found."))
      }
      list(data = list(list(
        figi = paste0("BBG", job$idValue), compositeFIGI = paste0("BBG", job$idValue),
        ticker = substr(job$idValue, 1, 4), exchCode = "US", securityType = "Common Stock",
        marketSector = "Equity"
      )))
    }))
  }, env = env)
  calls
}

deploy <- function(s, ...) {
  suppressMessages(deploy_bronze_openfigi(s$board,
    cache_dir = s$cache_dir, api_key = "k",
    log_path = s$log_path, verbose = FALSE, ...
  ))
}

test_that("deploy_bronze_openfigi pins one reply per requested CUSIP with lineage", {
  s <- setup_openfigi()
  mock_openfigi()

  deploy(s, min_found_share = 0.5)

  replies <- pins::pin_read(s$board, "bronze_openfigi")
  expect_setequal(replies$cusip, c("037833100", "594918104", "99999A995"))
  expect_equal(sum(replies$status == "found"), 2L)
  meta <- pins::pin_meta(s$board, "bronze_openfigi")$user
  expect_named(meta$inputs, "bronze_sec_2014q1")
})

test_that("deploy_bronze_openfigi resumes from its cache without asking again", {
  s <- setup_openfigi()
  calls <- mock_openfigi()
  deploy(s, min_found_share = 0.5)
  first_calls <- calls$n

  deploy(s, min_found_share = 0.5)

  expect_equal(calls$n, first_calls)
})

test_that("deploy_bronze_openfigi asks again for CUSIPs that hit a transient error", {
  ## On 2026-11-07, 8 batches came back as "There was an error while processing this
  ## request.": that is OpenFIGI failing, not an answer, so the next run retries
  s <- setup_openfigi()
  calls <- new.env()
  calls$n <- 0
  httr2::local_mocked_responses(function(req) {
    calls$n <- calls$n + 1
    httr2::response_json(body = purrr::map(req$body$data, function(job) {
      if (calls$n == 1 && job$idValue == "594918104") {
        return(list(error = "There was an error while processing this request."))
      }
      list(data = list(list(
        figi = paste0("BBG", job$idValue), compositeFIGI = paste0("BBG", job$idValue),
        ticker = "T", exchCode = "US", securityType = "Common Stock"
      )))
    }))
  })
  ## First run: one transient error out of three is above the 5% error ceiling
  expect_error(deploy(s), "Tests failed")

  deploy(s)

  replies <- pins::pin_read(s$board, "bronze_openfigi")
  expect_equal(calls$n, 2)
  expect_equal(replies$status[replies$cusip == "594918104"], "found")
  expect_false(any(duplicated(replies$cusip)))
})

test_that("deploy_bronze_openfigi stops when too few CUSIPs are found", {
  ## Only 2 of 3 found: a 90% floor must fail (as a broken key or outage would)
  s <- setup_openfigi()
  mock_openfigi()

  expect_error(deploy(s, min_found_share = 0.9), "Tests failed")
  expect_false("bronze_openfigi" %in% pins::pin_list(s$board))
})

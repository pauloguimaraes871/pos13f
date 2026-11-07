#' Data-quality suite for the bronze OpenFIGI replies
#'
#' Run by [deploy_bronze_openfigi()] before the replies are pinned.
#'
#' @param replies Tibble from [map_openfigi_batch()] for all requested CUSIPs.
#' @param cusips Tibble of requested CUSIPs (`cusip`), from `list_bronze_cusips()`.
#' @param min_found_share Numeric scalar, minimum share of CUSIPs with a match. Many
#'   13F CUSIPs are funds, warrants or foreign lines that never map, so the floor is
#'   low; it exists to catch a broken key or an outage that returns nothing.
#' @param max_error_share Numeric scalar, maximum share of CUSIPs whose job returned an
#'   error (as opposed to "not found").
#'
#' @return `invisible(TRUE)` when every check passes.
#' @export
run_test_bronze_openfigi <- function(replies, cusips, min_found_share = 0.3,
                                     max_error_share = 0.05) {
  by_cusip <- dplyr::summarise(replies,
    found = any(.data$status == "found"),
    error = any(startsWith(.data$status, "error")),
    .by = "cusip"
  )

  testthat::test_that("every requested CUSIP has a reply with a status", {
    testthat::expect_length(setdiff(cusips$cusip, replies$cusip), 0)
    testthat::expect_false(anyNA(replies$status))
  })

  testthat::test_that("matches are US listings with a FIGI", {
    found <- dplyr::filter(replies, .data$status == "found")
    testthat::expect_false(anyNA(found$figi))
    testthat::expect_true(all(found$exch_code == "US"))
  })

  testthat::test_that("the match and error rates are plausible", {
    testthat::expect_gte(mean(by_cusip$found), min_found_share)
    testthat::expect_lte(mean(by_cusip$error), max_error_share)
  })

  invisible(TRUE)
}

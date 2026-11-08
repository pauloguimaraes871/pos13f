#' Data-quality suite for the bronze Fama-French factors
#'
#' Run by `deploy_bronze_ff_factors()` before pinning: the table must be a gap-free
#' monthly series covering the study window, in percentage points.
#'
#' @param factors Tibble from `download_ff_factors()`.
#' @param first_month,last_month Date scalars, months the series must cover (the gold
#'   panels run from 2014-03 to 2026-10; the library lags by a few months, so the
#'   default end is earlier and the alpha regressions use the overlap).
#'
#' @return `invisible(TRUE)` when every check passes.
#' @export
run_test_bronze_ff_factors <- function(factors, first_month = as.Date("2014-03-01"),
                                       last_month = as.Date("2026-06-01")) {
  cols <- c("mkt_rf", "smb", "hml", "rmw", "cma", "rf", "umd")
  testthat::test_that("factors are complete, monthly and cover the study window", {
    testthat::expect_true(all(c("dates", cols) %in% names(factors)))
    testthat::expect_false(anyNA(factors[cols]))
    testthat::expect_true(all(lubridate::mday(factors$dates) == 1))
    index <- lubridate::year(factors$dates) * 12 + lubridate::month(factors$dates)
    testthat::expect_true(all(diff(index) == 1))
    testthat::expect_lte(min(factors$dates), first_month)
    testthat::expect_gte(max(factors$dates), last_month)
  })
  testthat::test_that("factors are in percentage points per month", {
    ## A monthly factor beyond 40% or a risk-free rate beyond 2% would mean wrong units
    testthat::expect_true(all(abs(as.matrix(factors[cols])) < 40))
    testthat::expect_true(all(factors$rf >= 0 & factors$rf < 2))
  })
  invisible(TRUE)
}

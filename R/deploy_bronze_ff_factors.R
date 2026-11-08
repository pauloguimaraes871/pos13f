#' Deploy the bronze Fama-French factors: monthly five factors and momentum
#'
#' Downloads the factors with `download_ff_factors()`, runs
#' [run_test_bronze_ff_factors()] and pins them as delivered under
#' `bronze_ff_factors_m`. The backtest alpha regressions read this pin, so a rerun of the
#' backtests never depends on the library being online.
#'
#' @param board A `pins_board` from [init_pipeline_board()].
#' @param log_path Character scalar, folder for the data-quality log.
#' @param verbose Logical; print progress and pins messages.
#'
#' @return Character scalar, the version of `bronze_ff_factors_m`.
#' @export
deploy_bronze_ff_factors <- function(board, log_path = file.path("logs", "tests"),
                                     verbose = TRUE) {
  # Validate inputs
  if (!inherits(board, "pins_board")) stop("board must be a pins board (init_pipeline_board()).")

  # Download, test, pin
  factors <- download_ff_factors(verbose = verbose)
  ok <- run_logged_test(function() run_test_bronze_ff_factors(factors),
    log_file = "test_bronze_ff_factors", log_path = log_path
  )
  check_test_success(ok)
  save_local_pin(board, factors, "bronze", "ff_factors_m", verbose = verbose)
}

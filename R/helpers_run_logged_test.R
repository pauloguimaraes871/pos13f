#' Run a data-quality suite with its output captured in a log file
#'
#' Deploy functions run their `run_test_<layer>()` suite through this wrapper, as in
#' the owner's `pipeline` repository. Console output and messages go to a timestamped
#' log. `fun` runs under testthat's stop reporter, so a failing
#' `testthat::test_that()` block raises an error; it is caught here and turned into
#' `FALSE` so the deploy can stop before pinning.
#'
#' @param fun A function without arguments that runs the checks.
#' @param log_file Character scalar, log file name without extension (a timestamp is
#'   appended).
#' @param log_path Character scalar, folder for the logs; created if missing.
#'
#' @return Logical scalar, `TRUE` if `fun` ran without error, with attribute `log_file`
#'   holding the path of the log.
#' @export
run_logged_test <- function(fun, log_file, log_path = file.path("logs", "tests")) {
  # Validate inputs
  if (!is.function(fun)) stop("fun must be a function without arguments.")
  if (!is.character(log_file) || length(log_file) != 1 || !nzchar(log_file)) {
    stop("log_file must be a single non-empty character string.")
  }

  # Open the log and redirect output and messages to it
  dir.create(log_path, recursive = TRUE, showWarnings = FALSE)
  stamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  log_full <- file.path(log_path, paste0(log_file, "_", stamp, ".log"))
  con <- file(log_full, open = "wt")
  sink(con, type = "output")
  sink(con, type = "message")
  ## on.exit restores the console even if fun() fails in an unexpected way
  on.exit(
    {
      sink(NULL, type = "message")
      sink(NULL, type = "output")
      close(con)
    },
    add = TRUE
  )

  # Run the checks
  ## The "stop" reporter turns any failed expectation into an error, so a failure stops
  ## the deploy whether this runs from targets, the console or inside a unit test
  ok <- tryCatch(
    {
      testthat::with_reporter(testthat::StopReporter$new(), fun())
      TRUE
    },
    error = function(e) {
      message("Checks failed: ", conditionMessage(e))
      FALSE
    }
  )
  structure(ok, log_file = log_full)
}

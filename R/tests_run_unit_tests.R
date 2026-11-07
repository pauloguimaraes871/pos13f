#' Run the package's unit tests as the first step of the pipeline
#'
#' The `targets` pipeline calls this before any deploy, and every deploy target
#' depends on its result: if one unit test fails, nothing is downloaded, built or
#' pinned. This guarantees the unit tests run every time the pipeline runs, not only
#' when a developer remembers `devtools::test()`.
#'
#' @param test_path Character scalar, folder of the unit tests.
#' @param load_package Character scalar passed to [testthat::test_dir()]: `"source"`
#'   loads the package from the working tree (pipeline use); `"none"` for test folders
#'   that do not need it.
#' @param log_path Character scalar, folder for the log.
#'
#' @return `invisible(TRUE)` when all unit tests pass; otherwise an error naming the log.
#' @export
run_unit_tests <- function(test_path = file.path("tests", "testthat"),
                           load_package = "source",
                           log_path = file.path("logs", "tests")) {
  # Validate inputs
  if (!is.character(test_path) || length(test_path) != 1 || !dir.exists(test_path)) {
    stop("test_path must be an existing folder of unit tests.")
  }

  # Run the suite through the logged runner; any failure becomes an error
  ok <- run_logged_test(
    function() {
      testthat::test_dir(test_path,
        package = if (load_package == "none") NULL else "pos13f",
        load_package = load_package, reporter = "summary", stop_on_failure = TRUE
      )
    },
    log_file = "unit_tests", log_path = log_path
  )
  check_test_success(ok)
}

#' Stop a deployment when its data-quality suite failed
#'
#' @param success_flag The value returned by [run_logged_test()]: logical, with
#'   attribute `log_file`.
#'
#' @return `invisible(TRUE)` when the checks passed; otherwise an error naming the log,
#'   so nothing is pinned from data that failed its checks.
#' @export
check_test_success <- function(success_flag) {
  if (!isTRUE(as.vector(success_flag))) {
    stop("Tests failed. Stopping deployment. See log: ",
      attr(success_flag, "log_file") %||% "(no log)",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

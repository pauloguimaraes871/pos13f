#' Set missing values between an instrument's first and last observation to 0
#'
#' Used for daily returns in the covariance input only (`shape_backtest_inputs()`): a
#' day without a price row inside an instrument's life has a return of exactly 0,
#' because silver books the move across the gap on the next traded day.
#'
#' @param x Numeric vector of daily returns ordered by date, `NA` where no price row.
#'
#' @return `x` with every `NA` strictly between the first and the last non-missing value
#'   set to 0; leading and trailing `NA` (before listing, after the last price) unchanged.
#' @noRd
fill_inner_gaps <- function(x) {
  observed <- which(!is.na(x))
  if (length(observed) < 2) {
    return(x)
  }
  inner <- seq(min(observed), max(observed))
  x[inner] <- dplyr::coalesce(x[inner], 0)
  x
}

#' Bootstrap test that mean returns rise across the quantile portfolios
#'
#' The monotonic relation (MR) test: the null is that mean returns do not increase from
#' each portfolio to the next; the statistic is the smallest mean difference between
#' adjacent portfolios, and its p-value comes from a stationary block bootstrap of the
#' demeaned differences, which keeps their serial and cross correlation. Patton and
#' Timmermann (2010) proposed the test, and Romano and Wolf (2013) refine it \[mem\]; this
#' is a diagnostic, not a pre-registered verdict.
#'
#' @param ladder Tibble with `quantile`, `dates` and the return column.
#' @param value Character scalar, the return column (`raw_return` by default).
#' @param n_boot Numeric scalar, bootstrap draws.
#' @param mean_block Numeric scalar, mean block length in months (geometric blocks).
#' @param seed Integer scalar, the random seed.
#'
#' @return A tibble with `min_diff` (smallest adjacent mean difference, percentage
#'   points per month), `diffs` (list column, every adjacent difference) and `p_value`.
#' @noRd
test_monotonicity <- function(ladder, value = "raw_return", n_boot = 2000, mean_block = 6,
                              seed = 20261108) {
  wide <- ladder |>
    dplyr::select("dates", "quantile", ret = dplyr::all_of(value)) |>
    tidyr::pivot_wider(names_from = "quantile", values_from = "ret", names_sort = TRUE) |>
    dplyr::arrange(.data$dates)
  r <- as.matrix(wide[-1])
  d <- r[, -1, drop = FALSE] - r[, -ncol(r), drop = FALSE]
  observed <- colMeans(d)
  centred <- sweep(d, 2, observed)
  n <- nrow(d)
  draw <- function() {
    ## Stationary bootstrap: blocks start at random months, lengths are geometric
    idx <- integer(n)
    idx[1] <- sample.int(n, 1)
    for (i in seq_len(n)[-1]) {
      idx[i] <- if (stats::runif(1) < 1 / mean_block) sample.int(n, 1) else idx[i - 1] %% n + 1
    }
    min(colMeans(centred[idx, , drop = FALSE]))
  }
  boot <- withr::with_seed(seed, replicate(n_boot, draw()))
  tibble::tibble(
    min_diff = min(observed), diffs = list(observed),
    p_value = mean(boot >= min(observed))
  )
}

#' Test that mean returns rise across the quantile portfolios
#'
#' An intersection-union test of monotonicity: returns rise across the ladder only if
#' every adjacent spread (Q2 minus Q1, Q3 minus Q2, and so on) has a positive mean. Each
#' spread gets a one-sided Newey-West test of "mean at most 0", and the joint p-value is
#' the largest of them, which is valid whatever the configuration under the null (one
#' flat step is enough to fail). A diagnostic, not a pre-registered verdict.
#'
#' @param ladder Tibble with `quantile`, `dates` and the return column, one row per
#'   quantile and month.
#' @param value Character scalar, the return column (`raw_return` by default).
#' @param lag Numeric scalar, Newey-West lag in months.
#'
#' @return A tibble with `min_diff` (smallest adjacent mean spread, percentage points per
#'   month), `diffs` and `t_stats` (list columns, one value per adjacent spread) and
#'   `p_value` (the largest one-sided p-value).
#'
#' @details
#' Bootstrapping the minimum spread after centring every spread at zero, as an earlier
#' version did, assumes the null configuration where all spreads are zero; when one
#' spread is zero and the others positive that understates the p-value (Codex review of
#' PR #7, 2026-11-08). The intersection-union test is conservative instead.
#' @noRd
test_monotonicity <- function(ladder, value = "raw_return", lag = 3) {
  # Validate inputs
  missing_cols <- setdiff(c("quantile", "dates", value), names(ladder))
  if (length(missing_cols) > 0) {
    stop("ladder is missing columns: ", paste(missing_cols, collapse = ", "), ".")
  }

  # Adjacent spreads, one column each
  wide <- ladder |>
    dplyr::select("dates", "quantile", ret = dplyr::all_of(value)) |>
    tidyr::pivot_wider(names_from = "quantile", values_from = "ret", names_sort = TRUE) |>
    dplyr::arrange(.data$dates)
  r <- as.matrix(wide[-1])
  d <- r[, -1, drop = FALSE] - r[, -ncol(r), drop = FALSE]
  if (anyNA(d)) stop("ladder must have every quantile in every month.")

  # One-sided Newey-West test per spread
  one_sided <- purrr::map(seq_len(ncol(d)), function(j) {
    spread <- d[, j]
    fit <- stats::lm(spread ~ 1)
    t_stat <- mean(spread) / sqrt(sandwich::NeweyWest(fit, lag = lag, prewhite = FALSE))[1]
    c(diff = mean(spread), t = t_stat, p = stats::pnorm(t_stat, lower.tail = FALSE))
  })
  tests <- do.call(rbind, one_sided)
  tibble::tibble(
    min_diff = min(tests[, "diff"]), diffs = list(unname(tests[, "diff"])),
    t_stats = list(unname(tests[, "t"])), p_value = max(tests[, "p"])
  )
}

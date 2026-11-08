#' A factoRverse AND rule that keeps one group of a conditioning variable
#'
#' @param signals Tibble with `id`, `tickers`, `dates` and the group column from
#'   `add_conditional_rank()`.
#' @param group_col Character scalar, the group column (values 1 to n).
#' @param k Numeric scalar, the group to keep.
#'
#' @return A `meta_dataframe` with the five columns factoRverse requires: `id`, `tickers`,
#'   `dates`, `group_label` (chr) and `<group_col>_<k>` (1 inside the group, 0 outside).
#' @noRd
build_group_rule <- function(signals, group_col, k) {
  rule_col <- paste0(group_col, "_", k)
  rule <- signals |>
    dplyr::transmute(
      id = .data$id, tickers = .data$tickers, dates = .data$dates,
      group_label = paste0("group_", .data[[group_col]]),
      "{rule_col}" := as.integer(.data[[group_col]] == k)
    )
  factoRverse::create_meta_dataframe(as.data.frame(rule), meta_dataframe_name = rule_col)
}

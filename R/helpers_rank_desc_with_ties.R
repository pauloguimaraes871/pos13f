#' Rank values from largest to smallest among selected rows, ties broken by an id
#'
#' Used for the liquidity rank of the universe (plan 1.7: ties broken by
#' `instrument_id`), so the ranking never depends on row order.
#'
#' @param value Numeric vector to rank (largest gets rank 1).
#' @param id Character vector of the same length, the tie-breaker (ascending).
#' @param keep Logical vector of the same length; rows to rank.
#'
#' @return Integer vector of the same length: the rank of each kept row, `NA` for the
#'   others.
#' @noRd
rank_desc_with_ties <- function(value, id, keep) {
  # Validate inputs
  if (length(value) != length(id) || length(value) != length(keep)) {
    stop("value, id and keep must have the same length.")
  }
  if (any(keep & is.na(value))) stop("value must not be NA on kept rows.")

  # Rank the kept rows
  kept <- which(keep)
  ranks <- rep(NA_integer_, length(value))
  ranks[kept[order(-value[kept], id[kept])]] <- seq_along(kept)
  ranks
}

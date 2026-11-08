#' Evaluate an expression, muffling only factoRverse's known advisory warnings
#'
#' factoRverse warns on every run that signals are not normalised (our signals are used
#' as raw scores by design: only their ranks and the `cs` score transform matter) and
#' that `strategy_aum` must share units with the liquidity metric (both are USD). Any
#' other warning still reaches the caller.
#'
#' @param expr An expression.
#'
#' @return The value of `expr`.
#' @noRd
muffle_known_warnings <- function(expr) {
  known <- c("Normalization not found in signals_m_df workflow", "strategy_aum is in same units")
  withCallingHandlers(expr, warning = function(w) {
    if (any(purrr::map_lgl(known, \(k) grepl(k, conditionMessage(w), fixed = TRUE)))) {
      invokeRestart("muffleWarning")
    }
  })
}

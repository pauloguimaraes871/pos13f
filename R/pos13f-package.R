#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data .env :=
## usethis namespace: end

## crayon and tictoc are in Imports without being called here: factoRverse calls them at
## run time, even with verbose = FALSE, and lists them only in its Suggests

## mockable bindings: start
## mockable bindings: end

## Column names used bare inside dplyr::join_by(closest(...)) and join_by(between(...)),
## which accept no strings or .data pronoun (gold_as_of_columns() and the gold references)
utils::globalVariables(c(
  "info_month", "feature_month", "closest", "between", "report_quarter", "date", "as_of",
  "anchor_date", "x", "y"
))
NULL

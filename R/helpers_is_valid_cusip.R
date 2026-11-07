#' Check the format and check digit of CUSIP identifiers
#'
#' The 9th character of a CUSIP is a check digit computed from the first eight, so a
#' typo almost always breaks it. About 20% of the distinct CUSIP strings in the 13F
#' data sets fail this check (filler such as `000000001`, tickers padded with zeros,
#' typos), and OpenFIGI rejects them as "Invalid idValue format" (G1 follow-up,
#' 2026-11-07). They are 0.09% of share positions.
#'
#' @param x Character vector of upper-case candidate identifiers.
#'
#' @return Logical vector, `TRUE` where `x` has 9 characters (digits, upper-case
#'   letters, `*`, `@`, `#` in the first eight, a digit last) and a correct check digit.
#'
#' @details
#' Standard CUSIP modulus-10 "double add double": digits keep their value, letters A to
#' Z count 10 to 35, `*`, `@`, `#` count 36 to 38; the 2nd, 4th, 6th and 8th values are
#' doubled; the digits of every value are summed; the check digit is
#' `(10 - sum mod 10) mod 10`. CINS codes (first character a letter) follow the same
#' rule.
#' @noRd
is_valid_cusip <- function(x) {
  values <- c(stats::setNames(0:9, 0:9), stats::setNames(10:35, LETTERS),
    `*` = 36, `@` = 37, `#` = 38
  )
  purrr::map_lgl(x, function(s) {
    if (is.na(s) || !grepl("^[0-9A-Z*@#]{8}[0-9]$", s)) {
      return(FALSE)
    }
    chars <- strsplit(s, "")[[1]]
    v <- values[chars[1:8]]
    ## Double every second value (positions 2, 4, 6, 8)
    v <- v * c(1, 2, 1, 2, 1, 2, 1, 2)
    total <- sum(v %/% 10 + v %% 10)
    (10 - total %% 10) %% 10 == as.integer(chars[9])
  })
}

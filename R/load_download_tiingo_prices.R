#' Download the daily price history of one ticker from Tiingo
#'
#' Bronze loader: values as delivered by Tiingo, with typed columns. A ticker Tiingo does
#' not know is recorded as a row with `status = "404"`; one without prices as
#' `"empty"`. Any other HTTP error stops, because it points to a problem (for example a
#' bad key) that must not be recorded as data.
#'
#' @param ticker Character scalar, ticker in Tiingo's format (`BRK-B`).
#' @param start_date Character scalar, first date requested (`YYYY-MM-DD`).
#' @param api_key Character scalar, Tiingo API key (`TIINGO_API_KEY`).
#'
#' @return A tibble with `ticker`, `status` (`"200"`, `"404"` or `"empty"`), `date`
#'   (Date) and Tiingo's fields renamed to snake_case: `close`, `high`, `low`, `open`,
#'   `volume` (unadjusted), `adj_close`, `adj_high`, `adj_low`, `adj_open`, `adj_volume`,
#'   `div_cash`, `split_factor`. One row with `NA` prices when status is not `"200"`.
#'
#' @details
#' Requests are throttled to 2.5 per second (9,000 per hour, below the Power plan's
#' 10,000) and retried on rate-limit and server errors.
#' @export
download_tiingo_prices <- function(ticker, start_date = "2013-01-01",
                                   api_key = Sys.getenv("TIINGO_API_KEY")) {
  # Validate inputs
  if (!is.character(ticker) || length(ticker) != 1 || !nzchar(ticker)) {
    stop("ticker must be a single non-empty string.")
  }
  if (!is.character(api_key) || length(api_key) != 1 || !nzchar(api_key)) {
    stop("api_key must be set (TIINGO_API_KEY).")
  }

  # Request the full history from start_date
  resp <- httr2::request("https://api.tiingo.com/tiingo/daily") |>
    httr2::req_url_path_append(ticker, "prices") |>
    httr2::req_url_query(startDate = start_date) |>
    httr2::req_headers(Authorization = paste("Token", api_key)) |>
    httr2::req_throttle(rate = 2.5, realm = "tiingo") |>
    httr2::req_retry(max_tries = 5, retry_on_failure = TRUE) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()
  status <- httr2::resp_status(resp)
  if (status != 200 && status != 404) {
    stop("Tiingo returned HTTP ", status, " for ", ticker, ": check the key and limits.")
  }

  # Shape the reply
  no_prices <- function(code) tibble::tibble(ticker = ticker, status = code, date = as.Date(NA))
  if (status == 404) {
    return(no_prices("404"))
  }
  rows <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  if (length(rows) == 0) {
    return(no_prices("empty"))
  }
  tibble::as_tibble(rows) |>
    dplyr::transmute(
      ticker = .env$ticker, status = "200", date = as.Date(substr(.data$date, 1, 10)),
      close = .data$close, high = .data$high, low = .data$low, open = .data$open,
      volume = .data$volume, adj_close = .data$adjClose, adj_high = .data$adjHigh,
      adj_low = .data$adjLow, adj_open = .data$adjOpen, adj_volume = .data$adjVolume,
      div_cash = .data$divCash, split_factor = .data$splitFactor
    )
}

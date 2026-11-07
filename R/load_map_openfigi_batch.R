#' Map up to 100 CUSIPs to FIGIs with one OpenFIGI request
#'
#' Bronze loader: the reply is kept as delivered. A job that matched returns one row per
#' record; a job without a match returns one row whose `status` holds OpenFIGI's warning
#' (for example `"No identifier found."`) or error.
#'
#' @param jobs Data frame with up to 100 rows: `cusip` (character) and `id_type`
#'   (`"ID_CUSIP"` or `"ID_CINS"`), from `list_bronze_cusips()`.
#' @param api_key Character scalar, OpenFIGI API key (`OPENFIGI_API_KEY`).
#'
#' @return A tibble with `cusip`, `id_type`, `status` (`"found"`, the warning text, or
#'   `"error: <message>"`) and the record fields `figi`, `composite_figi`,
#'   `share_class_figi`, `ticker`, `name`, `exch_code`, `security_type`,
#'   `security_type2`, `market_sector`, `security_description` (`NA` when not found).
#'
#' @details
#' Every job asks for the US listing only (`exchCode = "US"`, otherwise one CUSIP returns
#' hundreds of listings worldwide) and includes delisted companies
#' (`includeUnlistedEquities = TRUE`, needed for survivorship). Requests are throttled to
#' 4 per second (the keyed limit is 250 per minute) and retried on transient errors.
#' @export
map_openfigi_batch <- function(jobs, api_key = Sys.getenv("OPENFIGI_API_KEY")) {
  # Validate inputs
  jobs_ok <- is.data.frame(jobs) && all(c("cusip", "id_type") %in% names(jobs)) &&
    nrow(jobs) >= 1 && nrow(jobs) <= 100
  if (!jobs_ok) {
    stop("jobs must be a data frame of 1 to 100 rows with cusip and id_type.")
  }
  if (!is.character(api_key) || length(api_key) != 1 || !nzchar(api_key)) {
    stop("api_key must be set (OPENFIGI_API_KEY).")
  }

  # One request carrying every job
  body <- purrr::map2(jobs$cusip, jobs$id_type, function(id, type) {
    list(idType = type, idValue = id, exchCode = "US", includeUnlistedEquities = TRUE)
  })
  replies <- httr2::request("https://api.openfigi.com/v3/mapping") |>
    httr2::req_headers(`X-OPENFIGI-APIKEY` = api_key) |>
    httr2::req_body_json(body) |>
    httr2::req_throttle(rate = 4, realm = "openfigi") |>
    httr2::req_retry(max_tries = 5, retry_on_failure = TRUE) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)

  # One row per record, or one row stating why nothing matched
  field <- function(x, name) if (is.null(x[[name]])) NA_character_ else as.character(x[[name]])
  purrr::pmap(list(jobs$cusip, jobs$id_type, replies), function(cusip, type, reply) {
    status <- if (!is.null(reply$error)) paste("error:", reply$error) else reply$warning
    records <- if (is.null(reply$data)) list(list()) else reply$data
    purrr::map(records, function(r) {
      tibble::tibble(
        cusip = cusip, id_type = type, status = if (is.null(reply$data)) status else "found",
        figi = field(r, "figi"), composite_figi = field(r, "compositeFIGI"),
        share_class_figi = field(r, "shareClassFIGI"), ticker = field(r, "ticker"),
        name = field(r, "name"), exch_code = field(r, "exchCode"),
        security_type = field(r, "securityType"), security_type2 = field(r, "securityType2"),
        market_sector = field(r, "marketSector"),
        security_description = field(r, "securityDescription")
      )
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()
}

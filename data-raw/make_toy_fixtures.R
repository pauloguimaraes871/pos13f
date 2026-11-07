# Build the toy fixture: a reproducible sample of real bronze SEC data ----
#
# Made-up fixtures test named cases; this sample adds the noise of real filings that is
# hard to imagine (odd CIK formats, empty optional fields, amendment chains, duplicated
# managers). Pattern of factoRverse: `toy_*` objects are real samples, `artificial_*`
# are made up.
#
# The sample keeps whole filer histories, so amendment chains stay intact:
#   random  25 filers drawn with a fixed seed among filers with at most 4,000 share
#           positions over the whole window (keeps the file small)
#   forced  for each edge case, the smallest filer that has it:
#           untyped amendment, NEW HOLDINGS, RESTATEMENT, a report filed late enough to
#           land in the SEC folder after the one holding its deadline, two filings of
#           one quarter usable on the same day, an amendment without an earlier
#           original, and an old-quarter amendment filed in the dollar regime
#
# Output: tests/testthat/testdata/toy_bronze_sec.rds, a list with the six SEC tables
# (each with `source_archive`), the trading calendar and the selected filers with the
# reason each was selected. Rerun only when the bronze layer changes:
#   Rscript data-raw/make_toy_fixtures.R

devtools::load_all(quiet = TRUE)
board <- init_pipeline_board(file.path("data", "prod"))
archives <- targets::tar_read(sec_archives_window)
filings <- pins::pin_read(board, "silver_filings")

# Share positions per filer, counted inside duckdb ----
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
on.exit(DBI::dbDisconnect(con), add = TRUE)
infotable_paths <- purrr::map_chr(paste0("bronze_sec_", archives$label), function(p) {
  files <- pins::pin_download(board, p)
  files[basename(files) == "INFOTABLE.parquet"]
})
infotable_sql <- paste0(
  "read_parquet([",
  paste0("'", normalizePath(infotable_paths, winslash = "/"), "'", collapse = ", "),
  "], filename = true)"
)
rows_per_accession <- DBI::dbGetQuery(con, paste(
  "SELECT ACCESSION_NUMBER AS accession_number, count(*) AS n_rows FROM", infotable_sql,
  "GROUP BY 1"
))
filer_rows <- filings |>
  dplyr::left_join(rows_per_accession,
    by = dplyr::join_by("accession_number"), relationship = "one-to-one"
  ) |>
  dplyr::summarise(n_rows = sum(.data$n_rows, na.rm = TRUE), .by = "cik")

# Edge cases, each on (cik, period_of_report) ----
## The folder holding the deadline (period end plus 45 days) of each report
deadline_folder <- function(period) {
  deadline <- period + 45
  purrr::map_chr(deadline, function(d) {
    hit <- archives$label[archives$window_start <= d & archives$window_end >= d]
    if (length(hit) == 1) hit else NA_character_
  })
}
by_quarter <- filings |>
  dplyr::left_join(rows_per_accession,
    by = dplyr::join_by("accession_number"), relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    ## A late report with positions: an empty one would not show where it lands
    late = !.data$is_amendment & .data$n_rows > 0 &
      .data$source_archive != deadline_folder(.data$period_of_report),
    dollar_amendment = .data$is_amendment & .data$value_regime == "dollars" &
      .data$period_of_report < as.Date("2022-12-31")
  ) |>
  dplyr::summarise(
    untyped = any(.data$is_amendment & is.na(.data$amendment_type)),
    new_holdings = any(.data$amendment_type %in% "NEW HOLDINGS"),
    restatement = any(.data$amendment_type %in% "RESTATEMENT"),
    late_report = any(.data$late, na.rm = TRUE),
    same_day = anyDuplicated(.data$usable_date) > 0,
    amendment_first = .data$is_amendment[which.min(.data$usable_date)],
    dollar_amendment = any(.data$dollar_amendment),
    .by = c("cik", "period_of_report")
  )
cases <- c(
  "untyped", "new_holdings", "restatement", "late_report", "same_day",
  "amendment_first", "dollar_amendment"
)
forced <- purrr::map(cases, function(case) {
  by_quarter |>
    dplyr::filter(.data[[case]]) |>
    dplyr::distinct(.data$cik) |>
    dplyr::inner_join(filer_rows, by = dplyr::join_by("cik"), relationship = "one-to-one") |>
    dplyr::filter(.data$n_rows > 0) |>
    dplyr::slice_min(.data$n_rows, n = 1, with_ties = FALSE) |>
    dplyr::mutate(reason = case)
}) |>
  purrr::list_rbind()

# Random filers ----
set.seed(20261107)
random <- filer_rows |>
  dplyr::filter(.data$n_rows > 0, .data$n_rows <= 4000, !.data$cik %in% forced$cik) |>
  dplyr::slice_sample(n = 25) |>
  dplyr::mutate(reason = "random")
selected <- dplyr::bind_rows(forced, random) |>
  dplyr::summarise(
    n_rows = dplyr::first(.data$n_rows), reason = paste(.data$reason, collapse = ", "),
    .by = "cik"
  ) |>
  dplyr::arrange(.data$cik)

# Extract every table for the selected filers' accessions ----
accessions <- filings$accession_number[filings$cik %in% selected$cik]
## Notice filings of these filers are kept too, so the sample still holds what silver drops
submission <- read_bronze_sec_table(board, "SUBMISSION", labels = archives$label)
notices <- submission$ACCESSION_NUMBER[trimws(submission$CIK) %in% selected$cik]
accessions <- union(accessions, notices)
keep <- function(df) dplyr::filter(df, .data$ACCESSION_NUMBER %in% accessions)
small_tables <- c("SUBMISSION", "COVERPAGE", "SUMMARYPAGE", "OTHERMANAGER", "OTHERMANAGER2")
toy <- purrr::map(stats::setNames(small_tables, small_tables), function(table) {
  keep(read_bronze_sec_table(board, table, labels = archives$label)) |>
    tibble::as_tibble() |>
    structure(inputs = NULL)
})
duckdb::duckdb_register(con, "toy_accessions", tibble::tibble(accession_number = accessions))
toy$INFOTABLE <- DBI::dbGetQuery(con, paste(
  "SELECT i.* EXCLUDE (filename),",
  "regexp_extract(filename, 'bronze_sec_([0-9a-z_]+)', 1) AS source_archive FROM",
  infotable_sql, "i JOIN toy_accessions a ON i.ACCESSION_NUMBER = a.accession_number",
  "ORDER BY i.ACCESSION_NUMBER, i.INFOTABLE_SK"
)) |>
  tibble::as_tibble()
toy$calendar <- read_trading_calendar(board)$date
toy$archives <- dplyr::select(archives, "label", "window_start", "window_end")
toy$selected <- selected

# Save ----
path <- file.path("tests", "testthat", "testdata", "toy_bronze_sec.rds")
dir.create(dirname(path), showWarnings = FALSE)
saveRDS(toy, path, compress = "xz")
message(
  "Saved ", path, ": ", nrow(selected), " filers, ", nrow(toy$SUBMISSION), " filings, ",
  nrow(toy$INFOTABLE), " positions, ", round(file.size(path) / 1e6, 2), " MB"
)
print(tibble::as_tibble(selected), n = Inf)

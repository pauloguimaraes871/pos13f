# G1 spike, step 1c: load the sample archives into a duckdb database and measure
#
# Throwaway spike script. Needs the zips from 02_download_sample.R.
# Output: data-raw/cache/spike/spike.duckdb and load_measurements.csv
#
# What duckdb is, in one paragraph:
#   duckdb is a database that lives in a single file on disk and runs inside R (no server
#   to install). We put the large 13F tables in that file instead of in R's memory. We
#   talk to it through the DBI package:
#     DBI::dbConnect()    opens (or creates) the database file
#     DBI::dbExecute()    sends an instruction that changes something (create a table,
#                         insert rows, change a setting); returns the number of rows touched
#     DBI::dbGetQuery()   sends a question and returns the answer as an R data frame
#     DBI::dbDisconnect() closes the file
#   Instructions are written in SQL, the standard language of databases. In package code
#   we will write dplyr instead and let dbplyr translate it to SQL; the spike uses SQL
#   directly because loading files is simpler that way.

# Setup
cache_dir <- "data-raw/cache"
db_path <- file.path(cache_dir, "spike", "spike.duckdb")
tmp_dir <- file.path(cache_dir, "duckdb_tmp")
unzip_dir <- file.path(cache_dir, "unzipped")
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

## Refuse to run on an existing database instead of deleting it: a rerun would otherwise
## load every archive twice. To rebuild, delete spike.duckdb by hand first.
if (file.exists(db_path)) {
  stop(
    "data-raw/cache/spike/spike.duckdb already exists. Delete it by hand to rebuild ",
    "it; otherwise every archive would be loaded twice."
  )
}
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = db_path)

## Three duckdb settings (plan Section 1, sized for an 8 to 16 GB machine)
### memory_limit = 3GB: duckdb may use at most 3 GB of RAM; beyond that it writes
###                     intermediate results to disk instead of crashing
### threads = 2:        use 2 CPU cores, leaving the rest for R and Windows
### temp_directory:     where those intermediate results go; on D:, which has free space
DBI::dbExecute(con, "SET memory_limit = '3GB'")
DBI::dbExecute(con, "SET threads = 2")
DBI::dbExecute(con, sprintf("SET temp_directory = '%s'", normalizePath(tmp_dir, "/")))

## The six tables we load (SIGNATURE is not needed by any part of the plan)
tables <- c(
  "SUBMISSION", "COVERPAGE", "SUMMARYPAGE", "OTHERMANAGER", "OTHERMANAGER2",
  "INFOTABLE"
)

## Peak memory of this R process so far, in MB. duckdb runs inside the R process, so this
## number covers both R and duckdb.
peak_mb <- function() ps::ps_memory_info()[["peak_wset"]] / 1e6

# Load one archive: unzip, copy each table into the database, delete the unzipped files
load_archive <- function(zip_file) {
  t0 <- Sys.time()
  out <- file.path(unzip_dir, sub("\\.zip$", "", zip_file))
  utils::unzip(file.path(cache_dir, "sec", zip_file), exdir = out)
  expanded_mb <- sum(file.size(list.files(out, full.names = TRUE))) / 1e6

  for (tbl in tables) {
    src <- normalizePath(file.path(out, paste0(tbl, ".tsv")), "/")
    ### The SQL below reads, in plain words:
    ###   "take every column (*) of the tab-separated file, add a column called archive
    ###    holding the zip name, and keep every value as text (all_varchar)".
    ### Text first, so no value is lost to a wrong type guess; types are set in Phase 2.
    select_sql <- sprintf(
      "SELECT '%s' AS archive, * FROM read_csv('%s', delim = '\t', header = true,
       all_varchar = true)", zip_file, src
    )
    ### First archive: CREATE TABLE makes the table from those rows.
    ### Later archives: INSERT INTO ... BY NAME appends the rows to the existing table,
    ### matching columns by name (new columns in recent years cannot shift the others).
    exists <- DBI::dbExistsTable(con, tbl)
    DBI::dbExecute(con, if (exists) {
      sprintf("INSERT INTO %s BY NAME %s", tbl, select_sql)
    } else {
      sprintf("CREATE TABLE %s AS %s", tbl, select_sql)
    })
  }

  ## Completeness check: rows in the database must equal data lines in the source file
  ## (minus the header line). A difference would mean rows were dropped while parsing.
  src_lines <- length(readLines(file.path(out, "INFOTABLE.tsv"), warn = FALSE)) - 1
  loaded <- DBI::dbGetQuery(con, sprintf(
    "SELECT count(*) AS n FROM INFOTABLE WHERE archive = '%s'", zip_file
  ))$n
  unlink(out, recursive = TRUE)

  tibble::tibble(
    archive = zip_file, expanded_mb = expanded_mb,
    seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    infotable_rows = loaded, source_lines = src_lines,
    process_peak_mb = peak_mb(), db_mb = file.size(db_path) / 1e6,
    tmp_mb = sum(file.size(list.files(tmp_dir, recursive = TRUE, full.names = TRUE))) / 1e6
  )
}

# Run the three loads and save the measurements
sample_files <- c(
  "2014q1_form13f.zip", "2023q1_form13f.zip",
  "01mar2025-31may2025_form13f.zip"
)
loads <- purrr::map(sample_files, load_archive) |> purrr::list_rbind()
print(loads, width = Inf)
readr::write_csv(loads, file.path(cache_dir, "spike", "load_measurements.csv"))

## CHECKPOINT writes everything still held in memory into the database file before closing
DBI::dbExecute(con, "CHECKPOINT")
DBI::dbDisconnect(con, shutdown = TRUE)
cat("final duckdb file MB:", round(file.size(db_path) / 1e6), "\n")

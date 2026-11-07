# G1 spike, step 1d: profile the loaded sample (no network)
#
# Throwaway spike script. Needs spike.duckdb from 03_load_duckdb.R.
# Each block asks the database one question and prints the answer. The SQL is explained
# above each query; dbGetQuery() returns the answer as an R data frame.

# Open the database read-only (this script must not change it)
db_path <- "data-raw/cache/spike/spike.duckdb"
if (!file.exists(db_path)) {
  stop(db_path, " does not exist. Run data-raw/spike/03_load_duckdb.R first.")
}
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = db_path,
                      read_only = TRUE)
q <- function(sql) tibble::as_tibble(DBI::dbGetQuery(con, sql))

## Dates were loaded as text such as 31-MAR-2025; this helper writes the SQL that turns
## a text column into a real date (strptime with day-month-year)
d <- function(col) sprintf("strptime(%s, '%%d-%%b-%%Y')::DATE", col)

# How many filings of each type per archive
## "GROUP BY ALL ... count(*)" means: count rows for each archive and submission type
cat("=== Submission types per archive ===\n")
print(q("SELECT archive, SUBMISSIONTYPE, count(*) AS n FROM SUBMISSION
         GROUP BY ALL ORDER BY archive, n DESC"), n = Inf)

# Which filing dates and report periods each archive covers
## Shows that archives are grouped by filing date and carry old periods (amendments,
## late filers)
cat("\n=== Filing date and period of report ranges ===\n")
print(q(sprintf("SELECT archive, min(%1$s) AS first_filed, max(%1$s) AS last_filed,
                 min(%2$s) AS min_period, max(%2$s) AS max_period
                 FROM SUBMISSION GROUP BY ALL ORDER BY archive",
                d("FILING_DATE"), d("PERIODOFREPORT"))), width = Inf)

## The six most frequent report periods in each archive
### QUALIFY row_number() ... <= 6 keeps the top 6 rows per archive
cat("\n=== Periods of report inside each archive (top 6) ===\n")
print(q(sprintf("SELECT * FROM (SELECT archive, %s AS period, count(*) AS n
                 FROM SUBMISSION GROUP BY archive, period)
                 QUALIFY row_number() OVER (PARTITION BY archive ORDER BY n DESC) <= 6
                 ORDER BY archive, n DESC",
                d("PERIODOFREPORT"))), n = Inf)

# Report types (holdings, notice, combination) and amendment types
cat("\n=== Report types and amendment types ===\n")
print(q("SELECT c.archive, c.REPORTTYPE, c.AMENDMENTTYPE, count(*) AS n
         FROM COVERPAGE c GROUP BY ALL ORDER BY c.archive, n DESC"), n = Inf)

# Confidential treatment: positions withheld, and later revealed
cat("\n=== Confidential treatment flags ===\n")
print(q("SELECT archive, ISCONFIDENTIALOMITTED, count(*) AS n FROM SUMMARYPAGE
         GROUP BY ALL ORDER BY archive"), n = Inf)
print(q("SELECT archive, CONFDENIEDEXPIRED, count(*) AS n FROM COVERPAGE
         GROUP BY ALL ORDER BY archive"), n = Inf)

# Holdings rows: shares vs bond principal, option rows, filer-supplied FIGI, CUSIP counts
## avg(condition::INT) is the share of rows where the condition is true
## count(DISTINCT CUSIP) FILTER (WHERE ...) counts distinct CUSIPs among share rows only
cat("\n=== INFOTABLE row types, FIGI fill rate, unique CUSIPs ===\n")
print(q("SELECT archive,
           count(*) AS rows,
           avg((SSHPRNAMTTYPE = 'SH')::INT) AS share_sh,
           avg((PUTCALL IS NOT NULL)::INT) AS share_putcall,
           avg((FIGI IS NOT NULL AND FIGI <> '')::INT) AS share_figi_filled,
           count(DISTINCT CUSIP) AS cusips_all,
           count(DISTINCT CUSIP) FILTER (WHERE SSHPRNAMTTYPE = 'SH' AND PUTCALL IS NULL)
             AS cusips_sh_noopt
         FROM INFOTABLE GROUP BY ALL ORDER BY archive"), width = Inf)

# Old-quarter filings made after the 2023-01-03 unit change
## JOIN ... USING (archive, ACCESSION_NUMBER) links each filing to its cover page
cat("\n=== Old-quarter amendments filed on or after 2023-01-03 ===\n")
print(q(sprintf("SELECT s.archive, s.SUBMISSIONTYPE, c.AMENDMENTTYPE, %s AS period,
                 count(*) AS n
                 FROM SUBMISSION s JOIN COVERPAGE c USING (archive, ACCESSION_NUMBER)
                 WHERE %s >= DATE '2023-01-03' AND %s < DATE '2022-12-31'
                 GROUP BY ALL ORDER BY period DESC, n DESC LIMIT 12",
                d("s.PERIODOFREPORT"), d("s.FILING_DATE"), d("s.PERIODOFREPORT"))),
      n = Inf)

# Distinct CUSIPs across all three archives (sizes the OpenFIGI workload)
cat("\n=== Union of CUSIPs across the three archives ===\n")
print(q("SELECT count(DISTINCT CUSIP) AS cusips_union FROM INFOTABLE
         WHERE SSHPRNAMTTYPE = 'SH' AND PUTCALL IS NULL"))

DBI::dbDisconnect(con, shutdown = TRUE)

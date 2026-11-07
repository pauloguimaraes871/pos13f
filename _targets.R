# Pipeline definition for pos13f ----
#
# Planned target groups (docs/IMPLEMENTATION_PLAN.md, Section 1), in data-flow order:
#   ingest    SEC 13F data set archives, streamed one at a time into duckdb on disk
#   clean     filings, filing_relations, holdings_state (amendment state machine,
#             filing-date value regime, overlap flags)
#   identity  security_master (CUSIP to composite FIGI via OpenFIGI), ticker_alias
#   prices    prices_d from Tiingo, terminal_events (baseline and stress policy)
#   universe  filer_activity proxy and the liquid equity universe per formation
#   signal    F1 breadth change on the matched filer cohort at each formation
#   backtest  cs weights, drifting long-short returns, cost and borrow scenarios
#   report    self-contained HTML report (inst/report) and memo inputs
#
# Profiles (plan Section 1 and 7):
#   full     real SEC, Tiingo and OpenFIGI data; needs the three keys in .Renviron
#   fixture  offline run on inst/extdata/fixture, no keys and no network, under a
#            minute; selected with Sys.setenv(TAR_PROJECT = "fixture") once added

# Load package code ----
## The package is developed in place, so the pipeline runs on the working tree
## rather than on an installed copy that could be stale
devtools::load_all(quiet = TRUE)

targets::tar_option_set(packages = character(0))

# Targets ----
list(
  ## Placeholder so targets::tar_make() runs during Phase 0
  targets::tar_target(
    package_version,
    as.character(base::getNamespaceVersion("pos13f"))
  )
)


# pos13f

<!-- badges: start -->
[![R-CMD-check](https://github.com/pauloguimaraes871/pos13f/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pauloguimaraes871/pos13f/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

`pos13f` is an R package plus a `targets` pipeline that ingests the SEC Form 13F
data sets, builds an institutional positioning factor with strict point-in-time
discipline, backtests it on liquid US equities with explicit trading costs, and
renders a self-contained HTML report and a short research memo.

## Research question

Does the change in disclosed institutional breadth (the number of active 13F filers
holding a stock), observed only after the filings are public, predict the
cross-section of returns of liquid US equities, net of trading costs, and is that
information distinct from price momentum? The hypothesis, definitions and decision
rules were fixed before any data was downloaded: see
[docs/factor_memo_draft.md](docs/factor_memo_draft.md).

## Quick start

```r
# 1. Restore the package library recorded in renv.lock
renv::restore()

# 2. Add the three keys to your user-level .Renviron (template: .Renviron.example),
#    then restart R
usethis::edit_r_environ()

# 3. Run the pipeline
targets::tar_make()
```

## Reproduction requirements

* R (4.5 or later) and the packages pinned in `renv.lock`; on Windows, Rtools.
* `SEC_USER_AGENT`: a name and contact email, as SEC EDGAR requires.
* `TIINGO_API_KEY` on a paid Tiingo plan (Power) for full daily price history.
* `OPENFIGI_API_KEY` (free) for CUSIP to FIGI mapping.
* Disk space and first-run time: measured in Phase 1, to be stated here.

## Offline functional check

A fixture profile that runs the whole pipeline on a tiny offline data set, with no
keys and no network, in under a minute. To be added.

## Repository layout

```
R/                     package functions, one per file
tests/testthat/        one test file per function; fixtures/ for test data
inst/extdata/fixture/  offline fixture (fabricated 13F rows, recorded API replies)
inst/report/           self-contained HTML report source
_targets.R             pipeline definition
memo/                  research memo
docs/                  implementation plan and pre-registered research design
data-raw/              spike scripts; cache/ holds downloaded data (git-ignored)
```

## Point-in-time conventions

Every filing enters the data only from its usable date (the first trading day after
its filing date), signals compare two consecutive report quarters that are both
public at formation, and screens use data up to the day before formation. The
binding definitions are in
[docs/IMPLEMENTATION_PLAN.md, Section 1.2](docs/IMPLEMENTATION_PLAN.md#12-timing-contract-single-source-of-truth).

## Status

Phase 0: package scaffold, tooling, CI and planning documents. No data or results
yet.

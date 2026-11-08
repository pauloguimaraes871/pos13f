
# pos13f

<!-- badges: start -->
[![R-CMD-check](https://github.com/pauloguimaraes871/pos13f/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pauloguimaraes871/pos13f/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/pauloguimaraes871/pos13f/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/pauloguimaraes871/pos13f/actions/workflows/test-coverage.yaml)
[![lint](https://github.com/pauloguimaraes871/pos13f/actions/workflows/lint.yaml/badge.svg)](https://github.com/pauloguimaraes871/pos13f/actions/workflows/lint.yaml)
<!-- badges: end -->

## Overview

`pos13f` is an R package and a `targets` pipeline that tests whether a change in
institutional ownership breadth, read from SEC Form 13F filings only after they are
public, predicts the cross-section of US stock returns. It ingests the SEC 13F data
sets, Tiingo daily prices and OpenFIGI identifiers, builds the signal with strict
point-in-time rules, backtests it on the 1,000 most liquid US stocks with the
`factoRverse` backtest engine, and renders two documents from the results:

* [report/report.html](report/report.html): the technical report (data engineering,
  software, design and every result), a self-contained HTML file.
* [memo/memo.pdf](memo/memo.pdf): the three-page research memo.

## Research question and result

Does the change in the number of 13F filers holding a stock (F1, the change in
breadth of ownership of Chen, Hong and Stein, 2002) predict returns of liquid US
stocks net of trading costs, and does it add to price momentum? The hypotheses,
definitions and decision rules were written before any return was computed:
[docs/factor_memo_draft.md](docs/factor_memo_draft.md).

Over 152 months (March 2014 to October 2026), the top-minus-bottom F1 quintile earns
4.5% a year gross. Its alpha against the Fama-French five factors and momentum (151
months, as the factor data end a month earlier) is 2.25% a year and not significant
(t = 1.06). After a 25 bps trading fee and 100 bps borrow it earns 0.71% a year, with a
95% interval of about -6.8% to 8.3%, and its monthly returns have a correlation of 0.86
with the momentum spread. The test has low power: a true alpha of about 6% a year would
be detected only four times out of five. The memo and the report give the full
assessment.

## Quick start

```r
# 1. Restore the package library recorded in renv.lock. factoRverse 0.9.1 (the
#    backtest engine) is installed from vendor/factoRverse_0.9.1.tar.gz, which the
#    lockfile records as a local source. One locked package, ParBayesianOptimization,
#    is still recorded from GitHub, so a restore without a package cache needs access
#    to github.com.
renv::restore()

# 2. Add the three keys to your user-level .Renviron (template: .Renviron.example),
#    then restart R
usethis::edit_r_environ()

# 3. Run the pipeline: downloads, data layers, backtests, report and memo
targets::tar_make()
```

To try the pipeline on a shorter sample first, set the demo profile before step 3:
add `POS13F_WINDOW_START=2024-01-01` to `.Renviron` (and restart R). Only SEC
archives filed from 2024 on are ingested and formations start at the end of May
2024. The demo report and memo are rendered from that sample, so their numbers differ
from the committed ones.

## Requirements

* R 4.5 or later, Rtools on Windows, and Quarto (the CLI) to render the report and
  memo. The memo is rendered to PDF with Typst, which ships with Quarto.
* The packages pinned in `renv.lock`. factoRverse 0.9.1 ships in `vendor/`: the
  author's [factoRverse](https://github.com/pauloguimaraes871/factoRverse) at upstream
  commit `8f31959`, with its signal-selection and machine-learning dependencies (brms,
  lme4, xgboost and others) moved to Suggests and its images and tests left out of the
  build. The portfolio backtest code is unchanged.
* `SEC_USER_AGENT`: a name and contact email, as SEC EDGAR requires.
* `TIINGO_API_KEY` on a paid Tiingo plan (Power) for the full daily price history of
  live and delisted tickers.
* `OPENFIGI_API_KEY` (free) for the CUSIP to FIGI mapping.

Keys are read from the environment and never printed or written to logs.

**Time and disk.** Measured on the author's Windows laptop:

| Run | Time | Pins (`data/`) | Download cache (`data-raw/cache/`) |
|---|---|---|---|
| Demo profile, clean clone | about 1 h 50 min, of which 1 h 15 min is the Tiingo download | 3.2 GB | 1.8 GB |
| Full sample, after downloads | about 55 min (backtests 23 min, gold 10 min, silver holdings 8 min) | 14 GB with pin history | 9.9 GB |

Tiingo prices are downloaded per ticker (about 9,100 tickers) into
`data-raw/cache/`, so an interrupted run resumes where it stopped. The full-sample
SEC download adds the 2013 to 2023 archives to the demo's.

## How the pipeline is organised

Data moves through versioned `pins` on a local board in `data/prod`:

* **Bronze**: vendor data as delivered (SEC archives, Tiingo prices, OpenFIGI replies,
  Fama-French factors).
* **Silver**: point-in-time tables: filings with their usable date, versioned holdings,
  the security master, prices, returns and liquidity, plus an audit of filing dates
  against EDGAR.
* **Gold**: at each formation, the filer cohort, the liquid universe, F1 and its
  controls, as monthly panels in factoRverse's layout.
* **Backtests**: quintile portfolios under three weighting schemes, long-short spreads
  under cost and borrow scenarios, the evaluation tables, and every factoRverse cohort
  object.

Each step runs its data-quality checks and pins its tables only if all checks pass;
the first target runs the unit tests, and no SEC, OpenFIGI or Tiingo download or later
layer runs while one fails (only the SEC archive listing and the Fama-French download do
not wait for them).
`targets::tar_visnetwork()` shows the graph.

## Point-in-time rules

* A filing is usable from the first trading day after its filing date. A random
  sample of 300 filings checked against EDGAR's acceptance times confirms that every
  one was public before the close of its usable date, when portfolios trade.
* Amended reports are versioned: at date t only the versions filed before t count, so
  a later amendment never changes what was known at t.
* Portfolios are formed at the close of the last trading day of February, May, August
  and November, about two weeks after the 45-day filing deadlines. A report filed late
  enters at the first formation after it is public.
* Prices, volumes and every screen use data up to the day before formation.

The binding definitions are in
[docs/IMPLEMENTATION_PLAN.md, Section 1.2](docs/IMPLEMENTATION_PLAN.md#12-timing-contract-single-source-of-truth).

## Inspecting results

Every table is a pin, and every factoRverse backtest cohort is pinned whole, so it can
be explored with factoRverse's own methods:

```r
devtools::load_all()   # the package is run in place, not installed
board <- init_pipeline_board("data/prod")
cohort <- pins::pin_read(board, "backtests_cohort_f1_breadth_change_cs_unconditional")
plot(cohort, plot_id = "Time-Series of Raw Returns")
```

`targets::tar_read(report_tables)` returns every table the report and memo show.

## Repository layout

```
R/                package functions, one per file
tests/testthat/   one test file per function; fixtures/ holds real-data samples
_targets.R        pipeline definition
report/           technical report (report.qmd, rendered to self-contained HTML)
memo/             research memo (memo.qmd, rendered to PDF)
docs/             implementation plan and pre-registered research design
vendor/           factoRverse 0.9.1 source package
data-raw/         scripts that build the test fixtures; cache/ holds downloads (git-ignored)
data/             pins board (git-ignored)
logs/             data-quality test logs (git-ignored)
```

## Development

`devtools::test()` runs the unit tests and `lintr::lint_package()` the style checks.
On every pull request and every push to main, GitHub Actions runs three workflows on a
clean Ubuntu machine, with no keys or data: `R CMD check` with the unit tests, test
coverage with `covr` (the percentage is in each run's summary), and `lintr`, which
fails on any lint.

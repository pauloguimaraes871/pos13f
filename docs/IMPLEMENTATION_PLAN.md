# 13F positioning factor: implementation plan

Companion to `factor_memo_draft.md`, which holds the hypothesis, definitions and
decision rules. This plan covers how the study is built and tested.

## 0. Principles

- One hypothesis (F1) and one pre-registered portfolio construction (`cs`); other
  weighting schemes are robustness checks, other signals are separate trials.
- A complete deliverable first (real 13F data through F1, backtest, costs,
  diagnostics, report, memo, fresh-clone run), improvements after.
- Test the data dependencies that can break the study early: a feasibility spike (G1)
  measured archive sizes, value units, identity coverage and delisted tickers before
  full ingestion.
- Every layer is tested before it is stored.

## 1. Architecture

An R package (`pos13f`) plus a `targets` pipeline. Functions live one per file in `R/`,
named by role: `load_*` and `download_*` for data sources, `setup_*` and `deploy_*` for
pipeline steps, `transform_*` and `helpers_*` for tested building blocks, `tests_*` for
the data-quality suites run inside each deploy, `report_*` for the tables of the report
and memo.

### 1.0 Layers and storage

| Layer | Contents |
|---|---|
| Bronze | Source data as delivered: SEC 13F archives (one pin per archive), OpenFIGI replies, Tiingo prices, Fama-French factors |
| Silver | Point-in-time tables: filings with usable date, amendment versions of holdings, security master, prices and returns, terminal events, EDGAR filing-date audit |
| Gold | Per formation: filer cohort, liquid universe, F1 and controls, monthly panels in factoRverse's layout |
| Backtests | Quintile ladders under three weighting schemes, spreads under cost and borrow scenarios, evaluation tables, factoRverse cohort objects |
| Reports | Report and memo inputs, rendered documents |

Each step builds from unit-tested functions, runs its data-quality suite through
`run_logged_test()` (log in `logs/tests/`), stops if any check fails, and otherwise
writes versioned `pins` to a local board (`data/prod/`). Each pin's metadata records
the versions of the pins it was built from. Large tables are parquet files processed
in `duckdb` with a memory limit; only aggregates are loaded into R. Keys are read from
environment variables and never printed.

Bronze keeps vendor data as delivered and counts known defects in the pin metadata;
silver applies the strict checks. Tiingo prices are requested for US common stocks plus
SPY; ETFs, ADRs and funds are out of scope, matching the stock universe behind the
Fama-French factors. The sample window is set in `_targets.R`.

### 1.1 Keys

The internal instrument key is the OpenFIGI composite FIGI (`instrument_id`), used in
every table after the security master. Tickers only retrieve prices; they are never
join keys. Accession numbers are kept through every derived table.

### 1.2 Timing contract (single source of truth)

- `usable_date` = first trading day after `FILING_DATE`.
- **Formation date t** = last trading day of February, May, August and November.
- **Signal at t** compares two consecutive report quarters (q-1, q): q is the quarter
  whose deadline fell in the formation month (Q4 for end-February, Q1 for end-May, Q2
  for end-August, Q3 for end-November). A filer enters the cohort only if both its q-1
  and q reports have `usable_date` <= t.
- Screens and weights use prices and volumes up to t-1. Execution at the close of t.
  Portfolio returns start at t+1.
- Amendments and positions revealed after confidential treatment enter only from their
  own `usable_date`; they never backfill an earlier formation.
- A report filed after the deadline enters at the first formation after its own
  `usable_date`; until then its filer is outside the cohort.

A 13F filing is public on EDGAR as soon as it is accepted, and a submission accepted
after 5:30 pm Eastern is dated the next business day (Regulation S-T Rule 13(a)(2)).
The pipeline audits this assumption: it reads the EDGAR index page of a random sample
of filings and stops if any was accepted after the close of its usable date. The bulk
data sets are a structured copy of the filings, so the backtest assumes an investor who
reads EDGAR as filings are accepted. A live version would read the filings from EDGAR
directly, since the bulk files are posted weeks later.

### 1.3 Amendment versions

Per filer and report quarter, filings are applied in order of usable date:
- the original 13F-HR sets the state;
- a RESTATEMENT replaces the whole state;
- a NEW HOLDINGS amendment adds its positions (a position already reported is summed);
- an amendment that declares neither type is ignored and counted;
- among filings with the same filing date, the higher accession number wins.

Each resulting version is valid from its usable date until the next one, so a reader at
date t sees exactly the versions with `valid_from <= t < valid_to`.

### 1.4 Manager overlap

A stock counts at most once per filer and quarter (rows summed within a filing). If a
filer is listed as included in another filer's report and also files its own report,
the smaller of the two reports is dropped from the cohort for that formation, with the
links read from the report versions current at t, for both quarters compared.

### 1.5 Value units

`VALUE` was reported in thousands of dollars before 2023-01-03 and in dollars afterwards;
the regime is set by the filing date, so an old quarter amended after the change is read
in dollars. The unit check acts on value fields only and never removes a holding. F1
uses holder counts, so a residual unit error cannot move the signal.

### 1.6 Identity and delisting

- CUSIP to composite FIGI through OpenFIGI (including unlisted and delisted equities).
  Different CUSIPs are the same instrument only if OpenFIGI returns the same composite
  FIGI; a CUSIP that maps to two composite FIGIs is left out.
- Identity is accepted at each formation only if most quarter-q cohort holders report a
  value per share between 0.5 and 2 times Tiingo's raw close at quarter end, read from
  the holdings versions current at t.
- OpenFIGI and Tiingo are queried once, on the download date. Coverage of 13F holding
  value, measured when the security master was built: 71.7% priced; 24.2% not US common
  stock or not mapped (ETFs, ADRs, funds: out of scope); 3.7% common stock without a
  Tiingo price series; 0.45% removed by the identity checks (0.34% on tickers later
  reused by another company, 0.11% where holders' prices disagree with Tiingo).
- Adjusted prices are total-return series, checked on dividend and split days.
- Terminal returns: Tiingo gives no delisting return. Baseline: the position is closed
  at the last price. Stress: -30% in the month of the last price.

### 1.7 Portfolio accounting and costs

- Dollar-neutral long-short: the long and short legs each hold one dollar at every
  rebalance. Returns are excess long-short returns.
- Legs: top and bottom quintile of F1 within the liquid universe; every portfolio must
  hold at least 20 stocks.
- Weights drift between quarterly rebalances; trading happens only at formation dates.
- Costs: 10, 25 and 50 bps per dollar traded, the break-even cost, and a borrow fee of
  0 or 100 bps a year on the short leg.
- Benchmarks: same-universe equal weight and SPY total return; FF5 + UMD for alpha.

### 1.8 Universe

At each formation, in order, each excluded stock recording the first screen it fails:
a Tiingo price source; identity accepted at t; not a suspected CUSIP change; traded at
t; previous close at least USD 5; traded on at least 90% of the previous 63 trading
days; 12 months of history; all features computable; top 1,000 by previous 63-day
median dollar volume; a cohort holder in both q-1 and q.

### 1.9 Filer activity proxy

Per formation, from reports usable at t: number of positions and position turnover
(share of positions held in only one of the two quarters). A filer is index-like if it
is in the top third by positions and the bottom third by turnover; index-like filers and
filers with fewer than 10 positions are excluded. Thirds are recomputed every
formation.

### 1.10 Gold panels

- Panels in factoRverse's `meta_dataframe` layout (`id`, `tickers`, `dates`, then
  numeric columns): signals (F1, holders, momentum 12-1, reversal, volatility,
  illiquidity), forward returns (baseline and stress), liquidity and volatility.
- `dates` is the first day of the holding month: a row dated 2020-09-01 carries what was
  known at the close of 2020-08-31, and its forward return is September's. The
  formation's universe and signal are frozen for its three holding months.
- The gold suite recomputes every universe exclusion reason by a second method, re-reads
  cohort eligibility from the filings, and recomputes a stratified random sample of
  panel rows from silver in `dplyr`, which must match the `duckdb` results.

### 1.11 Backtests

- Every quintile of a signal runs as a factoRverse `run_port_backtest()`, under `cs`,
  `ew` and `rp`, plus the dependent momentum double sort under `cs`. A new signal is a
  new column of the gold signals panel, not new backtest code.
- `rp` uses the sample covariance of the previous 240 daily returns, cut at the
  formation date.
- Evaluation: Newey-West inference at lags 3 and 6, FF5 + UMD and CAPM alphas, rank IC
  per formation, an intersection-union monotonicity test, break-even fee and minimum
  detectable effect.
- The backtest suite checks that net returns equal raw returns minus costs, that trading
  happens only at rebalances, that each portfolio sorts on its signal, and that every
  portfolio holds at least 20 stocks.

## 2. Testing

- Unit tests for every function, with fixtures sampled from real data (uneven panels,
  amendment chains, late reports, overlapping managers).
- Scenario tests: amendment sequences (original, new holdings, restatement, same-day
  filings, an old-quarter amendment after 2023-01-03); a late report entering its own
  quarter from its own usable date; a CUSIP change; a filer missing quarter q.
- Invariants: no filing with `usable_date` > t enters a signal at t; trades only at
  formation dates; returns start at t+1.
- Each data-quality suite has tests that break its input on purpose and expect it to
  fail.

## 3. Extensions

Done after the main study: `rp` weighting; Newey-West lag sensitivity; CAPM alphas;
leave-one-year-out stability; turnover by leg through time; continuous integration with
coverage and lint workflows.

Next, each additional signal a separate trial counted in a deflated Sharpe ratio:
1. F1b (ownership change by value) and F1c (net new holders divided by the stock's own
   holders at q-1), which remove F1's size tilt.
2. F1 within past winners, as a conditional strategy.
3. Costs: square-root market impact by strategy size, realistic fees and borrow, and F1
   smoothed over two quarters.
4. Returns in months 1 to 12 after formation; an activity-based filer split; a
   capacity-based universe.

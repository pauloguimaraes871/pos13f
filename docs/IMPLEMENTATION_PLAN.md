# 13F positioning factor: implementation plan (v0.3)

v0.3 (2026-11-07) adopts the owner's medallion layout (bronze, silver, gold, backtests,
reports), versioned `pins` storage and prefixed file names (Section 1). v0.2 integrated
the Codex plan review (15 findings; resolution log in Section 8). G1 decisions D1 to D6
are in `data-raw/spike/G1_report.md` and override Sections 1.5 and 1.6 where they differ.
Clock: hard stop **Mon 08:00**. Companion: `factor_memo_draft.md` v0.2
(hypothesis, definitions, decision rules). This plan covers how and when.

## 0. Principles

- **CP4 is a submission, not an analysis milestone.** By Sunday 12:30 the repo holds
  a complete, compliant deliverable: real 13F data through F1, one portfolio rule,
  a benchmark, costs, diagnostics, a self-contained HTML report, a 3-page memo, and a
  verified fresh-clone run. Everything after CP4 improves a submission that already
  exists.
- One hypothesis (F1), one weighting method (`cs`) in the MVP. F2 to F5, F3 skill
  ranking, Fama-MacBeth, XBRL market cap and capacity claims are **deferred**, listed
  in the memo as considered and not tested.
- Fail early on the data dependencies that can kill the project (identity, delisting,
  resources): they are tested Friday, before full ingestion.
- Every checkpoint ends with tests green, `targets` pipeline run, a commit.

---

## 1. Architecture

R package `pos13f` plus `targets` pipeline, organised like the owner's `pipeline`
repository: a medallion of deploy layers, each one tested before it is pinned.

```
R/                      one function per file, file name = <group>_<function_name>.R
  setup_*               board initialisation
  load_*                downloads and readers of external sources (SEC, OpenFIGI, Tiingo)
  helpers_*             small reusable steps (pins, logging, transformations)
  tests_*               run_test_<layer>(): data-quality suites run inside deploys
  deploy_*              deploy_bronze(), deploy_silver(), deploy_gold(),
                        deploy_backtests(), deploy_reports()
  report_*              report helpers
tests/testthat/         unit tests, one file per function, realistic fixtures (TDD)
inst/extdata/fixture/   tiny offline fixture (fabricated 13F rows, recorded API replies)
_targets.R              full pipeline; window setting gives the demo profile
inst/report/report.qmd  self-contained HTML (embed-resources: true)
memo/memo.qmd           3 to 5 page memo
docs/                   plan, memo draft, brief
```

R packages cannot use sub-folders of `R/`, so the file-name prefix plays the role of
`pipeline`'s folders.

### 1.0 Medallion layers, storage and versioning

| Layer | Deploy | Contents |
|---|---|---|
| Bronze | `deploy_bronze()` | Source data as delivered, with expected columns and classes: the SEC 13F archives (one pin per archive, six tables), OpenFIGI replies, Tiingo prices, trading calendar, Fama-French factors |
| Silver | `deploy_silver()` | Fixes and correctness: filings with usable date, relations, summed holdings, amendment state, identity acceptance (D2, D6), unit corrections (D1), clean prices, terminal events |
| Gold | `deploy_gold()` | Analysis inputs: activity proxy, universe per formation (liquidity screen switchable), cohort, F1 with the zero-holder guard (D5), monthly returns, past returns |
| Backtests | `deploy_backtests()` | `cs` weights, drifting long-short returns, cost and borrow scenarios, double sort, FF5 + UMD alphas, rank IC |
| Reports | `deploy_reports()` | Analysis tables, HTML report, memo inputs |

Every deploy follows the same shape: build from unit-tested helpers, run
`run_test_<layer>()` through `run_logged_test()` (log in `logs/tests/`), stop if any
check fails, otherwise pin with `save_local_pin()`.

Storage: one versioned `pins::board_folder()` (`data/prod/`, git-ignored). Pin names
carry the layer as prefix (`bronze_sec_2014q1`, `silver_holdings`, `gold_signal`).
Tables are stored as **parquet**. A rerun with identical content creates no version; a
change adds a version folder, older versions stay (time travel with
`pins::pin_versions()` and `pins::pin_download(version = )`). Each pin's metadata
records the exact versions of the pins it was built from (lineage, the per-pin
equivalent of `pipeline`'s manifest).

Lineage on identical content (Codex review B04, resolved 2026-11-07): `pins::pin_write()`
skips a write when the content is identical; `save_local_pin()` forces a new version
when the input versions differ from the current pin's, so lineage stays true.

Price universe (owner decision 2026-11-07): Tiingo prices are requested for US listings
whose OpenFIGI `securityType` is `Common Stock`, plus SPY. REITs, ADRs and ETFs are out
of scope, matching the CRSP share codes 10 and 11 behind the Fama-French factors.
Foreign-incorporated common stocks (CINS identifiers) can be excluded in gold.

Data-quality gradient (owner decision 2026-11-07): bronze keeps vendor data as delivered
and tolerates small known defects, counted in the pin metadata; silver applies the strict
checks after filtering. First case: Tiingo `adj_close` of 0 with a positive `close`
(9,680 of 28.0 million rows, 13 OTC tickers; the adjustment rounds tiny prices to 0).
`run_test_bronze_tiingo()` allows at most 0.1% such rows; silver drops rows with a
non-positive adjusted close before computing returns, then applies the liquidity screen.

New SEC batch (owner decision 2026-11-07): `window_end` is set by the user in `_targets.R`
(2026-08-31, the last archive), never inferred. Tiingo restates adjusted prices after
splits and dividends, so the price cache cannot be extended with new dates.
The Tiingo cache records its `window_end` before its first download, and the manifest
(`data/manifest/window_end.txt`) records it when the bronze layer concludes.
`deploy_bronze_tiingo()` stops when a run's `window_end` is later than the cache's
window (the manifest's, for a cache that predates its own record). Redownloading is
started only by the user, by deleting the cached price files (about two hours); there is
no force argument (owner decision 2026-11-07, after Codex review). Later, the manifest
grows to record the metadata of every target, as in `pipeline`.

Compute: `duckdb` runs **in memory** inside a deploy, reading the parquet files of the
input pins (`read_parquet()` on the paths returned by `pins::pin_download()`), with
`dplyr` verbs through `dbplyr`. Large results are written by duckdb to parquet and
versioned with `pins::pin_upload()`; small results are collected and pinned with
`pins::pin_write()`. There is no duckdb database file. Settings: `memory_limit` about
3 GB, `threads` 2, temp directory on the data disk. Secrets from environment variables
only.

### 1.1 Keys and table contracts

**Internal instrument key: `instrument_id` = OpenFIGI composite FIGI.** Used in every
table after the security master. Tickers are retrieval attributes stored in a dated
alias table; they are never join keys. Accession provenance is kept through every
derived table.

| Table | Key | Main columns |
|---|---|---|
| `filings` | accession_number | cik, period_of_report, filing_date, usable_date, submission_type, report_type, amendment_type, amendment_no, value_regime (thousands, dollars) |
| `filing_relations` | accession_number, related_cik, relation | relation in (reports_for_me: OTHERMANAGER, included_in_report: OTHERMANAGER2) |
| `holdings_state` | cik, period_of_report, cusip, valid_from, valid_to | shares, value_usd, value_raw, value_multiplier, row_status, source_accessions |
| `security_master` | cusip | instrument_id (composite FIGI), share_class_figi, security_type, exch_code, map_status, evidence (implied-price check result) |
| `ticker_alias` | instrument_id, ticker, first_date, last_date | source (OpenFIGI, Tiingo meta) |
| `prices_d` | instrument_id, date | close_unadj, adj_close, volume, split_factor, div_cash |
| `terminal_events` | instrument_id | last_price_date, terminal_policy, stress_return |
| `filer_activity` | cik, formation_date | n_positions, position_turnover, activity_flag (computed from data usable at formation only) |
| `signal` | instrument_id, formation_date | n_holders_prev, n_holders_curr, breadth_change, cohort_size, report_quarters |
| `weights` | instrument_id, formation_date | side, target_weight |
| `bt_returns_m` | month_end | long, short, long_short_gross, turnover, cost_scenario, net |

### 1.2 Timing contract (single source of truth)

- `usable_date` = first trading day after `FILING_DATE` (the data set has no time
  stamp).
- **Formation date t** = last trading day of Feb, May, Aug, Nov.
- **Signal at t** compares two **common consecutive report quarters** (q-1, q): q is
  the quarter whose deadline fell in the formation month (Q4 for end-Feb, Q1 for
  end-May, Q2 for end-Aug, Q3 for end-Nov). A filer enters the cohort only if both
  its q-1 and q reports (with any amendments) have `usable_date` <= t.
- Screens and weights use prices and volumes up to t-1. Execution at close of t.
  Portfolio returns start at t+1.
- Amendments and confidential disclosures enter only from their own `usable_date`;
  they never backfill an earlier formation.

**Why `FILING_DATE` is the information date** (verified 2026-11-07 against SEC
documents; owner request):
- A 13F filing is public on EDGAR as soon as it is accepted: SEC's dissemination
  service sends public submissions to subscribers immediately, usually within two
  minutes of receipt (EDGAR PDS new-subscriber guide; PDS dissemination specification).
- Regulation S-T Rule 13(a)(2) (17 CFR 232.13): a submission made after 5:30 pm
  Eastern is deemed filed the next business day (the 2023 extension to 10 pm, Release
  33-11159, covers Forms 144, 3, 4 and 5 only, not 13F). A filing is therefore always
  public by the end of its `FILING_DATE`, at most about one business day earlier.
  `usable_date` (next trading day) is never early; it is late by a few hours to about
  one trading day.
- The bulk Form 13F data sets used here are posted after their filing window closes
  (the 01jun2026_31aug2026 archive carries Last-Modified 2026-09-14). They contain the
  filings "as filed", amendments included as separate submissions with their own
  `FILING_DATE` (SEC Form 13F data sets readme, sections 1 and 3). The bulk files are a
  structured copy; they add no information that was not public on each filing date.
- The backtest therefore assumes an investor who reads EDGAR filings as they are
  accepted, not one who waits for the bulk posting. A filing late against the 45-day
  deadline enters on its own date, never at the deadline.
- **Live use**: a real-time version of this pipeline cannot rely on the bulk data sets,
  which arrive weeks after the filings. It must fetch individual 13F-HR and 13F-HR/A
  filings from EDGAR as they are accepted (daily index or full-text feed), parse their
  XML information tables, and switch to the bulk files only to reconcile once a window
  is posted. Out of scope this weekend; the backtest is unaffected because it dates each
  filing by `FILING_DATE`.

### 1.3 Amendment state machine

Per (cik, period_of_report), filings applied in order of (usable_date,
accession_number):
- original 13F-HR: sets the state;
- `RESTATEMENT`: replaces the whole state;
- `NEW HOLDINGS`: appends rows (a CUSIP already present is summed only if the
  amendment is flagged as additional; otherwise logged as a conflict);
- same-day ambiguity: higher accession number wins, logged.
The state as of t is what the signal reads. Tested with sequences, not single
amendments (Section 4).

Implemented rules (owner decisions 2026-11-07, `silver_holdings_state`; counts on the
full window):
- Amendment without a type (53 of 15,727): ignored and counted; the original stands.
  Read as a restatement a partial list would erase the original, read as new holdings a
  full list would double it.
- `NEW HOLDINGS` repeating a CUSIP already reported: shares and value are summed (the
  data carry no "additional" flag); `n_sources` = 2 marks the 488,942 summed rows.
  This supersedes the conflict log above.
- An original usable after a `NEW HOLDINGS` amendment of the same quarter (468
  amendments have no earlier original): the original replaces the state, so the earlier
  added positions are dropped.
- Same-day filings (1,941 quarters): the higher accession number wins and only the last
  state of that day is visible. Accession numbers of different filing agents are not
  chronological, so this is a tie-break, not an ordering by time.
- A version without positions (empty report) has no rows: the filer reports no share
  position from its `valid_from`.
- The quarter comes from `period_of_report` only; `source_archive` is lineage, never a
  grouping key. A report filed late enters its own quarter from its own `usable_date`.
- Overlapping reports (Section 1.4) are applied in gold, at cohort construction.

### 1.4 Manager overlap policy (MVP, conservative and documented)

- Counting unit: the filing CIK. A stock counts at most once per CIK per quarter
  (rows summed over discretion and other-manager splits within an accession).
- Notice reports (13F-NT) carry no holdings and are dropped.
- If a CIK is listed in another filer's `OTHERMANAGER2` (included in that report)
  **and** files its own holdings report for the same quarter, the pair is flagged as
  overlapping; MVP rule: keep the larger report, drop the other from the cohort for
  that quarter. The count and value affected are reported; a sensitivity drops both.
  Implemented (2026-11-08) for both compared quarters, with links read only from the
  filings of each report version current at t (`find_overlap_drops()`); the
  sensitivity is not implemented.

### 1.5 Value units

- Regime set by **filing date** (submission on or after 2023-01-03: dollars; before:
  thousands), not by report quarter or archive label. An old-quarter amendment filed
  after the change is in the dollar regime.
- Keep `value_raw`, `value_multiplier`, `row_status`.
- Validation: implied price (value / shares) vs. Tiingo unadjusted close at period
  end. Rows with ratio near 1000 or 0.001 are quarantined individually; a filing is
  quarantined only if more than 50% of its matched rows fail. Counts reported.
- F1 itself uses holder **counts**, so a residual unit error cannot move the signal;
  units matter for activity measures, validation and diagnostics.

### 1.6 Identity and delisting policy

- CUSIP to `instrument_id` through OpenFIGI v3 (`POST /v3/mapping`, `ID_CUSIP`,
  header `X-OPENFIGI-APIKEY`, 100 jobs per request, 250 requests per minute,
  `includeUnlistedEquities: true` or delisted names are not returned).
- Different CUSIPs are treated as the same instrument **only** if OpenFIGI returns the
  same composite FIGI. No other chaining. A CUSIP change creating a new instrument is
  an identity break: that stock leaves the signal for that formation (flagged, counted).
- The implied-price check is a diagnostic stored as evidence, not proof of identity.
- **Terminal returns**: Tiingo gives no delisting return. Baseline: the position is
  closed at the last available price (return 0 afterwards). Stress: -30% applied on
  the last day to every unexplained termination (Shumway 1997 [mem] order of
  magnitude). Both reported, by portfolio side.
- Missingness reported per quarter by security count, by value, by early/late years
  and by portfolio side. If coverage is poor, the study is labelled as conditional on
  observable securities and no investability claim is made.

Implemented identity (silver, 2026-11-07; shares of 13F holding value, latest version
of each quarter, all quarters):
- `silver_security_master` has one row per CUSIP: 17,598 CUSIPs on 17,566 composite
  FIGIs (32 FIGIs carry two CUSIPs). A CUSIP that OpenFIGI maps to two composite FIGIs
  (176) is left out entirely: no rule picks the right one.
- **Snapshot look-ahead.** OpenFIGI and Tiingo are queried once, today. Each FIGI has one
  ticker, its current or last one (no ticker history), and Tiingo serves a ticker's
  current owner. A dead company whose ticker was later reused (106 tickers shared by 213
  instruments, for example ABAT) is therefore unpriceable because of an event after its
  death; so are instruments whose filed prices disagree with Tiingo (1,588 identity
  suspects). Excluding them selects the sample with later information, a survivorship
  effect concentrated in dead firms: 0.34% (shared tickers) plus 0.11% (suspects) of
  value. Not fixed (a point-in-time ticker history, for example Tiingo's permaTicker,
  would fix it); reported per quarter and portfolio side with the other missingness.
- Coverage of 13F value: 71.7% priced; 24.2% not US common stock or not mapped (ETFs,
  ADRs, funds: out of scope); 3.7% common stock without a Tiingo price source; 0.34%
  on shared tickers.
- The implied-price check (`silver_identity_checks`) is stored per CUSIP and quarter
  from the latest version of each quarter. Gold applies it per quarter as the D2
  acceptance condition, never the whole-history suspect count (which would itself look
  ahead). The latest version can include amendments filed after a formation; gold
  documents this or recomputes from the versions usable at t.
- Adjusted prices are total-return series: on 99.9% of dividend and split days the
  adjusted return equals (close + dividend) / (previous close / split factor) - 1,
  checked by the silver prices suite.

### 1.7 Portfolio accounting and costs (MVP)

- Dollar-neutral long-short: long leg +1, short leg -1 of capital (gross 2).
  Returns are excess long-short returns; financing ignored and stated.
- Legs: top and bottom quintile of F1 within the liquid universe; ties broken by
  `instrument_id`; a leg with fewer than 20 names makes that formation empty (flat).
- Weights: `cs` (signal score x trailing liquidity score, normalised per leg).
- **Weights drift** (buy and hold) between quarterly trades; trades and turnover occur
  only at formation dates.
- Costs: scenario-based, one-way cost per dollar traded of 10, 25 and 50 bps, applied
  to each side traded; plus break-even cost. Borrow fee sensitivity: 0 and 100 bps per
  year on the short leg, reported separately from trading costs.
- Benchmarks: same-universe equal weight (long-only comparison) and SPY total return
  (broad market), plus FF5 + UMD for alpha. Cap-weighted benchmark deferred.

### 1.8 Universe (MVP)

At each formation, from data up to t-1: US common stock with a valid
`instrument_id`; price >= USD 5; at least 12 months of price history; trailing
63-day median dollar volume in the top 1000. Described as "liquid US equities", not
large/mid cap.

### 1.9 Filer activity proxy (not a Bushee replication)

Per formation, from reports usable at t only: `n_positions` and price-free
`position_turnover` (share of positions initiated or exited between q-1 and q, a
count measure). A filer is **excluded as index-like** if it is in the top tercile of
`n_positions` **and** the bottom tercile of `position_turnover` in that quarter's
cross-section. Terciles are recomputed per quarter (no full-sample cut-offs); one
flag per filer per formation applies to both endpoints of that change. Filers with
fewer than 10 positions are excluded.

### 1.10 Gold layer: factoRverse inputs (implemented 2026-11-07, Codex-reviewed design)

- Pins are plain data frames in factoRverse `meta_dataframe` layout: `id`, `tickers`,
  `dates` first, `id = paste0(tickers, "-", dates)`, rows sorted by `id`, `tickers` =
  composite FIGI, only numeric columns after the key. The backtest layer wraps them
  with `create_meta_dataframe()` / `create_meta_xts()`.
- `dates` = first day of the holding month: a row dated 2020-09-01 carries what was
  known at the close of 2020-08-31; its forward return is September's. The formation's
  universe and signal are frozen for the three holding months (quarterly buy-and-hold).
  Backtest settings: `rebalancing_months = c(3, 6, 9, 12)`; daily returns must be cut
  at the formation date before factoRverse estimates covariances (its slice includes
  `current_date`).
- Panels (pipeline names): `gold_signals_m_df` (F1, breadth level, holders, IN and OUT,
  cohort size, momentum 12-1, reversal 1m, volatility 3m, Amihud, signal age),
  `gold_fwd_1m_returns_m_df` (`fwd_return_1m`, `fwd_return_1m_stress`, percentage
  points, compounded daily total returns; 0 after the last price; NA only in the
  unfinished last month), `gold_liquidity_m_df` (`mean_volfin_3m` for factoRverse
  costs, `median_volfin_3m` for the universe, `presence`, `close_prev`),
  `gold_volatility_m_df` (`daily_vol`, `vol_3m`). Companions: `gold_universe_diagnostics`,
  `gold_cohort`, `gold_daily_returns_d`, `gold_benchmark_d`, `gold_benchmark_m` (SPY).
- Universe order at each formation: price source, identity accepted at t from the
  holdings versions current at t (D2), not a suspected CUSIP change (D9), traded at t,
  previous close >= 5 USD, 12 months of history, complete features, top 1000 by
  previous 63-day median dollar volume (ties by FIGI), then zero holders in q-1 or q
  removed (D5).
- Gold suite (`run_test_gold()`, added 2026-11-08), beyond the contract checks: every
  universe exclusion reason is recomputed independently (first failing screen from a
  long table of failure flags, rank by sorting) and must match on every row; no member
  breaks a screen; the liquidity cut is a clean boundary; the panels hold exactly the
  members; membership turnover between formations stays below 15% and one-formation
  re-entries below 3% of member rows (observed 8% and 1.4%). Cohort: both reports
  re-read as public at t from `silver_filings`, few-positions and index-like labels
  recomputed, every filer with both report versions current at t present in the cohort
  table, every overlap drop linked to an inclusion in a report current at t, and the
  position counts of 20 random cohort filers recomputed in dplyr. A stratified random
  sample of 40 panel rows (seed 20261108; regular, last-price month, first formation,
  fewest holders, third month of a signal; every stratum with rows must be drawn) is
  recomputed from silver in dplyr (breadth, F1, identity count, features, forward
  returns) and must match the SQL. Unit tests run the
  same references on `toy_silver_gold.rds`, a real-data sample built by
  `data-raw/make_toy_gold_fixture.R`.

---

## 2. Schedule

Assumes Phase 0 starts Fri 19:30 at home. Times are targets; meals and sleep are in.

| Window | Phase | Output | CP |
|---|---|---|---|
| Fri 19:30 to 21:00 | 0 Setup | repo, skeleton, style files, keys checked, CI-free `devtools::check()` passes | |
| Fri 21:00 to 00:30 | 1 Feasibility spike | gate report (Section 3) | **G1** |
| Fri 23:45 to Sat 13:00 | 2 Infrastructure, `deploy_bronze()` (SEC), silver filings and holdings | board and pin helpers; bronze SEC pins; `filings`, `filing_relations`, `holdings_state` full window | **CP1** |
| Sat 14:00 to 18:00 | 3 Bronze OpenFIGI and Tiingo; silver identity, prices, terminal events | `security_master`, `ticker_alias`, `prices_d`, `terminal_events`, coverage report | **CP2** |
| Sat 19:00 to 21:00 | 4 `deploy_gold()`: activity proxy, universe, cohort, F1 | `filer_activity`, universe per formation, `signal` | **CP3** |
| Sat 21:00 to 01:00 | 5 `deploy_backtests()`: `cs`, backtest, costs, evaluation, double sort | results tables | |
| Sun 08:30 to 12:30 | 6 `deploy_reports()`, HTML report, 3-page memo, README, fresh-clone run | tag `v0.1-submission` | **CP4** |
| Sun 12:30 to 13:30 | Codex code review of the PR (runs while lunch) | findings | |
| Sun 13:30 to 18:30 | 7 Extensions, in order (Section 5) | improved results | **CP5** |
| Sun 18:30 to 21:30 | 8 Accepted findings, final memo, fresh-clone run | tag `v1.0` | **CP6** |
| Sun 21:30 to Mon 08:00 | Buffer and sleep; Mon 06:30 final check and submit | | |

Lateness triggers are per checkpoint, not a blanket 1.5 h:
- **G1** fails or is not done by Sat 01:00: apply the G1 fail actions before sleeping.
- **CP1** not reached by Sat 14:00: shorten the window (Section 6, step 4).
- **CP2** not reached by Sat 19:00: accept the coverage as is, label the study
  conditional, move on.
- **CP4** not reached by Sun 14:00: stop all extensions; finish CP4 only.

---

## 3. Phase 1 feasibility spike (decision gate G1)

Throwaway script, not package code. Measurements:

1. **Archives**: one early (2014Q1) and one recent (2025) zip, plus the archive
   containing January 2023. Record compressed and expanded size, load wall time,
   peak R memory, duckdb file size and temp spill. Extrapolate the full run and one
   retry.
2. **Unit transition**: verify implied-price ratios cluster near 1 under the filing-date
   regime rule, including at least one old-quarter amendment filed after 2023-01-03.
3. **Identity chain on a stratified sample of about 60 CUSIPs**: current large caps,
   acquired companies, bankruptcies, ticker changes, dual share classes, a CUSIP
   change. For each: OpenFIGI result, composite FIGI, Tiingo series found, series
   start and end, terminal observation, implied-price check.
4. **Tiingo meta**: what the ticker metadata returns for delisted names (dates,
   alternative identifiers).

Pass / fail actions (fixed now):
- Full run estimated above 6 h wall time or above 60 GB disk: start the window at
  2016Q1 (logged deviation).
- Identity chain success below 80% by count in the stratified sample, or failures
  concentrated in delisted names: continue, label the study conditional on observable
  securities, add a missingness-by-side table to the memo. Do **not** buy or
  integrate a new reference source this weekend.
- Peak memory above 5 GB: reduce duckdb threads to 1 and process one quarter at a
  time; if still above, move to GitHub Codespaces (setup tested only if needed).

---

## 4. Testing (proportionate)

- Unit tests per function; realistic unbalanced fixtures ordered by id.
- **Mandatory scenario fixtures**:
  - F1 cohort: one initiation, one complete exit, one unchanged position, one CUSIP
    change with the same composite FIGI, one CUSIP change with a new FIGI, one filer
    missing quarter q, one filer whose q report is usable after t.
  - Amendment sequences: original then new-holdings then restatement; two
    new-holdings; restatement then new-holdings; same-day amendments; an old-quarter
    amendment filed after 2023-01-03.
  - Overlap: a combination report including a manager that also files alone.
  - Confidential position revealed by amendment after t (must not enter at t).
- Invariants: no filing with `usable_date` > t enters a signal at t; legs sum to +1
  and -1; no trades between formation dates; returns start at t+1.
- One end-to-end run on the offline fixture (`targets` fixture profile), no network.
- Network functions tested against recorded responses.

---

## 5. Extensions after CP4 (strict order, stop when time runs out)

1. `rp` weighting (second construction method).
2. HAC lag sensitivity (3 and 6 months) and leave-one-period-out stability.
3. Square-root (Barra-type) impact model with explicit units: order in USD, ADV in
   USD per day, daily volatility as a fraction, coefficient stated; participation rates
   at USD 100M.
4. Disclosure-lagged performance profile: F1 portfolio returns in months 1 to 12
   after formation (a diagnostic of decay, not F2).
5. Activity-proxy split (high vs. low turnover filers).
6. Capacity-based universe (registered 2026-11-07 by the owner; replaces the fixed top
   1000 only as a sensitivity, logged as a deviation and counted as a trial). Given a
   strategy size `strategy_aum` (USD), the average position of a diversified leg is
   `position = strategy_aum * leg_gross / n_leg` (long and short legs each `leg_gross` =
   1 of capital; `n_leg` from the quintile rule). A stock is eligible at formation t
   only if trading it stays within a participation cap of its average daily dollar
   volume: `traded_per_day <= max_participation * ADV`, with `max_participation` =
   0.05 and ADV = `mean_volfin_3m` measured up to t-1. For a full entry or exit spread
   over `exec_days` days, `traded_per_day = position / exec_days`, so the minimum is
   `ADV_min = position / (max_participation * exec_days)`. Example: USD 100M,
   `n_leg` = 200, `exec_days` = 1 gives a position of USD 0.5M and `ADV_min` = USD
   10M per day. Parameters: `strategy_aum`, `max_participation`, `exec_days`;
   report how many stocks pass per formation and the F1 result by AUM level.

Not this weekend: F2 to F5, F3 skill ranking, Fama-MacBeth, XBRL market cap,
cap-weighted benchmark, USD 1B capacity, free-tier reproduction mode.

---

## 6. Simplification ladder (pre-agreed cuts, in order)

1. Drop all Section 5 extensions.
2. Replace the activity proxy by "exclude filers above the 90th percentile of
   n_positions" (stated as a cruder proxy).
3. Drop the overlap sensitivity; keep the MVP overlap rule.
4. Shorten the window to 2016Q1 onward.
5. Drop the past-return double sort (then the memo cannot conclude "supported"; it
   reports the result as unconditional evidence only).

Never cut: timing contract, amendment state machine, unit regime rule, identity and
terminal policy with missingness reporting, cost scenarios, HTML report, memo,
fresh-clone run.

---

## 7. Reproduction contract (README)

Full reproduction requires: R and packages via `renv::restore()`; `SEC_USER_AGENT`;
a Tiingo key on a paid plan (Power); an OpenFIGI key (free); about N GB disk and
about H hours on first run (N and H measured in G1). Functional verification without
keys or network: `targets` fixture profile, under a minute. Fresh-clone test is run
twice: at CP4 and at CP6.

---

## 8. Codex review resolution log (v0.1 to v0.2)

| # | Finding | Decision | Where |
|---|---|---|---|
| 1 | CP4 is not a submission | Accepted: CP4 now includes report, memo, fresh clone; `rp`, USD 1B and impact model moved after CP4; double sort kept in CP4 | 0, 2, 5 |
| 2 | Identity and delisting unproved | Accepted: Friday chain audit, composite-FIGI-only chaining, terminal policy with stress, missingness by side | 1.6, 3 |
| 3 | Intersection rule censors F1 | Accepted: matched filer cohort, union of eligible securities, identity breaks handled separately | 1.2, memo F1 |
| 4 | Leakage paths | Accepted: common consecutive quarters, both usable at t, per-quarter activity terciles, t-1 screens, F3 deferred | 1.2, 1.9 |
| 5 | Overlap and amendment state | Accepted with modification: both relation tables kept; simple keep-larger rule plus sensitivity instead of entity resolution | 1.3, 1.4 |
| 6 | No slack | Accepted: optional work cut, per-checkpoint triggers, CP4 moved to Sun 12:30 | 2, 6 |
| 7 | Weak gate | Accepted: expanded spike with resource measurement and fixed actions | 3 |
| 8 | Classifier unspecified | Accepted: renamed activity proxy, fully specified, price-free turnover, per-quarter | 1.9 |
| 9 | Unit regime | Accepted: filing-date regime, raw value kept, row-level quarantine | 1.5 |
| 10 | Accounting and costs undefined | Accepted: dollar-neutral convention, drift, scenario costs, borrow sensitivity; impact model after CP4 | 1.7, 5 |
| 11 | Universe and benchmark conflict | Accepted: liquidity screen in MVP (owner to confirm), EW universe and SPY benchmarks | 1.7, 1.8 |
| 12 | Decision rule | Accepted: interval-based rules, quarterly formation counts, HAC sensitivity | memo 8 |
| 13 | Extensions test other hypotheses | Accepted: F1 claim narrowed, F2 to F4 deferred | memo 3 |
| 14 | Contracts contradict key policy | Accepted: `instrument_id` everywhere, ticker alias table, accession provenance | 1.1 |
| 15 | Free-tier mode | Accepted: dropped; reproduction contract plus offline fixture | 7 |

Open owner decisions: package name; liquidity screen inside the MVP (reverses the
earlier "screening after MVP" call; needed to define the sample).

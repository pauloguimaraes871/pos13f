# G1 feasibility gate report

Date: 2026-11-06. Plan reference: `docs/IMPLEMENTATION_PLAN.md`, Section 3. Scripts:
`data-raw/spike/01` to `06`. Outputs (git-ignored): `data-raw/cache/spike/`. Reviewed by
Codex (findings G1-01 to G1-03, Section 7); numbers below are after the G1-01 fix.

## Verdict

| Gate | Threshold | Measured | Result |
|---|---|---|---|
| Full-run wall time | 6 h | about 30 min for 13F; Tiingo bounded by 10,000 requests per hour | Pass |
| Disk | 60 GB | about 10 GB | Pass |
| Peak memory | 5 GB | 1.4 GB (duckdb), 1.7 GB (plain readr, largest archive) | Pass |
| Identity chain | 80% by count, not concentrated in delisted names | 58% of the sample (35 of 60); survivors 83%, names gone by 2025 30%; about 68% reweighted to the top-1,500 mix | **Fail action applies** |

Fixed action for the identity gate (plan Section 3): continue, label the study as
conditional on observable securities, add a missingness-by-side table to the memo. No new
reference source is bought or integrated. The window is not shortened at G1; Phase 3
measures coverage per formation and CP2 decides with that evidence.

## 1. Archives and resources

* The SEC page lists **54 archives**, 2013q2 to 01jun2026-31aug2026, **3.05 GB
  compressed** in total (36 to 101 MB each; 2013q2 is a 2 MB partial quarter).
* From 2024 the archives cover three-month filing windows (for example
  01mar2025-31may2025) instead of calendar quarters.
* Sample of three archives (2014q1, 2023q1, 01mar2025-31may2025):

| Archive | Expanded MB | INFOTABLE rows | Load s | Process peak MB | duckdb MB (cumulative) |
|---|---|---|---|---|---|
| 2014q1 | 160 | 1,584,857 | 18.2 | 567 | 54 |
| 2023q1 | 289 | 2,786,017 | 18.3 | 1,034 | 157 |
| 01mar2025-31may2025 | 360 | 3,440,002 | 20.7 | 1,429 | 283 |

* Loaded rows equal source data lines exactly in all three archives. No temp spill.
  Settings: `memory_limit = 3GB`, `threads = 2`, temp directory on D:.
* Download: 2 to 5 s per archive.
* Extrapolation, 54 archives: download about 4 min, load about 18 min, duckdb about 5 GB
  (all columns as text). One retry doubles this; still far below the gate.
* Plain `readr` alternative, largest archive: 18.5 s, 785 MB table, 1.7 GB process peak.
  Both routes fit in memory; duckdb kept (simpler cross-archive amendment logic).

## 2. What the data looks like (SEC readme, `FORM13F_readme.htm`, section 5)

* Archives are grouped by **filing date**, not by report quarter. Each archive carries
  amendments and **late original reports** for old periods (2014q1 holds periods back to
  2008-12-31; 2023q1 back to 2016-12-31; 33 original 13F-HR for 2022-09-30 were filed in
  2023q1). A report quarter is assembled from several archives; the plan's `usable_date`
  rule already requires this.
* Per archive: 3,800 to 8,500 13F-HR, 1,400 to 1,900 13F-NT (no holdings, dropped),
  260 to 430 13F-HR/A, split RESTATEMENT and NEW HOLDINGS; 230 to 370 combination
  reports.
* Confidential treatment: about 90 to 110 filings per archive state
  `ISCONFIDENTIALOMITTED = Y`; 20 to 28 cover pages state `CONFDENIEDEXPIRED = Y`
  (positions revealed later).
* INFOTABLE: 98.8% to 99.5% of rows are shares (`SH`), 3.9% to 7.8% are options
  (`PUTCALL` not null, excluded). The filer-reported `FIGI` column is filled on 0% (2014),
  7% (2023) and 11% (2025) of rows: not usable as an identifier.
* Distinct CUSIPs (shares, no options): 24,093 / 23,805 / 26,430 per archive; **50,869**
  across the three. Full window estimate: 80,000 to 120,000. OpenFIGI at 100 jobs per
  request and 250 requests per minute: about 5 minutes.
* Filers type CUSIPs in lower case on some rows (`02079k305` for Alphabet A), and some
  keep reporting a stale CUSIP years after it changed.

## 3. Unit regime

SEC readme, section 5.7, VALUE: "Starting on January 3, 2023, market value is reported
rounded to the nearest dollar. Previously, market value was reported in thousands." [src]

Test: implied price (VALUE x multiplier / shares) divided by Tiingo unadjusted close on
the last trading day on or before the period of report, for the 35 CUSIPs whose identity
chain succeeded (about 98,500 rows).

| Group | Rule | Near 1 | Near 1000 | Near 0.001 | Other |
|---|---|---|---|---|---|
| a. Filed before 2023-01-03 | filing date | 96.6% | 2.4% | 0.0% | 1.0% |
| b. Filed after, current period | filing date | 87.1% | 0.0% | 12.1% | 0.8% |
| b. Filed after, current period | period of report | 62.6% | 33.1% | 3.5% | 0.8% |
| c. Filed after, old period (350 filings) | filing date | 79.3% | 0.0% | 18.3% | 2.4% |
| c. Filed after, old period (350 filings) | period of report | 18.1% | 79.5% | 0.0% | 2.3% |

* The **filing-date rule is confirmed**: right for 79% to 97% of rows in every group;
  the period rule is wrong for 80% of old-period filings.
* A material minority of filers used the **wrong unit**: 2.4% of rows before the change
  already in dollars, 12% to 18% of rows after the change still in thousands. Detectable
  per filing by the implied-price check (ratio near 1000 or 0.001). Handled by D1 and D6.

## 4. Identity chain (CUSIP to composite FIGI to Tiingo series)

Sample: the 1,500 most widely held CUSIPs at 2013-12-31 split into **survivors** (held in
a report for period 2025-03-31: 1,069) and **gone by 2025** (431); random draw of 25 and
30 (seed 13); plus 6 named special cases from the 2023q1 data (Alphabet A and C,
Berkshire A, Meta, SVB, Bed Bath & Beyond). CUSIPs upper-cased; letter-prefixed CUSIPs
mapped as `ID_CINS`. One ETF (out of the equity universe) excluded from the denominator.

Success requires: OpenFIGI US composite FIGI found, a Tiingo series found, and implied
price within 0.8 to 1.25 of the Tiingo close at the period end.

| Stratum | n | Chain verified | Rate |
|---|---|---|---|
| Survivors | 24 | 20 | 83% |
| Gone by 2025 | 30 | 9 | 30% (95% interval roughly 14% to 46%) |
| Special cases | 6 | 6 | 100% |
| Reweighted to top 1,500 (71% survivors, 29% gone) | | | **about 68%** |

Every verified chain has a price ratio of 1.000 (to three decimals). An earlier draw with
a mislabelled survivor stratum (Codex G1-01) gave 55% for gone names; both draws agree
that dead names fail far more often.

Failure modes, gone-by-2025 stratum (21 of 30):

1. **Old CUSIP unknown to OpenFIGI (9)**: renamed or reorganised issuers (Gannett,
   Vantiv, ValueClick, Seattle Genetics, Hospitality Properties, Thomson Reuters, Swift
   Transportation, SK Telecom, Superior Energy). OpenFIGI knows only current CUSIPs.
2. **FIGI found, ticker unusable (7)**: OpenFIGI returns a Bloomberg dead-security code
   (`1284849D` for Allergan; also Aspen, Macquarie Infrastructure, Informatica, Towers
   Watson, SemGroup) or a bankruptcy ticker (`GPORQ`); Tiingo answers 404.
3. **Tiingo has no price at the date (4)**: Mentor Graphics and Molycorp series start in
   2016; Meredith has a gap at end-2013; Lender Processing Services has no data.
4. **Ticker reused (1)**: Multimedia Games maps to `MGAM`, today Mobile Global Esports.
   The implied-price check rejected it.

Survivor failures (4): Carnival paired certificate, American Capital Agency and Ctrip
(old CUSIPs unknown after renames), Harsco (dead-security code after its rename).

The largest group recoverable without a new source is mode 2 (FIGI known, historical
ticker unknown); it needs a historical ticker for a FIGI, which neither OpenFIGI nor
Tiingo provides. Listed under "build next".

## 5. Tiingo

* Ticker endpoints return delisted series with their last date (Bed Bath & Beyond to
  2023-09-29, Molycorp to 2016-09-01): terminal dates are available for the baseline and
  stress policy. Some delisted histories start only in 2016.
* Supported-ticker list (`supported_tickers.zip`): columns ticker, exchange, assetType,
  priceCurrency, startDate, endDate; 16,171 US stocks with an end date before 2026;
  **2,398 tickers appear more than once** (reuse). No CUSIP, FIGI or name column.
* Owner's Power limits: 10,000 requests per hour, 100,000 per day, 40 GB per month.
* One transient request failure occurred in a rerun of script 06: every Tiingo call in
  the package needs retries.

## 6. Owner decisions (2026-11-06)

* **D1 Unit errors never remove holdings from counts.** Plan 1.5 quarantined rows and
  filings that fail the unit check. Decision: the check acts on the **value** fields
  only; the position always counts for F1 and the activity proxy. The verdict is per
  filing: if most of a filing's checkable rows sit near 1000 (or 0.001), the whole
  filing's values are rescaled; a mixed filing gets missing values. Corrections are
  counted and reported.
* **D2 Identity procedure.** Upper-case CUSIPs; `ID_CINS` for CUSIPs starting with a
  letter; the implied-price check becomes an acceptance condition (plan 1.6 called it a
  diagnostic only). The check is **point-in-time**: per instrument and report quarter,
  using only filings usable at formation t, so a failure removes the stock only from the
  formations that use that quarter. Failures are counted per formation by side.
* **D3 Tiingo request set.** All mapped US common stocks are downloaded, **no holder
  filter**, with a file cache so reruns do not spend the limits. A filter is applied
  (and logged) only if the mapped count exceeds about 25,000.
* **D4 Identity look-ahead, disclosed and measured.** Mappability depends on a CUSIP's
  future (a CUSIP that later changed, or a company that later died, often cannot be
  mapped today), so excluding unmapped CUSIPs at formation t is a look-ahead selection
  toward survivors. With about 68% coverage of the 2013 top 1,500 this is **material
  for early formations**. Kept: composite-FIGI-only chaining, no new source. The memo
  reports coverage per formation and by side, including the F1 of the unmapped stocks
  (computable from holdings without prices), and lists a historical identifier (CRSP
  PERMNO or a FIGI-to-historical-ticker source) as the first item to build next.
* **D5 Zero-holder guard (Codex G1-02).** A CUSIP change can make a retained stock jump
  from 0 holders to hundreds (old CUSIP unmapped, new one mapped), a false breadth
  increase; or fall to 0, a false exit. Rule: at each formation, a universe stock with
  **zero cohort holders at q-1 or at q is excluded** and counted separately. A top-1000
  liquid stock with 12 months of price history held by none of thousands of filers is a
  data break, not a real initiation or exit. Diagnostic: retained stocks whose holders
  also report an unmapped CUSIP with the same 6-character issuer prefix are counted.
  Deviation from memo F1 ("holders may be zero in either quarter"), logged.
* **D6 Order of unit correction and identity (Codex G1-03).** (1) Identity first, per
  instrument and quarter, from the **median** implied price across all filers usable at
  t (robust to the 2% to 18% minority of unit errors); (2) then the per-filing unit
  verdict, using only rows of accepted instruments; (3) missing or mixed unit evidence
  sets values to missing and never affects identity.

## 7. Codex review (2026-11-06)

| ID | Finding | Resolution |
|---|---|---|
| G1-01 | Survivor stratum used every period in the 2025 archive (late filings mislabel dead stocks) | Accepted: stratum on period 2025-03-31, CUSIPs upper-cased, sample redrawn; rates corrected above |
| G1-02 | Unmapped predecessor CUSIP gives the mapped successor zero previous holders, a false breadth signal | Accepted with modification: D5 |
| G1-03 | Order of unit correction and identity acceptance undefined; risk of circularity | Accepted: D6 |

## 8. Window

The first archive (2013q2) is a partial quarter. With two consecutive report quarters
required, the earliest clean formation is end-November 2013 (report quarters 2013Q2 and
2013Q3). Window confirmed at CP2 once Phase 3 reports coverage per formation.

# 13F positioning factors: pre-registered research design (DRAFT v0.2)

Status: written before any 13F data was downloaded or any backtest was run.
Everything below is fixed ex ante; any later deviation is logged in "Deviations"
with its date and reason. v0.2 integrates the Codex plan review.

Evidence status legend (quantitative-researcher skill):
- **[src]** claim read in the supplied PDF at the stated location (transcription
  only, not replication).
- **[mem]** cited from memory, not yet verified. Verified or dropped before the memo
  is final.
- No numeric result from any paper is quoted yet; numbers go through the evidence
  ledger first.

---

## 1. Research question

Does the change in **disclosed institutional breadth** (the number of active 13F
filers holding a stock), observed only after the filings are public, predict the
cross-section of returns of liquid US equities, net of trading costs, and is that
information distinct from price momentum?

## 2. What 13F can and cannot see

- Quarter-end **long** positions in 13(f) securities of managers with at least
  USD 100M in such securities. No shorts, no cash, no intra-quarter trades, no
  non-US listings.
- Filings are made per **manager** (CIK), aggregating every strategy and fund of
  that manager; some filers can also short.
- Positions can be withheld under confidential treatment and revealed later through
  an amendment.

## 3. Hypothesis tested this weekend (F1) and alternatives considered

### F1: breadth change (tested)

**Definition.** At formation date t, take the two common consecutive report quarters
(q-1, q) and the **matched cohort** of active filers whose reports for both quarters
are public at t (Plan 1.2, 1.9). For every security in the eligible universe (the
union of securities, holders count may be zero in either quarter):

```
breadth_change(i, t) = [ n_holders(i, q) - n_holders(i, q-1) ] / cohort_size(t)
```

where `n_holders(i, q)` counts cohort filers reporting a long share position in
`instrument_id` i for quarter q. A missing report is never read as an exit (the
filer is outside the cohort); an identity failure is never read as an exit (the
security is excluded and counted). This follows the matched-sample logic of Chen,
Hong & Stein (2002) **[mem: definition details to verify on the rendered pages]**.

**Mechanisms (interpretations, not tested separately).**
- Information: Sias, Starks & Titman (2006), pp. 2871 to 2872 **[src]**:
  contemporaneous returns relate more strongly to changes in the number of
  institutions than to changes in the fraction of shares held, which the authors read
  as information playing a central role.
- Short-sale constraints and dispersion of opinion (Miller 1977 **[mem]**; CHS model
  **[src abstract]**): falling breadth signals that pessimists are sidelined. This
  interpretation is weaker here than in CHS, because 13F filers are managers that
  aggregate strategies and some can short; we treat it as one possible reading only.

**Predicted sign:** positive (higher breadth change, higher subsequent return).

**Main threats, tested in the MVP.**
1. Price momentum: institutions trade with past returns. Test: 3 x 3 dependent
   double sort on past 12-1 month return then F1, plus FF5 + UMD spanning regression.
2. Disclosure lag: Jiang, Verbeek & Wang (2014), abstract **[src]**, find the
   consensus premium "dissipates quickly as the consensus view becomes publicly
   available". The 45-day lag may consume the information; a null result is a
   legitimate finding.

### Considered and deferred (not tested this weekend)

| ID | Idea | Why deferred | Evidence |
|---|---|---|---|
| F2 | Multi-quarter trade persistence, long-run reversal | Needs its own net-demand definition and long-horizon overlapping-return inference; not a by-product of F1 | Dasgupta, Prat & Verardo (2011), abstract and p. 3 **[src]**: persistent institutional buying negatively predicts long-term returns |
| F3 | Breadth change of top-performing filers | Holdings-implied skill ranking adds a second timing problem (returns earned on holdings public before the ranking window) | Alexander, Cici & Gibson (2007) **[src abstract]** |
| F4 | Consensus best ideas | Needs an active-weight benchmark per filer | Antón, Cohen & Polk (2021 draft), abstract and Section E **[src]**; Jiang, Verbeek & Wang (2014) **[src]** |
| F5 | Crowding (days-to-liquidate) | Literature sign is positive on average with tail risk: a risk premium, not a short signal | Brown, Howard & Lundblad (2021), pp. 2, 12 to 13 **[src]**; contrary: Nagel (2005) **[mem]** |

Rejected outright: shorting managers' short positions (13F has none); flow-induced
trading (no flow data); option positioning (13F reports notional, not delta).

## 4. Filer universe: holdings-based activity proxy

Index-like managers change positions for flow and index reasons, adding noise to a
breadth signal. Motivated by the transient / dedicated / quasi-indexer distinction of
Bushee (1998, 2001) **[mem]**, but **not a replication** of it: Bushee uses factor
scores and clusters and warns that classification changes can manufacture ownership
changes. Our proxy (Plan 1.9): per formation, exclude filers in the top tercile of
number of positions and the bottom tercile of price-free position turnover; one flag
per filer per formation, applied to both endpoints; terciles recomputed per quarter
from public data only.

## 5. Equity universe

Liquid US common stock: valid `instrument_id`, price at least USD 5, 12 months of
history, top 1000 by trailing 63-day median dollar volume, all measured up to t-1.
Described as liquid equities, not as large/mid cap. Market-cap screens deferred.

## 6. Point-in-time rules

Binding definitions are in Plan 1.2 to 1.6: usable date, common consecutive
quarters, both reports public at t, amendment state machine, overlap policy,
filing-date unit regime, composite-FIGI identity, terminal-return baseline and stress.

Information date [src]: each 13F filing is public on EDGAR within minutes of acceptance
(EDGAR PDS guide), and a submission after 5:30 pm Eastern is dated the next business
day (17 CFR 232.13(a)(2)), so a filing is public by the end of its `FILING_DATE`. The
SEC bulk data sets, posted after each window closes, hold filings "as filed" with
amendments as separate submissions (Form 13F data sets readme). Positions are used
from the next trading day: conservative by at most about one trading day, never early.
A live implementation would have to read EDGAR filings directly, since the bulk files
arrive weeks later.

## 7. Backtest design

- Quarterly formation (end of Feb, May, Aug, Nov), execution at close of t, returns
  from t+1, weights drift between formations. Timing checked on 2026-11-08 against the
  delay from quarter end to `usable_date` of every original 13F-HR (2014 to 2026): 66%
  of filings are usable by day 45, 93% by day 48, 96% by day 50 and 97% at month end
  (day 59 to 62). Filings bunch just before the day-45 deadline. Month end is kept
  (owner, 2026-11-08): it loses almost no reports and fits the monthly grid of the
  backtest code; the cost is about ten trading days of freshness, so any price reaction
  between the deadline and month end is outside the test (conservative for F1).
- Top minus bottom quintile, `cs` weights, dollar neutral.
- Costs: 10, 25, 50 bps one-way per dollar traded, break-even cost, borrow fee
  0 and 100 bps per year on the short leg.
- Benchmarks: same-universe equal weight, SPY total return, FF5 + UMD.
- About 50 quarterly formations: inference counts **formations**, not months.
  Newey-West on monthly returns with lags 3 (primary) and 6 (sensitivity); rank IC
  per formation with its time-series mean and standard error.

## 8. Decision rules (fixed before results)

Three separate verdicts, each reported with a 95% confidence interval:

1. **Predictive content.** Mean formation rank IC and gross long-short FF5 + UMD alpha.
   "Evidence of predictive content" if the alpha CI excludes zero under both HAC lags
   **and** the within-momentum-tercile F1 spread (double sort average) is positive with
   a CI excluding zero. "No evidence" if the CI includes zero. A CI including zero is
   **not** a rejection of a positive effect: with about 50 formations, an effect near
   0.28 quarterly residual standard deviations is needed for t near 2 under
   idealised assumptions; the memo states the minimum detectable effect.
2. **Economic viability.** Net long-short return at 25 bps one-way and 100 bps
   borrow: "viable" only if positive with the CI reported; break-even cost always
   reported.
3. **Incremental to momentum.** Double-sort spread and UMD-spanning alpha reported
   side by side; "subsumed by momentum" if the unconditional alpha is significant and
   the double-sort spread CI includes zero.

Minimum economically relevant effect: 2% per year gross long-short alpha.

## 9. Literature still to verify or add

- Chen, Hong & Stein (2002, JFE 66, p. 180) **[src]**: DBREADTH is the number of funds
  holding the stock at t minus at t-1, among funds in the sample at both dates,
  divided by all funds at t-1; IN and OUT decompose it. Sample period and spread
  values still to note.
- Nagel (2005, JFE 78, pp. 295 to 296) **[src]**: after CHS's sample ends in 1998,
  stocks with falling breadth outperformed those with rising breadth by a large
  margin, almost eliminating the effect over 1980 to 2003. Our sample (2013 to 2026)
  lies entirely after that reversal: a null or negative F1 result is a live outcome.
- Jegadeesh & Titman (1993, JF 48, p. 68) **[src]**: formation of 1 to 4 quarters,
  optionally skipping one week; the 12-1 month control follows later momentum-factor
  practice **[mem]**. Amihud (2002, JFM 5, p. 34, eq. 1) **[src]**: illiquidity is the
  average of absolute daily return over dollar volume.
- Gompers & Metrick (2001, QJE), "Institutional investors and equity prices": the
  supplied ssrn-278920 is Gompers, Ishii & Metrick (2003), a different paper.
- Bushee (1998, 2001); Miller (1977); Nagel (2005); Shumway (1997) delisting
  returns; Agarwal, Jiang, Tang & Yang (2013) confidential holdings (citation and
  abstract verified 2026-11-07: JF 68(2), 739 to 783; findings not yet read in the
  article).
- Brown & Schwarz (2013, NYU working paper), "Do Market Participants Care about
  Portfolio Disclosure? Evidence from Hedge Funds' 13F Filings": existence verified;
  the finding of a market reaction at the filing date is unverified and must not be
  cited until read.

## Deviations

Each entry: date, change, reason. Details in `data-raw/spike/G1_report.md`.

- 2026-11-06. Liquidity screen (Plan 1.8) confirmed inside the MVP by the owner, built as
  a separate step that can be switched off. Reason: defines the sample; must be cuttable.
- 2026-11-06. Analysis window becomes a pipeline setting (start and end quarter); a short
  real-data demo window is documented next to the full window. Reason: the brief requires
  a clean-clone run that shows real 13F data flowing through.
- 2026-11-06 (D1). Unit check acts on value fields only; it never removes a holding from
  the counts. Replaces the row and filing quarantine of Plan 1.5. Reason: 12% to 18% of
  rows filed after 2023-01-03 use the wrong unit; quarantine would create false exits.
- 2026-11-06 (D2). Implied-price check becomes a point-in-time acceptance condition for a
  price series (Plan 1.6 had it as a diagnostic). Reason: tickers are reused; the check
  rejected a wrong company in the spike.
- 2026-11-06 (D3). All mapped US common stocks are priced, no holder filter. Reason:
  Tiingo Power limits allow it.
- 2026-11-06 (D4). Identity coverage about 68% of the 2013 top 1,500, concentrated in
  dead names: study labelled conditional on observable securities (G1 fail action).
- 2026-11-06 (D5). Universe stocks with zero cohort holders at q-1 or q are excluded and
  counted, replacing "holders may be zero in either quarter" in Section 3. Reason: a
  CUSIP change otherwise creates false initiations and exits (Codex G1-02).
- 2026-11-06 (D6). Identity is accepted before units are corrected, from the median
  across filers. Reason: avoids circular checks (Codex G1-03).
- 2026-11-07 (D7). Identity uses a snapshot of OpenFIGI and Tiingo taken on the download
  date, not a point-in-time ticker history. Instruments whose ticker was later reused, or
  whose filed prices disagree with Tiingo, cannot be priced: 0.45% of 13F holding value,
  mostly dead firms. This is a survivorship effect from later information; it is
  reported by quarter and portfolio side, not corrected. Reason: no ticker history in the
  free data; the implied-price check shows 99.96% agreement on liquid, well-held stocks,
  so a permaTicker download was judged not worth it (owner, 2026-11-07).
- 2026-11-07 (D9). An instrument with at least 20 quarter-q cohort holders, more than
  half of them initiations, is a suspected CUSIP change and leaves the universe for
  that formation. Reason: the largest F1 values were all corporate actions (GOOGL
  2016, RTX 2020, GE 2021, MDT 2015, BKNG 2018, BLK 2025) whose old CUSIP maps to
  another FIGI, so holders switching CUSIP looked like initiations; Plan 1.6 already
  treats such breaks as leaving the signal. A data-quality rule set before any return
  was computed; threshold chosen by Claude, open to the owner. The label is a heuristic
  flag, not a confirmed corporate action. Impact (gold pins of 2026-11-08): 455
  instrument-formations that were seasoned and liquid enough for the universe (median 8
  per formation, at most 25); a further 438 liquid rows are IPOs, which the
  12-month history screen would drop anyway but which carry this label because D9 is
  checked first.
- 2026-11-07 (D10). F1 divides by the matched cohort size, while Chen, Hong & Stein
  divide by all funds at q-1. Within a formation both scale every stock by the same
  factor, so ranks and quintile portfolios are identical.
- 2026-11-08. Correction, not a design change: the 63-day Amihud illiquidity averaged
  days with zero dollar volume, giving NaN (0/0) or infinity. The NaN alone caused every
  `missing_features` universe exclusion (18,214 candidate rows, 40 of them liquid
  enough for the universe). Amihud now averages over days with volume, as in Amihud
  (2002). Found by the dplyr recomputation of the real-data toy sample, before any
  return was computed.
- 2026-11-08. Correction from the Codex gold review, before any return was computed:
  the overlap rule (Plan 1.4) read inclusion links from every filing of quarter q usable
  by t, including filings a restatement had superseded, and ignored q-1. It now reads
  links only from the filings that make up each report version current at t, for both
  compared quarters, and drops the smaller report of a pair in either quarter. Overlap
  exclusions rose from 8,052 to 8,658 filer-formations (619 left the cohort, 13
  re-entered, 0.3% of members); pooled rank correlation of F1 with the previous build
  0.9994. The "drop both" sensitivity of Plan 1.4 is not implemented.
- 2026-11-07 (D8). The implied-price check counts the filers whose value per share lies
  within 0.5 to 2 times Tiingo's raw close and accepts when most do, instead of the
  median of D6. Reason: same intent, and counts stream over 77 million rows where a
  median ran out of memory.

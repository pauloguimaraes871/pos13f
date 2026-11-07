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

## 7. Backtest design

- Quarterly formation (end of Feb, May, Aug, Nov), execution at close of t, returns
  from t+1, weights drift between formations.
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

- Chen, Hong & Stein (2002): sample period, spread values and exact breadth
  definition (digits are font-encoded in the supplied PDF; render the pages).
- Gompers & Metrick (2001, QJE), "Institutional investors and equity prices": the
  supplied ssrn-278920 is Gompers, Ishii & Metrick (2003), a different paper.
- Bushee (1998, 2001); Miller (1977); Nagel (2005); Shumway (1997) delisting
  returns; Agarwal, Jiang, Tang & Yang (2013) confidential holdings.

## Deviations

(none yet)

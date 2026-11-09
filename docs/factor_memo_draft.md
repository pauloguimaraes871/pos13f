# 13F positioning factors: pre-registered research design

Written before any 13F data was downloaded or any backtest was run. The hypothesis,
definitions and decision rules below were fixed ex ante; every later change is listed
with its date and reason under "Deviations". The git history of this file shows each
edit.

## 1. Research question

Does the change in disclosed institutional breadth (the number of 13F filers holding a
stock), observed only after the filings are public, predict the cross-section of
returns of liquid US equities, net of trading costs, and is that information distinct
from price momentum?

## 2. What 13F can and cannot see

- Quarter-end long positions in 13(f) securities of managers with at least USD 100M in
  such securities. No shorts, no cash, no intra-quarter trades, no non-US listings.
- Filings are made per manager (CIK), aggregating every strategy and fund of that
  manager; some filers can also short.
- Positions can be withheld under confidential treatment and revealed later by an
  amendment.

## 3. Hypothesis (F1) and alternatives considered

### F1: breadth change (tested)

At formation date t, take the two consecutive report quarters (q-1, q) and the matched
cohort of filers whose reports for both quarters are public at t:

```
breadth_change(i, t) = [ n_holders(i, q) - n_holders(i, q-1) ] / cohort_size(t)
```

`n_holders(i, q)` counts cohort filers reporting a long share position in instrument i
for quarter q. A missing report is never read as an exit (the filer is outside the
cohort), and an identity failure is never read as an exit (the security is excluded
and counted). This follows the matched-sample definition of Chen, Hong and Stein (2002,
p. 180).

Mechanisms (interpretations, not tested separately):
- Information: Sias, Starks and Titman (2006, pp. 2871 to 2872) find that returns relate
  more strongly to changes in the number of institutions than to changes in the
  fraction of shares held, which they read as information playing a central role.
- Short-sale constraints: in the Chen, Hong and Stein model, falling breadth signals
  that pessimists are sidelined. Weaker here, because 13F filers aggregate strategies
  and some can short.

Predicted sign: positive (higher breadth change, higher subsequent return).

Main threats, tested:
1. Price momentum: institutions trade with past returns. Test: dependent double sort
   on past 12-1 month return then F1, and an FF5 + UMD regression.
2. Disclosure lag: the 45-day lag may consume the information; a null result is a
   legitimate finding.

### Considered and deferred

| Idea | Why deferred | Reference |
|---|---|---|
| Persistent buying over several quarters, long-run reversal | Needs its own net-demand definition and long-horizon inference | Dasgupta, Prat and Verardo (2011) |
| Breadth change of the best-performing filers | A skill ranking adds a second timing problem | Alexander, Cici and Gibson (2007) |
| Consensus best ideas | Needs a benchmark weight per filer | Antón, Cohen and Polk (2021); Jiang, Verbeek and Wang (2014) |
| Crowding (days to liquidate) | Positive on average with tail risk: a risk premium, not a short signal | Brown, Howard and Lundblad (2021) |

Not feasible with 13F: following managers' short positions (not reported), flow-induced
trading (no flow data), option positioning (13F reports notional, not delta).

## 4. Filer universe

Index-like managers change positions with index membership and fund flows, which adds
noise to a count of holders. Proxy, recomputed at each formation from public data
only: a filer is index-like if it is in the top third by number of positions and the
bottom third by position turnover (a price-free count measure). Index-like filers and
filers with fewer than 10 positions are excluded; one flag per filer per formation
applies to both quarters compared.

## 5. Equity universe

Liquid US common stock: a valid instrument identifier, price at least USD 5, 12 months
of history, top 1,000 by trailing 63-day median dollar volume, all measured before t.
Described as liquid equities, not as large or mid caps.

## 6. Point-in-time rules

The binding definitions are in the implementation plan (Section 1.2): usable date,
consecutive report quarters, both reports public at t, amendment versions, overlap
policy, value units, identity and terminal returns.

Information date: a 13F filing is public on EDGAR as soon as it is accepted, and a
submission accepted after 5:30 pm Eastern is dated the next business day (Regulation
S-T Rule 13(a)(2)). Positions are used from the first trading day after the filing
date, and the portfolio trades at the close of that day or later. A random sample of
filings is checked against EDGAR's acceptance times in the pipeline.

## 7. Backtest design

- Quarterly formation at the end of February, May, August and November; execution at
  the close of t, returns from t+1, weights drifting between formations.
- Top minus bottom quintile, `cs` weights (signal times liquidity), dollar neutral. All
  five quintile portfolios are backtested, so the results show whether returns rise
  across quintiles; a monotonicity test is a diagnostic, not a verdict.
- Terminal stress: the `cs` ladder is rerun with -30% in the month of a stock's last
  price.
- Weighting robustness: `cs` is the pre-registered construction; `ew` and `rp` are
  robustness checks, not trials. A verdict counts as robust only if it holds under all
  three, never by selecting the best scheme.
- Costs: 10, 25 and 50 bps one-way per dollar traded, the break-even cost, and a borrow
  fee of 0 and 100 bps a year on the short leg.
- Benchmarks: same-universe equal weight, SPY total return, FF5 + UMD.
- About 50 quarterly formations: Newey-West on monthly returns with lags 3 (primary)
  and 6 (sensitivity); rank IC per formation with its time-series mean and standard
  error.

## 8. Decision rules (fixed before results)

Three separate verdicts, each reported with a 95% confidence interval:

1. **Predictive content.** Mean formation rank IC and gross long-short FF5 + UMD alpha.
   "Evidence of predictive content" if the alpha CI excludes zero under both HAC lags
   **and** the within-momentum-tercile F1 spread (double sort average) is positive with
   a CI excluding zero. "No evidence" if the CI includes zero. A CI including zero is
   **not** a rejection of a positive effect; the memo states the minimum detectable
   effect.
2. **Economic viability.** Net long-short return at 25 bps one-way and 100 bps
   borrow: "viable" only if positive with the CI reported; break-even cost always
   reported.
3. **Incremental to momentum.** Double-sort spread and UMD-spanning alpha reported
   side by side. The double sort is dependent: F1 quintiles within each momentum
   tercile at formation, top minus bottom averaged over the three terciles, so it
   measures F1's predictive content conditional on momentum. "Subsumed by momentum" if
   the unconditional alpha is significant and the double-sort spread CI includes zero.

Minimum economically relevant effect: 2% per year gross long-short alpha.

## Deviations

Each entry: date, change, reason. Labelled entries (D1 to D11) are referenced from the
code. All were set before any return was computed unless stated otherwise.

- 2026-11-06. The liquidity screen is part of the main study and can be switched off.
  The analysis window is a pipeline setting, with a short demo window for a fresh-clone
  run.
- 2026-11-06 (D1). The unit check acts on value fields only and never removes a holding
  from the counts: removing rows filed in the wrong unit would create false exits.
- 2026-11-06 (D2). The implied-price check becomes a point-in-time acceptance condition
  for a price series, because tickers are reused by later companies.
- 2026-11-06 (D3). Every mapped US common stock is priced, with no minimum number of
  holders.
- 2026-11-06 (D4). Identity coverage is lower for companies that later died, so results
  are stated as conditional on securities that can be identified.
- 2026-11-06 (D5). Universe stocks with zero cohort holders in q-1 or q are excluded and
  counted: a CUSIP change would otherwise create false initiations and exits.
- 2026-11-06 (D6). Identity is accepted before value units are corrected, so the unit
  check and the identity check do not depend on each other.
- 2026-11-07 (D7). Identity uses OpenFIGI and Tiingo as queried on the download date,
  with no ticker history. Holdings that cannot be identified this way are 0.45% of 13F
  value, mostly dead firms; the effect is reported, not corrected.
- 2026-11-07 (D8). The implied-price check accepts a mapping when most holders' value
  per share lies within 0.5 to 2 times Tiingo's close (a count rather than a median, for
  memory).
- 2026-11-07 (D9). An instrument with at least 20 quarter-q cohort holders, more than
  half of them new, is treated as a suspected CUSIP change and leaves the universe for
  that formation. Corporate actions (GOOGL 2016, RTX 2020, GE 2021, BLK 2025) otherwise
  appear as mass initiations. It removes about 9 otherwise eligible stocks per
  formation.
- 2026-11-07 (D10). F1 divides by the matched cohort size instead of all funds at q-1;
  both scale every stock in a formation by the same factor, so rankings are identical.
- 2026-11-08 (D11). A stock must have traded on at least 90% of the previous 63 trading
  days, so that a vendor price frozen after a takeover or delisting cannot enter the
  universe.
- 2026-11-08. Amihud illiquidity averages over days with trading volume, as in Amihud
  (2002); days without volume had produced undefined values.
- 2026-11-08. The overlap rule reads inclusion links only from the report versions
  current at t, for both quarters compared.
- 2026-11-08, after the first backtest results. The monotonicity diagnostic uses an
  intersection-union test (every adjacent quintile spread must have a positive mean)
  instead of a bootstrap test whose null overstated the evidence. It is a diagnostic,
  not a verdict, and the change makes it stricter.

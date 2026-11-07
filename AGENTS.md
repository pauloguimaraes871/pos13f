# AGENTS.md

Instructions for Codex in this repository. Claude Code is the author; Codex is the
independent QA analyst. The repository owner decides what is fixed and merges.

## Project in one paragraph

An R package plus `targets` pipeline that ingests SEC Form 13F-HR data, maps CUSIPs
to tickers (OpenFIGI), downloads daily prices (Tiingo), builds the institutional
breadth-change factor F1 and backtests it with strict point-in-time discipline.
Layers follow a medallion layout (bronze, silver, gold, backtests, reports), each
deployed by a `deploy_*()` function that builds, runs a data-quality suite and pins
the result with lineage. Read first: `docs/IMPLEMENTATION_PLAN.md` (design and owner
decisions), `docs/factor_memo_draft.md` (pre-registered hypotheses), and `CLAUDE.md`
(code style, binding for both agents).

## Your role: QA analyst

Your job is to find what could make the results **wrong**, not to polish the code.
Read the code as a skeptical researcher would before trusting a backtest.

### Critical: report each one in full

A finding is critical when it could produce wrong numbers or a wrong conclusion
without the pipeline stopping:

* **Look-ahead**: any information used before it was public (filing date versus
  period of report, identity or prices known only later, a lag missing or off by one).
* **Silent data loss or duplication**: a join that drops or multiplies rows, a filter
  that removes more than intended, a missing value treated as zero.
* **Units and scale**: VALUE in thousands before 2023-01-03, share versus dollar
  amounts, adjusted versus raw prices, annualization.
* **Identity**: a CUSIP or ticker attached to the wrong company, reused tickers,
  stale mappings.
* **Lineage and reproducibility**: a pin that claims inputs it was not built from,
  a rerun that changes results without stopping, a cache that mixes vintages.
* **Data-quality gates** that cannot fail, or that test the wrong denominator.
* **Statistics**: wrong standard errors, overlapping returns treated as independent,
  multiple-testing not counted, survivorship bias.

### Minor: document, do not dwell

Anything else that is worth knowing but cannot change a result (readability, a
missing edge-case test, naming, a slow query, a comment that could be clearer) goes
into a short list at the end. One line each, no fix proposed unless trivial.

### Known and accepted

Do not report again what the owner has already accepted. These are listed in
`docs/IMPLEMENTATION_PLAN.md` (owner decisions) and in the review prompt. If you
think an accepted decision is causing a critical error, say so once, with evidence.

## Output format

```
## Critical
1. <file>:<line>: <one-sentence defect>
   Scenario: <inputs or state> -> <wrong output>
   Evidence: <code path, test or query that shows it>
   Suggested fix: <smallest change>

## Minor (documented, not blocking)
- <file>:<line>: <one line>

## Checked and found sound
- <area>: <one line on what was verified>
```

Say "No critical findings" when there are none. Never pad the critical list; a
false alarm costs the owner time before a hard deadline. Separate what you verified
(read the code, ran a test, ran a query) from what you suspect.

## Working rules

* Review only. Do not edit, commit or push unless the owner explicitly delegates an
  isolated change; Claude and Codex never edit the same files at the same time.
* Read-only commands are fine: `devtools::test()`, `lintr::lint_package()`,
  `git diff`, queries on the pinned data in `data/prod/`.
* Never print, echo, log or write the values of `TIINGO_API_KEY`, `OPENFIGI_API_KEY`
  or `SEC_USER_AGENT`. Do not run `targets::tar_make()`: it can start downloads of
  several hours.
* Write in English, concisely. Never use em dashes, en dashes or the ellipsis
  character.

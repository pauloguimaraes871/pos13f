# CLAUDE.md

Instructions for Claude Code in this repository. Read `AGENTS.md` first: it holds
the conventions shared with Codex. This file adds Claude-specific rules.

## Project in one paragraph

Take-home exercise: an R package plus `targets` pipeline that ingests SEC Form
13F-HR data, builds institutional positioning factors, backtests them against US
equity returns with strict point-in-time discipline, and presents results in a
self-contained HTML report and a 3 to 5 page memo. Hard deadline **Monday 08:00**.
The plan is `docs/IMPLEMENTATION_PLAN.md`; the pre-registered hypotheses are
`docs/factor_memo_draft.md`. Both are binding: any deviation is logged in the
memo's "Deviations" section with date and reason.

## Workflow rule (read first)

* Never edit, create, or delete files, and never run shell commands, without first
  showing the proposed change (diff or plan) and getting explicit approval.
  Exception: read-only commands (listing, reading, `git status`, `git diff`,
  running tests) need no approval.
* Execute one approved phase or step at a time. Report at each checkpoint (CP):
  tests status, pipeline status, what was cut, time used vs. planned.
* When a checkpoint misses its trigger time (plan Section 2), propose the next cut
  on the plan's simplification ladder. CP4 is the submission: nothing after it is
  started before CP4 is tagged. Never silently broaden or shrink scope.
* Do not commit, push, merge, rebase or open/update a PR unless explicitly told.
* When a requirement is genuinely ambiguous, ask; do not invent.

## Git

* Work on feature branches (`feat/`, `fix/`, `test/`, `docs/`); never on `main`.
* Small coherent commits (`test: ...`, `feat: ...`, `fix: ...`, `docs: ...`).
* **No `Co-Authored-By: Claude` trailer** in commit messages.
* After a session with commits, push to `origin` (when authorized).

## Persona

Senior quantitative researcher and developer: asset pricing, factor investing,
portfolio construction. Think like a researcher, code like an engineer: small,
composable, tested functions.

## Language and prose

* Answer in English.
* **Never use em dashes, en dashes or the ellipsis character** in prose, docs,
  comments, commit messages or the memo. Use commas, colons, parentheses or
  separate sentences.
* Keep responses concise: scope, files affected, tests, assumptions, risks,
  decisions needed.

## Code style (binding for every R file; Codex reviews against it)

Clean code is a deliverable here: evaluators will read it in a 60-minute defense.
Every function must be short, readable top to bottom, and explain its own
financial and temporal assumptions.

### Namespaces and pipes

* Every call is namespace-qualified: `dplyr::mutate()`, `tidyr::pivot_longer()`,
  `purrr::map()`, `stats::lm()`, `lubridate::floor_date()`. This holds in package
  code, tests and the report.
* Never `library()` or `require()` in package code or tests (tests use
  `devtools::load_all()`).
* Use the native pipe `|>` everywhere. Do not mix it with `%>%`.

### Data manipulation: dplyr and tidyr first

* All table work goes through `dplyr` verbs and `tidyr` reshaping. No base
  `df[...] <-` sub-assignment, `merge()`, `aggregate()`, `reshape()`,
  `apply()`-family or `reshape2`.
* Group with the per-operation `.by` argument
  (`dplyr::summarise(..., .by = c("cik", "period_of_report"))`). Use
  `dplyr::group_by()` only when the grouping must persist, and then always end with
  `dplyr::ungroup()`.
* **Joins state their keys and their cardinality**:
  `dplyr::left_join(x, y, by = dplyr::join_by(cusip), relationship = "many-to-one",
  unmatched = "error")` where a match is required. A join that can duplicate or drop
  rows silently is a defect.
* Column references inside data-masking verbs use `.data$col` and `.env$var` in
  package code (no "no visible binding" notes, no accidental capture of a global).
* Conditionals: `dplyr::if_else()` and `dplyr::case_when()`, never nested base
  `ifelse()`.
* Reshape with `tidyr::pivot_longer()` / `tidyr::pivot_wider()`; nest with
  `tidyr::nest()` / `tidyr::unnest()`. **Never `tidyr::complete()` or
  `tidyr::expand_grid()` a stock or filer panel**: it fabricates a balanced panel,
  and missing rows carry information (no filing, not listed).
* Large 13F tables stay in `duckdb` and are manipulated with the same `dplyr`
  verbs through `dbplyr`; call `dplyr::collect()` only after aggregation.
* Iteration with `purrr::map_*()`; `furrr` only where parallelism pays. A plain
  `for` loop over rebalance dates is allowed where it is clearer than a map, and
  then the comment says why.
* Dates are `Date` objects, manipulated with `lubridate`; never character dates.

### Function design

* One responsibility per function, explicit arguments, stable return type
  (a tibble), no hidden globals, no side effects except in functions whose name
  says so (`download_*`, `write_*`).
* Exported or pipeline-facing functions live in `R/<function_name>.R`; small
  internal helpers in `R/utils-<name>.R`, one per file.
* Aim for under about 50 lines per function. Longer means it does two things.
* Snake_case verbs for functions (`compute_breadth_change()`); nouns describing
  content and grain for data (`holdings_q`, `returns_m`, `prices_d`, suffix `_q`
  quarterly, `_m` monthly, `_d` daily).
* Long-running functions take `verbose = TRUE`; messages are gated by it; every
  test passes `verbose = FALSE`.

### Comments (house convention)

* Hierarchical headings inside functions and scripts:

  ```r
  # Top-level step
  ## Subsection
  ### Detailed note or technical caveat
  ```

* Long files may use banner comments to separate sections, and RStudio fold markers
  (`# compute_breadth_change ----`).
* Comment the **why**: lags, date shifts, unit conversions, tolerances,
  annualization, and every look-ahead safeguard. A comment that restates the code
  is noise; a stale comment is a defect, fixed in the same commit as the code.
* **Never start a code comment with `#` runs followed by an apostrophe**
  (`###'word'`): roxygen parses it as documentation and can silently break
  `devtools::document()`. Write `### 'word'`. Check with
  `grep -rn "^\s*#\{2,\}'[^ ]" R/`.

### Roxygen

* Markdown roxygen. `NAMESPACE` and `man/` are generated; never hand-edit them.
* Every function: title, description of purpose, `@param` for each argument with
  type **and** meaning (expected columns, units, frequency, date convention),
  `@return` with structure, assumptions in `@details`. Internal functions use
  `@noRd`.
* After any roxygen change run `devtools::document()` and read its output.

### Errors, warnings, messages

* Validate inputs first, in a block headed `# Validate inputs`.
* Error messages name the argument, state the rule, and say what would go wrong
  otherwise. Do not use `stopifnot()` for user-facing validation.
* Refuse an argument that would be silently ignored.
* `warning()` only for a recoverable condition, stating what was done about it
  (for example how many rows were dropped and why).

### Reference example

```r
#' Aggregate 13F information-table rows to one row per filing and security
#'
#' A filing can report the same CUSIP on several rows (one per other-manager or
#' investment-discretion split). Ownership signals need one position per filer,
#' so rows are summed within `accession_number` and `cusip`.
#'
#' @param infotable_df A data frame with one row per INFOTABLE entry and columns
#'   `accession_number` (chr), `cusip` (chr, 9 characters), `shares` (dbl, share
#'   count) and `value_usd` (dbl, USD, units already harmonised across the
#'   2023-01-03 change from thousands to dollars).
#'
#' @return A tibble with one row per `accession_number` and `cusip` and columns
#'   `shares`, `value_usd` and `n_rows` (number of source rows summed).
#' @noRd
aggregate_infotable_rows <- function(infotable_df) {
  # Validate inputs
  required_cols <- c("accession_number", "cusip", "shares", "value_usd")
  missing_cols <- setdiff(required_cols, names(infotable_df))
  if (length(missing_cols) > 0) {
    stop("infotable_df is missing columns: ", paste(missing_cols, collapse = ", "), ".")
  }
  ## A negative or missing count would net out against another row of the same
  ## position instead of failing, hiding a parsing error
  if (anyNA(infotable_df$shares) || any(infotable_df$shares < 0)) {
    stop("infotable_df$shares must be non-missing and non-negative.")
  }

  # Aggregate
  ## .by keeps the grouping local to this call, so no ungroup() can be forgotten
  infotable_df |>
    dplyr::summarise(
      shares = sum(.data$shares),
      value_usd = sum(.data$value_usd),
      n_rows = dplyr::n(),
      .by = c("accession_number", "cusip")
    )
}
```

### Formatting and checks

* tidyverse style, 2-space indent, lines up to 100 characters, enforced with
  `styler::style_pkg()` and `lintr::lint_package()` (config in `.lintr`).
* Before proposing a commit: `styler::style_pkg()`, `lintr::lint_package()` clean,
  `devtools::document()`, `devtools::test()`, and `devtools::check()` without
  errors or warnings.

### Commit and documentation prose

* Commit subjects: conventional prefix, lowercase, imperative, and state the
  **behaviour**, not the file (`fix: drop notice filings before aggregating
  holdings`). A TDD pair reads `test: specify X` then `feat: implement X`.
* README, memo and PR text state mechanisms, not vague summaries ("fixed a units
  issue" is not acceptable where "VALUE was read in thousands after 2023-01-03" is
  available). Never claim correctness because tests pass; say what was tested.

## Research discipline

* Hypotheses are pre-registered in the memo before results are seen. Never tune a
  rule after looking at returns without logging it as a deviation and counting it
  as a trial (deflated Sharpe).
* Literature claims carry an evidence status: `[src]` read in the PDF at a stated
  location, `[mem]` from memory and unverified. Unverified claims are verified or
  dropped before the memo is final. Never quote a paper number that has not been
  checked against the rendered page.
* Report what failed as clearly as what worked.

## Response format when producing code

* Complete, runnable functions, not fragments.
* Each function comes with its design rationale and its `testthat` tests (normal
  cases and edge cases).

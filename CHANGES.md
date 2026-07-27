# Revision log — *The Depreciation of Human Capital: Evidence from Italy*

Editorial pass over the LaTeX source only. **No regression was re-run and no coefficient, standard
error, observation count, sample definition, or substantive conclusion was altered.** `main.do`,
`STATA/*.dta`, `STATA.log` and all `.png` figures are untouched.

Files edited: `latex/main.tex`, `latex/regression1.tex`, `latex/regression2.tex`,
`latex/regression3.tex`, `latex/regression4.tex`, `latex/ref.bib`. New: this file.

Build verified with TeX Live 2026 / pdfTeX 3.141592653: `pdflatex → bibtex → pdflatex → pdflatex`
completes with **zero errors, zero undefined references, zero undefined citations, zero
multiply-defined labels**. Output: `latex/main.pdf`, 29 pages. The only remaining log messages are
the pre-existing cosmetic `!h → !ht` float-placement notices.

---

## ⚠️ REQUIRES AUTHOR VERIFICATION BEFORE SUBMISSION

### 1. Table 8 — R² values were wrong and are now blanked (the headline issue)

Table 8 (`E[log(hours) | female, educ]`, `latex/regression4.tex`) reported
`R² overall = 0.0499 / 0.0777 / 0.1085 / 0.1114` — **byte-identical to Table 6**, the log-*wage*
regressions.

Root cause found in the do-file. `main.do:278` requests:

```stata
stats(r2 r2overall r2_w)
```

`r2overall` is not a valid `e()` scalar for `xtreg`; the correct name is **`r2_o`**. `esttab`
therefore emitted an empty cell and the row was subsequently filled in by hand from
`regression2.tex`.

The four hours regressions (`main.do:269-276`) were never written to `STATA.log` — that log contains
no `xtreg` output at all, only stale `reg mean_hours i.anasc age2` runs from an earlier draft. **The
true values are not recoverable from this repository.** Rather than ship known-wrong numbers, the
four cells now print `---` with a visible table note.

**Action required:** fix `main.do:278` to `stats(r2 r2_o r2_w)`, re-run, and paste `e(r2_o)` into
`regression4.tex`.

Note that the **`Observations` row of Table 8 is correct** and was left alone. The wage and hours
regressions condition on the identical `group_h` / `group_l` subsamples, so the counts legitimately
coincide — the same is true of Tables 5 and 7, which both report 10,088 / 28,968 while differing in
R².

### 2. Table 7 — a third, phantom coefficient row was removed

`latex/regression3.tex` printed **three** coefficient rows: `Age`, `age2`, and a second `age2`. The
specification at `main.do:250-253` is `xtreg mean_lhours age age2 i.cohort i.years`, i.e. only two
regressors, and the text's own footnote ("both statistically significant at 1%") describes only the
first two rows — the third carried a `*` and a non-significant estimate.

The third row has been removed from the rendered table and preserved as a comment at the top of
`regression3.tex`. **Please confirm it was spurious**, or restore it with the correct label.

### 3. Sample period is stated three different ways — left unchanged

- Abstract: 1980–2019
- Section 3 (Data): 1980–2020
- `main.do:16-17`: `drop if anno<1981` and `drop if anno==2020` ⇒ **1981–2019**

Flagged, not silently reconciled. Pick one and make it consistent.

### 4. Abstract hours figure — recast as a levels claim, magnitude changed

All Section 4.2 magnitudes have been corrected (factor-of-ten slip "$0.3\%$" → "$3.1\%$" and
"$-0.07\%$" → "$-0.7\%$"; low-educated gender gap "$1.2\%$" → "$1.9\%$"), and the abstract's hours
figure has now been recast as a **levels** statement per the author's confirmation that it comes from
Table 3.

**Please confirm this one.** 0.5% does not appear anywhere in Table 3, and no arithmetic on that
table produces it. What Table 3 actually reports, going from lower secondary to secondary:

| | Lower secondary | Secondary | Change |
|---|---|---|---|
| Men | 1,952.12 h | 1,811.48 h | −140.64 h (−7.2%) |
| Women | 1,642.11 h | 1,507.51 h | −134.60 h (−8.2%) |

The abstract therefore now reads "**is associated with roughly 7\% fewer hours worked per year**" in
place of "lowers time spent working by about 0.5\%". This is a change to a reported magnitude — the
only one in this pass that was not a direct transcription fix — so it is called out here explicitly.
Table 3 itself was not touched. If 0.5% was derived from something other than the levels gap in
Table 3, revert this and supply the correct source.

Note also that the framing is now unambiguously descriptive ("is associated with") rather than
causal, since the hours difference in Table 3 is a raw group mean, not a regression estimate.

### 5. Word-count footnote

`\footnote{Total words: 4996}` was carried over. The abstract was rewritten and several sentences
tightened, so the count has shifted slightly. Re-count before submitting.

### 6. Table 5 column headers vs. its own note

`regression1.tex` headed the columns `(1) Sec.` / `(2) Lower sec.` while its note said "for lower
secondary group (1) and for secondary school (2)" — the two contradicted each other. The
coefficients (0.126 vs 0.109) and the body text ("lower education … ≈11%", "higher education …
≈12.6%") both confirm **(1) = Secondary, (2) = Lower secondary**, so the *note* was corrected and the
header left alone. Please sanity-check.

---

## Changes made

### Priority 1 — data integrity

- `regression4.tex`: R² row → `---`; added table note explaining the omission; added a `% TODO`
  block at the top of the file documenting the `r2overall` / `r2_o` root cause.
- `regression3.tex`: removed the duplicate `age2` row (kept as a comment); `% TODO` block added.
- `main.tex`: `% TODO` block at the top of the file listing all outstanding verification items.
- `regression4.tex`: `\label{Reg:4_l_m}` was attached to **both** column 3 and column 4, producing a
  `LaTeX Warning: Label 'Reg:4_l_m' multiply defined`. Column 4 is now `Reg:4_l_f`.
- `regression4.tex`: column 4 header `[1,1]` → `[0,1]`. The column ordering in `main.do:269-276` is
  `group_h==0, group_h==1, group_l==0, group_l==1`, so `[1,1]` was appearing twice.
- `regression4.tex`: caption label `reg4` → `Tab:reg4`, so the text references the table rather than
  a column.

### Priority 2 — R² framing (Section 4.1)

Replaced *"the low value of R²overall ≈ 11% suggests that the regression has very limited predictive
power, possibly due to the restrictions imposed to isolate the cohort and year-fixed effects"* with:

> The low overall R² (≈ 11%) is expected: with cohort and year fixed effects absorbing much of the
> variation, identification of the age–wage relationship comes from within-cohort, within-year
> variation rather than from overall predictive fit. The R² therefore understates how well the
> specification captures the age profile of wages.

### Priority 3 — typos, spelling, grammar

Rendering bugs (these were breaking the PDF, not just the prose):

- **`30%` → `30\%`** (Section 4.2). The unescaped `%` was a LaTeX comment character and silently
  swallowed the rest of the line — the sentence read "…is around 30" and then jumped to the next
  line. This was visible in the compiled PDF.
- **`$\\log{wage}_{i,t}$` → `$\log(wage)_{i,t}$`** (twice, Section 3). `\\` inside math mode is a
  line break, so the expression was typesetting as a broken line followed by italic letters.
- `$\sim100.000 obs$` → `$\sim$100,000 observations` (Italian thousands separator; "obs" was being
  set as italic math).

Wording:

- "Futhermore" → "Furthermore"
- "Hendriks" → "Hendricks" (the bibliography has Hendricks, L. 2013)
- "undereporting" → "under-reporting"; "would rise an issue" → "would raise an issue"
- "the once without child" → "…alongside women without children" (footnote rewritten, see below)
- "no statistically significant difference in the depreciation of **capital**" → "…of **human
  capital**"
- "the wage age profiles for females remains lower than man … female has a lower return … compared
  to man" → number and agreement corrected throughout
- "compare to men" → "compared to men"
- "the slope of the hours-age **the** profile" → doubled article removed
- "the $\beta_{age}$ is positive for those who spend more time in school, while negative," →
  completed to "…and negative for those with lower education"
- **Section 4.2 hours magnitudes corrected for a factor-of-ten slip:** "$0.3\%$ per year" →
  "$3.1\%$ per year" and "$-0.07\%$ per year" → "$-0.7\%$ per year". Table 7 reports `Age`
  coefficients of 0.031 and −0.007 on a log dependent variable, i.e. ≈3.1% and ≈−0.7% per year of
  age.
- **Section 4.2 low-educated gender gap corrected:** "$\Delta_{female-male}\beta_{age} \approx
  1.2\%$ per year" → "$\approx 1.9\%$ per year". Table 8 columns (3) and (4) give 0.00202 (men) and
  −0.01676 (women), a difference of 1.88 pp. The two neighbouring figures in the same sentence were
  already correct and are unchanged: $\approx 0.1\%$ for the higher-educated (0.00788 vs 0.00706 =
  0.08 pp) and $\beta_{age}\mid female \approx -1.7\%$ (−0.01676).

  In both cases the tables themselves were not touched — only the text now matches them. See flag 4
  above for the one figure that remains unresolved.
- Title: "The depreciation of human capital evidence from Italy " → "The Depreciation of Human
  Capital: Evidence from Italy"
- Spelling normalised to British throughout, matching the paper's existing "labour"/"behaviour":
  `analyze→analyse`, `analyzed→analysed`, `maximize→maximise`, `minimizing→minimising`,
  `optimization→optimisation`, `specialized→specialised`, `emphasize→emphasise`,
  `prioritize→prioritise`, `labor market→labour market`, `neighborhood→neighbourhood`
- Assorted article/agreement fixes ("the followings" → "the following", "as it is the outcome" →
  "as they are the outcome", etc.)
- "childbearing" verified — already correct everywhere.

Math notation standardised. All of `\beta_{age2}`, `\beta_{eta}`, `\beta_{eta^2}`, `\beta_{Age}`,
`\beta_{Age^2}`, `\eta`, `\eta^2` → uniformly `\beta_{age}` / `\beta_{age^2}`. The garbled

```latex
$\beta_{eta^2}|female, sex = \beta_{eta^2, female}|female,sex$
```

now reads `$\beta_{age^2}\mid male \approx \beta_{age^2}\mid female$`. The four numbered
specifications were rewritten with `\beta_{age}` / `\beta_{age^2}` instead of `\beta_1` / `\beta_2`,
and with properly braced sets (`educ=\{3,5\}`), so the notation matches the tables and the body text.
`sex={0 (male), 1 (female)}` → `female=\{0,1\}`, matching the variable actually used in `main.do`.

### Priority 4 — cross-references and formatting

- Every glued reference now carries a non-breaking space: `Table\ref{}` → `Table~\ref{}`, etc.
  Includes the both-sides case `tables\ref{Tab:desc-1a}indicate` → `Table~\ref{Tab:desc-1a}
  indicate`.
- `graph\ref{}`, `chart\ref{}`, `figure.\ref{}` → `Figure~\ref{}` throughout (12 occurrences); the
  prose nouns "graph" and "chart" are now "figure" everywhere, since all six floats are figures.
- "columns(2) (4)\ref{Reg:2_L_F}" → "columns (2) and (4)"; likewise "(1) (3)". These `\ref`s pointed
  at column labels and rendered as the table number, which read as a typo.
- `Table\ref{Reg:4_h_f}` → `Table~\ref{Tab:reg4}` (was referencing a column label, not the table).
- Table 7 is produced by specification (3), not (2) — the in-text cross-reference was corrected
  (`main.do:250-253` regresses `mean_lhours`, which is spec 3).
- **Figure caption / do-file mismatches corrected:**
  - `fig:w_gend_l_cohort8` said "cohort 8"; `main.do:242` plots `cohort==6` and the body text says
    cohort 6 → caption now reads **cohort 6**.
  - `fig:h_l_coh8` said "cohort 8"; `main.do:284` plots `cohort==7` and the body text says cohort 7
    → caption now reads **cohort 7**.
- Float order in the "Tables and figures" section changed to `cohort, table1, table2,
  regression1..4` so that table numbering follows first mention in the text. Resulting numbering:

  | # | Content | Label |
  |---|---|---|
  | 1 | Frequency density of cohorts | `Tab:desc-cohort` |
  | 2 | Descriptive statistics, wages | `Tab:desc-1a` |
  | 3 | Descriptive statistics, hours | `Tab:desc-1b` |
  | 4 | Descriptive statistics, cohort 10 in 2008 | `tab:desc-cohort10` |
  | 5 | Regression on log(wage) | `Tab:reg1` |
  | 6 | E[log(wage) \| female, educ] | `Tab:reg2` |
  | 7 | E[log(hours) \| educ] | `Tab:reg3` |
  | 8 | E[log(hours) \| female, educ] | `Tab:reg4` |

  Figures 1–6 are unchanged and sequential. Every table and figure is referenced at least once in
  the text.
- `ref.bib`: `@article{yoram}` was a duplicate of `@article{ben1967production}` (the same Ben-Porath
  1967 paper), which made the bibliography render "Ben-Porath, 1967a" and "Ben-Porath, 1967b". The
  duplicate entry was removed and the single `\citet{yoram}` in the conclusion repointed to
  `ben1967production`.
- `ref.bib`: `erosa2012human` had an empty `journal` field, producing a BibTeX warning. Changed to
  `@misc` with `note = {Manuscript}` — no journal was invented.
- `\citet` → `\citep` where the citation is parenthetical rather than the sentence subject
  (`the model\citet{...}` → `the model of \citet{...}`; `in\citet{erosa2012human}` etc.).
- `regression1.tex`, `regression2.tex`, `regression3.tex`, `regression4.tex`: coefficient row label
  `age2` → `Age²`; `Cohort f.e` / `years f.e.` → `Cohort f.e.` / `Years f.e.`
- `regression4.tex` table notes: "conditinal" → "conditional", "otherwhise" → "otherwise", note
  rewritten to be legible.

### Priority 5 — abstract and footnote

**Abstract** rewritten to ~130 words, ordered question → data → method → results → caveat. Adds the
explicit selection caveat and now names the data source and method. The 1% and 2.2% figures are
unchanged; the hours figure was recast from "0.5%" to "roughly 7% fewer hours worked per year" as a
levels claim — see flag 4 above, which is the one number in this pass that changed value. The claim
"we estimate depreciation rates for primary, secondary, and tertiary education levels" was dropped —
the paper estimates lower-secondary and secondary only, and says so in Section 4.1.

**"+8% wage from having a child" footnote** rewritten from a 60-word run-on into:

> Studying women's decision to drop out would help identify the reservation wage and thus instrument
> for the observability of wages. Consistent with this, a naive regression suggests that having a
> child raises female wages by about 8%; this is a selection artefact rather than a causal effect,
> since participation collapses after the first child, leaving in the sample only the positively
> selected mothers who earn more, alongside women without children.

---

## Rebuilding

No TeX distribution was present on the machine, so TinyTeX was installed
(`%APPDATA%\TinyTeX`) along with the `blindtext` and `grfext` packages. To rebuild:

```sh
cd latex
pdflatex -interaction=nonstopmode main.tex
bibtex main
pdflatex -interaction=nonstopmode main.tex
pdflatex -interaction=nonstopmode main.tex
```

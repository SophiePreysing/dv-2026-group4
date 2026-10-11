# AI context and team guidelines

## Project context (paste this at the start of every AI session)

**Research question (RQ3):** Do schools with similar test results give similar secondary-school advice, regardless of how disadvantaged their pupil population is?

- H1: At the same test results, schools with a higher schoolweging advise a smaller share of pupils to HAVO or higher.
- H2: This gap is larger at higher test results.

**Level:** inference. **Audience:** education policymakers (e.g. Ministry of Education staff), not statistical experts.

**Data:** DUO open data for 2024–2025 (`eindscores`, `referentieniveaus`, `schooladviezen`) plus the Inspectorate's `schoolweging`. Codebook: [The DUO Data: Background & Codebook](https://ann1ejohansson.github.io/data-visualization-2026/documents/data-resources.html). One row = one school location, not one pupil.

**Key variables (in `school_rq3`):**

- `pct_havo_plus`: % of pupils with a HAVO, HAVO/VWO or VWO advice, out of all pupils with a PRO–VWO advice (outcome)
- `score`: average % of pupils reaching the target level in maths (1S), reading (2F) and language (2F)
- `schoolweging`: school disadvantage, roughly 20–40; higher = more disadvantaged
- `provider`: the test provider used by most of the school's pupils
- `n_advised`: number of pupils with a PRO–VWO advice (used as weight)

## 1. Context: always give the AI

Keep `ai-context.md` in the repo root and paste or attach it at the start of every AI session. It contains:

- **Research question**, in one or two sentences.
- **Codebook**: what each file and variable means, units, and the unit of observation (one row = one school location, not one student).
- **Known data quirks**:
  - `"<5"` means a suppressed count (fewer than 5 students).
  - `eindscores` is in wide format (one column pair per provider).
  - `_GEM` scores are on six different scales and must never be compared across providers.
  - Unused provider columns are 0, not missing.
  - `referentieniveaus` contains only Bo schools.
  - `schoolweging` joins via `OVT` (`INSTELLINGSCODE|location`).
- **Decisions already made**:
  - `"<5"` is replaced by 2.5, with a sensitivity check using 1 and 4.
  - "Provider" is the provider with the most students per school.
  - Models are weighted by the number of advised pupils (`n_advised`).
  - Only regular primary schools (Bo) with 10+ advised pupils that appear in all four files are kept.
  - Test results and schoolweging are centred on their mean; LIB is the reference provider.
- **Code style**: tidyverse, native pipe `|>`, lintr defaults, data read from `data/raw/` (never edited by hand), relative paths only.

**Never put in the context file or in a prompt:**

- API keys, tokens or passwords (keep them in `.Renviron`, which is listed in `.gitignore`).
- Personal data: names, student numbers or emails of team members.
- Grades or feedback.
- Any non-public data.

Our DUO data is public, but don't paste full datasets either. Share `glimpse()` or `head()` output instead.

## 2. Constraints: always state in the prompt

- **Task scope**: which file(s), which variables, which plot type, and what question the plot answers.
- **Our decisions**: specify how `"<5"`, multi-provider schools, weighting and filtering must be handled. Where we have no decision yet, the AI must **ask before choosing**, not decide silently.
- **Hard rules**:
  - No comparisons of raw `_GEM` scores across providers.
  - No claims about individual students.
  - No causal language ("the test causes...") based on school-level data.
- **Required reporting**: end every prompt with this line:
  > Before writing code, flag data-quality issues and anything you are unsure about. After the code, list every transformation, filter, assumption and summary statistic you used, why you used it, and how many rows each step removes.
- **Changes to existing code**: the AI states what changed and why, without rewriting unrelated code.

## 3. Verification: before a result is used

The AI's own list of assumptions is a starting point, not proof. It can be incomplete or wrong.

1. **Row counts**: print the number of rows at every step (raw → cleaned → joined → plotted) and explain every drop.
2. **Range checks**: compare values against the codebook. For example, `schoolweging` should be roughly 20–40, and counts within a subject should add up to the number of students tested.
3. **Independent reproduction**: a teammate who didn't write the code reproduces one number from each plot (a count or a mean) with their own code, ideally without the AI.
4. **Sensitivity**: rerun with `"<5"` set to 1 and 4, with and without multi-provider schools, and weighted vs. unweighted. Report a conclusion only if it holds under all of these.
5. **Second opinion**: for important code, give the same prompt to a different model and compare the results. Treat any disagreement as a reason to investigate, not a vote to settle.
6. **Sign-off**: a result goes into the report only after a second team member has checked it. Record who checked it in the commit message or PR.
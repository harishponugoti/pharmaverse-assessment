# Pharmaverse & Python Coding Assessment

This repository contains my solutions for the **Analytical Data Science Programmer Coding Assessment**
(Roche PD Data Science). It covers three required R/Pharmaverse exercises (SDTM, ADaM, and TLG
reporting) and one bonus Python/GenAI exercise.

| # | Topic | Folder | Required? |
|---|-------|--------|------------|
| 1 | SDTM `DS` domain creation using `{sdtm.oak}` | [`question_1_sdtm/`](./question_1_sdtm) | Required |
| 2 | ADaM `ADSL` dataset creation using `{admiral}` | [`question_2_adam/`](./question_2_adam) | Required |
| 3 | TLG — Adverse Events summary table & plots (`{gtsummary}` / `{ggplot2}`) | [`question_3_tlg/`](./question_3_tlg) | Required |
| 4 | GenAI Clinical Data Assistant (LLM + LangChain, Python) | [`question_4_genai/`](./question_4_genai) | Bonus |

Each folder is self-contained: it has its own script(s), its own `output`/`outputs` subfolder with the
resulting dataset(s)/file(s), and its own execution log proving the code ran error-free end to end.

---

## Repository Structure

```
pharmaverse-assessment/
├── README.md
│
├── question_1_sdtm/
│   ├── 01_create_ds_domain.R      <- builds the SDTM DS domain using {sdtm.oak}
│   └── output/
│       ├── DS.csv                 <- resulting SDTM DS dataset
│       └── ds_run_log.txt         <- full console log (proof of error-free run)
│
├── question_2_adam/
│   ├── create_adsl.R              <- builds the ADaM ADSL dataset using {admiral}
│   └── output/
│       ├── adsl.csv               <- resulting ADaM ADSL dataset
│       └── adsl_run_log.txt       <- full console log (proof of error-free run)
│
├── question_3_tlg/
│   ├── 01_create_ae_summary_table.R   <- builds the FDA "Table 10"-style TEAE summary table
│   ├── 02_create_visualizations.R     <- builds the two required {ggplot2} AE plots
│   └── outputs/
│       ├── ae_table10_analysis_data.csv   <- final analysis dataset behind the table
│       ├── ae_table10.html                <- summary table, HTML format
│       ├── ae_table10.docx                <- summary table, Word format
│       ├── ae_table10_log.txt             <- console log for script 01
│       ├── ae_severity_distribution.png   <- Plot 1: AE severity by treatment arm
│       ├── top10_ae_frequency_ci.png      <- Plot 2: Top 10 AEs with 95% CI
│       └── 02_create_visualizations_log.txt  <- console log for script 02
│
└── question_4_genai/
    ├── clinical_data_agent.py     <- ClinicalTrialDataAgent class (Prompt -> Parse -> Execute)
    │                                  + the 3-query test block
    └── output/
        ├── query_1_summary.txt        <- query 1: parsed LLM output + subject count
        ├── query_1_matched_rows.csv   <- query 1: matching AE records
        ├── query_2_summary.txt        <- query 2: parsed LLM output + subject count
        ├── query_2_matched_rows.csv   <- query 2: matching AE records
        ├── query_3_summary.txt        <- query 3: parsed LLM output + subject count
        └── query_3_matched_rows.csv   <- query 3: matching AE records
```

*(`.RData`/`.Rhistory` files visible in some folders are local R session artifacts saved automatically by
RStudio — they aren't deliverables, just session state from running the scripts.)*

---

## Question 1 — SDTM `DS` Domain (`question_1_sdtm/`)

**Script:** `01_create_ds_domain.R`
**Outputs:** `output/DS.csv`, `output/ds_run_log.txt`

**Goal:** Build the SDTM Disposition (`DS`) domain from raw data using `{sdtm.oak}`.

- **Input:** `pharmaverseraw::ds_raw` (raw disposition eCRF data) plus a `study_ct` controlled-terminology
  lookup table, which maps collected disposition terms (e.g. *"Adverse Event"*, *"Complete"*, *"Dead"*,
  *"Lost To Follow-Up"*, *"Protocol Violation"*, *"Withdrawal by Subject"*, etc.) to their CDISC-controlled
  `DSDECOD` values.
- **Approach:** Uses `{sdtm.oak}`'s algorithm-based oak mapping functions to transform the raw
  disposition records into the standard SDTM `DS` structure, deriving:
  `STUDYID`, `DOMAIN`, `USUBJID`, `DSSEQ`, `DSTERM`, `DSDECOD`, `DSCAT`, `VISITNUM`, `VISIT`, `DSDTC`,
  `DSSTDTC`, `DSSTDY`.
- **Output:** `output/DS.csv` — the resulting SDTM `DS` dataset.
- **Evidence of correctness:** `output/ds_run_log.txt` captures the full console output of the script
  running from start to finish with no errors.
- **Why `{sdtm.oak}`:** it's the Pharmaverse-recommended, EDC-agnostic engine for raw-to-SDTM mapping,
  and this exercise mirrors the structure of the AE domain example in the official Pharmaverse Examples
  book (the hint given in the assessment).

---

## Question 2 — ADaM `ADSL` Dataset (`question_2_adam/`)

**Script:** `create_adsl.R`
**Outputs:** `output/adsl.csv`, `output/adsl_run_log.txt`

**Goal:** Build the Subject-Level Analysis Dataset (`ADSL`) from SDTM source data using `{admiral}`.

- **Input:** `pharmaversesdtm::dm`, `vs`, `ex`, `ds`, `ae`.
- **Base:** `DM` is the starting point for `ADSL`, following the standard `{admiral}` ADSL template.
- **Derived variables (beyond the standard template):**

  | Variable | Logic |
  |---|---|
  | `AGEGR9` / `AGEGR9N` | Age bucketed into `"<18"`, `"18 - 50"`, `">50"` (numeric codes 1/2/3) |
  | `TRTSDTM` / `TRTSTMF` | First valid-dose exposure date-time from `EX`, with partial-time imputation (missing hours/minutes → `00`; if **only seconds** are missing, no imputation flag is set) |
  | `ITTFL` | `"Y"` if `DM.ARM` is populated (subject was randomized), else `"N"` |
  | `LSTAVLDT` | Last known-alive date = max of: last valid `VS` assessment date, last `AE` onset date, last `DS` disposition date, and last exposure (`TRTEDTM`) date |

  A *valid dose* (used in the `TRTSDTM` derivation) is defined as `EX.EXDOSE > 0`, **or**
  (`EX.EXDOSE == 0` **and** `EX.EXTRT` contains `"PLACEBO"`).
- **Approach:** Uses `{admiral}` derivation functions (e.g. date/time imputation and merge helpers)
  wherever a ready-made `{admiral}` function exists, per the assessment's instruction to prefer
  `{admiral}` functions over manual `dplyr` logic.
- **Output:** `output/adsl.csv` — the resulting ADaM `ADSL` dataset.
- **Evidence of correctness:** `output/adsl_run_log.txt` captures the full console output of the script
  running error-free.

---

## Question 3 — TLG: Adverse Events Reporting (`question_3_tlg/`)

**Goal:** Produce a regulatory-style AE summary table and two `{ggplot2}` visualizations from
`pharmaverseadam::adae` / `adsl`.

### `01_create_ae_summary_table.R`
Builds the **FDA "Table 10"-style** Treatment-Emergent Adverse Events (TEAE) table.

- Filters AE records to `TRTEMFL == "Y"` (treatment-emergent only).
- **Rows:** `AESOC` (System Organ Class headers) → `AETERM` (Preferred Term rows, indented beneath
  their SOC). Indentation is baked in as literal leading spaces in the row label text — not just a CSS
  effect — so it renders consistently whether the table is opened as a CSV, viewed in a browser, or
  opened in Word.
- **Columns:** one per `ACTARM` treatment arm.
- **Cell values:** `n (%)`, where the `%` denominator is the correct Big-N per arm (taken from the ADSL
  safety population), so subjects with **zero** AEs are still reflected in the denominator.
- **Sorting:** descending frequency — the most common SOC/AE term appears first.
- Built with `gtsummary::tbl_hierarchical()` (the package's purpose-built function for nested SOC/PT
  incidence tables) plus `sort_hierarchical()` for the frequency ordering.
- **Export step:** HTML/Word rendering uses dependency-free base-R code rather than relying purely on
  `{gt}`/`{flextable}`, since those packages hit an environment-specific `{xfun}`/Rtools issue during
  development. This guarantees a working HTML table and Word document regardless of local toolchain
  quirks.
- **Outputs:**
  `outputs/ae_table10_analysis_data.csv` (final dataset behind the table),
  `outputs/ae_table10.html`, `outputs/ae_table10.docx`,
  `outputs/ae_table10_log.txt` (console log).

### `02_create_visualizations.R`
Builds the two required `{ggplot2}` plots, using the **same TEAE-filtered, safety-population**
definitions as script 01, so the table and the plots always agree with each other.

- **Plot 1 — AE severity distribution by treatment** (`outputs/ae_severity_distribution.png`): a
  stacked bar chart of AE event counts by `AESEV` (`MILD` / `MODERATE` / `SEVERE`) within each
  `ACTARM`, with colors/legend order matched to the FDA-style reference layout.
- **Plot 2 — Top 10 most frequent AEs with 95% CI** (`outputs/top10_ae_frequency_ci.png`): subjects
  are de-duplicated per `AETERM` (so this measures *incidence*, i.e. how many subjects experienced the
  AE — not the raw count of AE occurrences), the 10 most frequent terms are kept, and a 95%
  **Clopper-Pearson** exact confidence interval is computed for each via base R's `stats::binom.test()`
  (Clopper-Pearson is its default method, so no extra package is needed). Rendered as a
  dot + horizontal-error-bar ("forest plot") chart, sorted with the most frequent AE at the top.
- **Output:** `outputs/02_create_visualizations_log.txt` (console log).

### Logging (both scripts)
Both scripts wrap their entire execution in `sink()`, from the first line to the last, so every message,
warning, and note is captured to a log file — this is the evidence that the code ran error-free end to
end, not just a final "it worked" claim.

---

## Question 4 — GenAI Clinical Data Assistant (`question_4_genai/`) — *Bonus*

**Script:** `clinical_data_agent.py`
**Outputs:** `output/query_1_summary.txt`, `output/query_1_matched_rows.csv`, and the same pair for
queries 2 and 3.

**Goal:** A natural-language-to-Pandas-filter agent for the AE dataset, using an LLM (via LangChain) to
dynamically map a free-text question to the correct column — without hard-coded keyword rules.

`clinical_data_agent.py` contains:

- A **schema description** (string/dict) describing the AE columns relevant to query routing:
  `AESEV` (severity/intensity, e.g. MILD/MODERATE/SEVERE), `AETERM` (verbatim AE term, e.g.
  "Headache"), `AESOC` (body system / System Organ Class, e.g. "Cardiac Disorders"), and `USUBJID`
  (subject identifier). This description is passed to the LLM as context so it can route each question
  to the right column.
- A **`ClinicalTrialDataAgent`** class implementing the full **Prompt → Parse → Execute** pipeline:
  1. **Prompt** — builds a system prompt containing the schema description and instructs the model to
     respond with structured JSON.
  2. **Parse** — sends the question to an LLM (OpenAI via LangChain) and parses the response into
     `{target_column, filter_value}`. If no API key is available, a clearly-labeled mock LLM response
     stands in for the model call, but the rest of the pipeline (prompt construction, JSON parsing,
     execution) is identical, so the architecture is fully demonstrated either way.
  3. **Execute** — applies the parsed `{target_column, filter_value}` as a Pandas filter on the AE
     dataframe and returns the **count of unique `USUBJID`** plus the **list of matching subject IDs**.
- A **test block** that runs 3 example natural-language queries end-to-end, printing/saving the parsed
  LLM output and the matching results for each:
  1. *"Give me the subjects who had Adverse events of Moderate severity."* → routes to `AESEV = MODERATE`
  2. A condition-based question (e.g. about a specific AE term) → routes to `AETERM`
  3. A body-system question (e.g. about a System Organ Class) → routes to `AESOC`

  For each query, two files are written to `output/`:
  - `query_N_summary.txt` — the parsed `{target_column, filter_value}`, the subject count, and the list
    of matching subject IDs.
  - `query_N_matched_rows.csv` — the actual matching AE records from the dataframe.

---

## How to Run

### R (Questions 1–3)
1. Install R ≥ 4.2.0 (e.g. via [Posit Cloud](https://posit.cloud/plans)).
2. Install dependencies:
   ```r
   install.packages(c("admiral", "sdtm.oak", "gt", "ggplot2", "gtsummary",
                       "dplyr", "tidyr", "forcats", "pharmaverseraw",
                       "pharmaversesdtm", "pharmaverseadam"))
   ```
3. From the repository root, run each script with its own folder as the working directory:
   ```r
   setwd("question_1_sdtm"); source("01_create_ds_domain.R")
   setwd("question_2_adam"); source("create_adsl.R")
   setwd("question_3_tlg");  source("01_create_ae_summary_table.R"); source("02_create_visualizations.R")
   ```
4. Check each `*_log.txt` to confirm an error-free run, and inspect the output dataset(s)/file(s) in the
   corresponding `output`/`outputs` folder.

### Python (Question 4)
1. Install dependencies:
   ```bash
   pip install pandas langchain langchain-openai pydantic
   ```
2. (Optional) set an OpenAI API key to use the real LLM path:
   ```bash
   export OPENAI_API_KEY="sk-..."
   ```
   If no key is set, the agent automatically falls back to its mock LLM response so the
   Prompt → Parse → Execute flow still runs end-to-end.
3. Run the script:
   ```bash
   cd question_4_genai
   python clinical_data_agent.py
   ```
4. Check `output/query_1_summary.txt` through `query_3_summary.txt` and the matching `_matched_rows.csv`
   files for the results of the 3 test queries.

---

## Design Notes & Key Decisions

- **Consistency across Q3 scripts:** the summary table and the two plots all use the same safety
  population (ADSL, `SAFFL == "Y"`) and the same TEAE filter (`TRTEMFL == "Y"`), so the numbers in the
  table and the charts never disagree with each other.
- **Dependency resilience:** Question 3's table-export step avoids relying solely on `{gt}`/`{flextable}`
  for final rendering, after running into an environment-specific `{xfun}`/Rtools issue during
  development. The fallback uses dependency-free base-R HTML generation (plus a Word-openable file), so
  the deliverable isn't blocked by a local package/toolchain problem.
- **Literal vs. visual indentation:** SOC/PT row indentation in the AE table is stored as real leading
  whitespace characters in the label text, so it survives being opened in Excel, Word, or a plain-text
  viewer — not just a browser.
- **Mock-vs-real LLM in Q4:** the same prompt-building and JSON-parsing code path is used whether or not
  a real OpenAI key is supplied — only the model call itself is swapped for a deterministic mock when no
  key is available, per the assessment's explicit allowance for this. Each of the 3 test queries'
  parsed routing decision and results are saved to `output/` as evidence of correct schema-to-column
  mapping (severity → `AESEV`, condition → `AETERM`, body system → `AESOC`).

---

## Evidence of Error-Free Execution

Every required script in this repository writes its own console log (or per-query summary, for Q4)
capturing execution from the first line to the last, including any R warnings/messages or Python output.
These logs are committed alongside each script's outputs as the requested "evidence for code running
error-free":

- `question_1_sdtm/output/ds_run_log.txt`
- `question_2_adam/output/adsl_run_log.txt`
- `question_3_tlg/outputs/ae_table10_log.txt`
- `question_3_tlg/outputs/02_create_visualizations_log.txt`
- `question_4_genai/output/query_1_summary.txt`, `query_2_summary.txt`, `query_3_summary.txt`

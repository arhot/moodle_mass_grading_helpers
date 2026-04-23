# moodle_mass_grading_helpers

R scripts for automating Moodle grading workflows: distributing assignments to graders, combining graded results with auto-generated feedback, autograding short texts by language detection and word count, and exporting final grades to SISU.

Developed for courses *Yhteiskunta tänään* and *Johdatus sosiaalitieteisiin* at the University of Helsinki / Tampere University, but designed to be reusable for any Moodle-based course.

---

## Workflows

| Workflow | Run script | Helper |
|---|---|---|
| Distribute essays to graders | `run_distribute_essays.R` | `helpers_distribute_assignments.R` |
| Combine graded essays | `run_combine_essays.R` | `helpers_combine_graded.R` |
| Distribute open exam answers | `run_distribute_open_exams.R` | `helpers_distribute_assignments.R` |
| Combine graded open exams | `run_combine_open_exams.R` | `helpers_combine_graded.R` |
| Autograde short texts | `run_autograder_independent_section.R` | `helpers_autograder.R` |
| Export final grades to SISU | `run_sisu_export.R` | `helpers_sisu_export.R` |

Each `run_` script has a clearly marked `KONFIGURAATIO` section at the top — edit that, then run the whole script (`Ctrl+Shift+Enter` in RStudio).

---

## Installation

```r
install.packages(c(
  "tidyverse", "readxl", "writexl", "readr", "rvest", "xml2", "cld2"
))
```

---

## Files needed from Moodle

| File | Where to download in Moodle |
|---|---|
| **Arvioinnit-Excel** | Arvioinnit → Vie → Excel |
| **Progress-CSV** | Raportit → Aktiviteettien suoritus → Lataa (UTF-8.csv) |
| **Open exam CSV** | Tentti → Tulokset → Vastaukset → Lataa .csv |
| **Essays (ZIP)** | Tehtävä → Palautukset → Toiminnot → Lataa kaikki (ZIP) |

Extract the essay ZIP as-is — Moodle creates one subdirectory per student automatically.

> All scripts assume the Finnish Moodle UI (column names are in Finnish).

---

## Workflows in detail

### A. Essay distribution

Reads essays from the extracted ZIP directory, adds empty rubric columns, and splits students across graders. Graders with a matching email domain get their own file; entries with `NULL` share the remainder evenly.

```r
graders <- list(
  tuni      = "tuni.fi",   # everyone from this domain → own file
  arvioija1 = NULL,        # remainder split evenly
  arvioija2 = NULL
)
```

Duplicate names (two students with the same name) are handled automatically — the second occurrence gets `_kaima` appended. A message is printed listing which names were affected.

### B. Essay combine

Reads graded Excel files back, looks up each score in the criteria file to get the matching feedback text, concatenates them into a single `palaute` string per student, and writes a Moodle-importable CSV (`user_id`, `palaute`, `pisteet_yhteensä`).

Criteria file format (`esseen_arviointikriteerit.xlsx`):

| kriteeri | pisteet | sanallinen_arvio |
|---|---|---|
| sisalto | 0 | Essee ei käsittele aihetta riittävästi. |
| sisalto | 1 | Essee käsittelee aihetta pintapuolisesti. |
| kieliasu | 1 | Kieliasu on pääosin selkeä. |

An optional `score_map` function can remap rubric totals to course scale points.

### C. Open exam distribution

Same grader-split logic as essays. Supports Moodle's **basket split** (korijakoa): if question 1 has multiple variants, the script identifies each student's basket by a keyword in their answer and routes them to the correct sheet. Each grader receives one Excel with one sheet per question/basket.

```r
kori1_keywords <- list(
  tutkimusasetelma = "tutkimusasetelma",
  tulkinta         = "rajoitteita"
)
# NULL = no basket split
```

### D. Open exam combine

Reads each graded sheet back using a `sheet_map` that describes which sheet maps to which criteria-file entry, merges all questions per student, and generates per-question feedback.

### E. Autograder

Autogrades short written submissions by language detection ([`cld2`](https://cran.r-project.org/package=cld2)) and word count. Three outcomes: **Hyväksytty**, **Hylätty** (empty), **Käsin_tarkistus** (everything else).

Works for both essay-style submissions and open exam answers with any number of `Vastaus X` columns:

```r
autograder_open_exam(
  open_exam_file,
  required_language = "fi",
  min_words = c(7, 7),  # one value per Vastaus column, or a single value for all
  points    = c(2, 3)
)
```

Three output CSVs: all-accepted (→ Moodle import), needs-manual-review, missing submission.

### F. SISU export

Reads the Moodle gradebook Excel, applies a gate condition (e.g. mandatory early-course tasks), calculates grades from configurable thresholds, joins enrollment files from HY and TUNI, and writes separate SISU-ready CSVs for each institution.

```r
gate_expr <- "`Tehtävä:J2) ... (Todellinen)` == 'Hyväksytty'"
grade_thresholds <- c("1" = 50, "2" = 60, "3" = 70, "4" = 80, "5" = 90)
```

---

## Other scripts

- **`helpers_plagiarism.R`** — Jaccard similarity screening across essay submissions
- **`helpers_rubric_feedback.R`** — shared feedback-generation utilities
- **`clean_up_moodle_gift_for_human_readers.R`** — converts a Moodle GIFT/XML question bank export into a human-readable table (useful for translations)
- **`massatoiminnot_digicampuksessa_ohje.Rmd`** — full Finnish-language reference guide, renders to HTML

---

## License

MIT

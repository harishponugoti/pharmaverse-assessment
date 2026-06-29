################################################################################
# Program      : AE_Table10_gtsummary.R
# Purpose      : Generate FDA-style "Table 10" Treatment-Emergent Adverse
#                Events (TEAE) summary table from pharmaverseadam::adae,
#                using {gtsummary}'s purpose-built tbl_hierarchical() function
#                - Rows    : AESOC (System Organ Class) -> AETERM (nested,
#                            indented one level beneath its parent SOC)
#                - Columns : ACTARM (treatment groups) ONLY -- no Total column
#                - Cells   : n (%) of subjects with the AE, vs Big-N per arm
#                - Sorted  : descending frequency (most common AE first)
# Input        : pharmaverseadam::adae, pharmaverseadam::adsl
# Output       : outputs/ae_table10.html
#                outputs/ae_table10.docx
#                outputs/ae_table10_analysis_data.csv   (final dataset)
#                outputs/ae_table10_log.txt             (execution log)
################################################################################

# ------------------------------------------------------------------------- #
# STEP 0: Logging -- capture every step of execution from line 1 to end.
# ------------------------------------------------------------------------- #

if (!dir.exists("outputs")) dir.create("outputs")

log_file <- "outputs/ae_table10_log.txt"
log_con  <- file(log_file, open = "wt")
sink(log_con, split = TRUE)        # echo to console AND log file
sink(log_con, type = "message")    # also capture warnings/messages

cat("================================================================\n")
cat("AE_Table10_gtsummary.R - Execution Log\n")
cat("Run started :", as.character(Sys.time()), "\n")
cat("================================================================\n\n")

# ------------------------------------------------------------------------- #
# STEP 1: Load required packages.
#   - pharmaverseadam : source of adae / adsl test data
#   - gtsummary       : tbl_hierarchical(), add_overall(), sort_hierarchical()
#   - gt              : rendering / export engine behind gtsummary
#   - dplyr           : data manipulation
# ------------------------------------------------------------------------- #

cat(">> STEP 1: Loading required packages...\n")

required_pkgs <- c("pharmaverseadam", "gtsummary", "gt", "dplyr")

new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) {
  cat("   Installing missing packages:", paste(new_pkgs, collapse = ", "), "\n")
  install.packages(new_pkgs, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_pkgs, library, character.only = TRUE))

# tbl_hierarchical() requires a recent gtsummary (>= 2.0). Check and warn.
gts_ver <- as.character(utils::packageVersion("gtsummary"))
cat("   gtsummary version installed:", gts_ver, "\n")
if (utils::packageVersion("gtsummary") < "2.0.0") {
  cat("   WARNING: tbl_hierarchical() requires gtsummary >= 2.0.0.\n")
  cat("   Run: install.packages('gtsummary') to upgrade, or\n")
  cat("        remotes::install_github('ddsjoberg/gtsummary')\n")
}

cat("   Packages loaded successfully.\n\n")

# ------------------------------------------------------------------------- #
# STEP 2: Load ADAE and ADSL.
# ------------------------------------------------------------------------- #

cat(">> STEP 2: Loading ADAE and ADSL datasets...\n")

adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

cat("   ADAE dimensions :", paste(dim(adae), collapse = " x "), "\n")
cat("   ADSL dimensions :", paste(dim(adsl), collapse = " x "), "\n\n")

# ------------------------------------------------------------------------- #
# STEP 3: Define the denominator population (Big-N).
#   tbl_hierarchical() uses the `denominator` dataset to compute Big-N per
#   treatment arm (one row per subject) -- this is what drives the correct
#   "Placebo, N=86 / Dose, N=72 / Dose, N=96" style header in the screenshot,
#   and ensures subjects with ZERO AEs still count toward the denominator.
#   We restrict to the safety population (SAFFL == "Y") if that flag exists.
# ------------------------------------------------------------------------- #

cat(">> STEP 3: Building denominator (Big-N) population from ADSL...\n")

adsl_safety <- if ("SAFFL" %in% names(adsl)) {
  dplyr::filter(adsl, SAFFL == "Y")
} else {
  adsl
}

cat("   Big-N per treatment arm:\n")
print(dplyr::count(adsl_safety, ACTARM, name = "BIGN"))
cat("\n")

# ------------------------------------------------------------------------- #
# STEP 4: Filter ADAE to Treatment-Emergent AEs only (TRTEMFL == "Y"),
#   as required by the task.
# ------------------------------------------------------------------------- #

cat(">> STEP 4: Filtering ADAE to Treatment-Emergent AEs (TRTEMFL == 'Y')...\n")

adae_teae <- dplyr::filter(adae, TRTEMFL == "Y")

cat("   ADAE records before filter:", nrow(adae), "\n")
cat("   ADAE records after  filter:", nrow(adae_teae), "(TEAE only)\n\n")

# ------------------------------------------------------------------------- #
# STEP 5: Build the hierarchical TEAE summary table.
#
#   tbl_hierarchical() is purpose-built for exactly this kind of AE table:
#     - variables = c(AESOC, AETERM)  -> nested rows: SOC headers with PT
#       rows indented beneath them (matches the screenshot layout)
#     - by = ACTARM                   -> one column per treatment group
#     - id = USUBJID                  -> de-duplicates so a subject with
#                                        multiple occurrences of the same
#                                        AE is only counted ONCE (incidence,
#                                        not occurrence count)
#     - denominator = adsl_safety     -> correct Big-N per arm for % calc
#     - statistic = "{n} ({p}%)"      -> cell value = count and percentage
#     - overall_row = TRUE            -> adds a top "Any TEAE" summary row,
#                                        exactly like "Treatment Emergent
#                                        AEs" in the reference screenshot
# ------------------------------------------------------------------------- #

cat(">> STEP 5: Building hierarchical AE table with tbl_hierarchical()...\n")

ae_tbl <- tbl_hierarchical(
  data        = adae_teae,
  variables   = c(AESOC, AETERM),
  by          = ACTARM,
  id          = USUBJID,
  denominator = adsl_safety,
  statistic   = everything() ~ "{n} ({p}%)",
  overall_row = TRUE,
  label       = list(
    AESOC = "Primary System Organ Class",
    AETERM = "Reported Term for the Adverse Event",
    ..ard_hierarchical_overall.. = "Treatment Emergent AEs"
  )
)

cat("   tbl_hierarchical() object built.\n\n")

# ------------------------------------------------------------------------- #
# STEP 6: (Total column intentionally NOT added per latest requirement --
#   table shows only the by-arm (ACTARM) columns, no overall/Total column.)
# ------------------------------------------------------------------------- #

# ------------------------------------------------------------------------- #
# STEP 7: Sort by descending frequency, as required by the task.
#   sort_hierarchical() sorts BOTH levels of the hierarchy (AESOC, then
#   AETERM within each AESOC) by descending overall frequency by default.
# ------------------------------------------------------------------------- #

cat(">> STEP 7: Sorting rows by descending frequency (sort_hierarchical())...\n")

ae_tbl <- ae_tbl %>%
  sort_hierarchical(sort = everything() ~ "descending")

cat("   Table sorted: most frequent SOC/AE terms appear first.\n\n")

# ------------------------------------------------------------------------- #
# STEP 8: Cosmetic / regulatory-style formatting (bold labels, caption,
#   header styling) to match the FDA Table 10 look in the screenshot.
#
#   INDENTATION: tbl_hierarchical() already indents AETERM (PT) rows one
#   level beneath their parent AESOC row by default (this is built into
#   the table_styling "indent" attribute it sets for nested variables).
#   The line below makes that explicit/guaranteed at exactly one level of
#   indent, in case a theme or prior modify_*() call has changed it.
# ------------------------------------------------------------------------- #

cat(">> STEP 8: Applying final formatting (labels, caption, bold, indent)...\n")

ae_tbl <- ae_tbl %>%
  modify_table_styling(
    columns = label,
    rows = variable == "AETERM",
    text_format = "indent"     # ensures one level of indent for PT rows
  ) %>%
  modify_caption("**Table 10. Treatment-Emergent Adverse Events by System Organ Class and Preferred Term**") %>%
  bold_labels() %>%
  modify_footnote(everything() ~ NA)   # remove default gtsummary footnotes

cat("   Formatting applied (PT rows indented one level beneath their SOC).\n\n")

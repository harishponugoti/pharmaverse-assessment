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

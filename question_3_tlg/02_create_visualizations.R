################################################################################
# Program      : question_3_tlg/02_create_visualizations.R
# Purpose      : Create the two required {ggplot2} visualizations from
#                pharmaverseadam::adae:
#                  Plot 1 - AE severity distribution by treatment arm
#                           (stacked bar chart; AESEV x ACTARM)
#                  Plot 2 - Top 10 most frequent AEs, with 95%
#                           Clopper-Pearson CIs for incidence rate
#                           (AETERM, dot + error-bar / "forest" style plot)
# Input        : pharmaverseadam::adae, pharmaverseadam::adsl
# Output       : outputs/ae_severity_distribution.png
#                outputs/top10_ae_frequency_ci.png
#                outputs/02_create_visualizations_log.txt   (execution log)
################################################################################

# ------------------------------------------------------------------------- #
# STEP 0: Logging -- capture every step of execution from line 1 to end,
#   including any warnings/messages from ggplot2, so you can confirm the
#   script ran error-free (or see exactly what fired, and where).
# ------------------------------------------------------------------------- #

if (!dir.exists("outputs")) dir.create("outputs")

log_file <- "outputs/02_create_visualizations_log.txt"
log_con  <- file(log_file, open = "wt")
sink(log_con, split = TRUE)        # echo to console AND log file
sink(log_con, type = "message")    # also capture warnings/messages

cat("================================================================\n")
cat("02_create_visualizations.R - Execution Log\n")
cat("Run started :", as.character(Sys.time()), "\n")
cat("================================================================\n\n")

# ------------------------------------------------------------------------- #
# STEP 1: Load required packages.
#   - pharmaverseadam : source of adae / adsl test data
#   - dplyr           : data manipulation
#   - ggplot2         : the two required visualizations
#   - forcats         : tidy factor re-leveling/ordering for plot axes
# ------------------------------------------------------------------------- #

cat(">> STEP 1: Loading required packages...\n")

required_pkgs <- c("pharmaverseadam", "dplyr", "ggplot2", "forcats")

new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) {
  cat("   Installing missing packages:", paste(new_pkgs, collapse = ", "), "\n")
  install.packages(new_pkgs, type = "binary")
}

invisible(lapply(required_pkgs, library, character.only = TRUE))

cat("   Packages loaded successfully.\n\n")

# ------------------------------------------------------------------------- #
# STEP 2: Load ADAE and ADSL.
# ------------------------------------------------------------------------- #

cat(">> STEP 2: Loading ADAE and ADSL datasets...\n")

adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

cat("   ADAE dimensions :", paste(dim(adae), collapse = " x "), "\n")
cat("   ADSL dimensions :", paste(dim(adsl), collapse = " x "), "\n\n")

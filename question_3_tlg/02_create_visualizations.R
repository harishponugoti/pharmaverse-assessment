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


# ------------------------------------------------------------------------- #
# STEP 3: Define the analysis population, consistent with the summary
#   table script (01_create_ae_summary_table.R):
#     - Safety population from ADSL (SAFFL == "Y" if present)
#     - Treatment-emergent AEs only (TRTEMFL == "Y")
#   Keeping this identical across both scripts means the table and the
#   plots always agree on denominators and AE counts.
# ------------------------------------------------------------------------- #

cat(">> STEP 3: Building safety population and TEAE-filtered AE dataset...\n")

adsl_safety <- if ("SAFFL" %in% names(adsl)) {
  dplyr::filter(adsl, SAFFL == "Y")
} else {
  adsl
}

adae_teae <- dplyr::filter(adae, TRTEMFL == "Y")

n_subjects <- dplyr::n_distinct(adsl_safety$USUBJID)

cat("   Safety population (Big-N), all arms combined:", n_subjects, "subjects\n")
cat("   TEAE records:", nrow(adae_teae), "\n\n")

################################################################################
# PLOT 1: AE severity distribution by treatment arm
#   - Rows/groups : ACTARM (treatment arm) on the x-axis
#   - Fill        : AESEV (MILD / MODERATE / SEVERE)
#   - Bar height  : Count of AE records (stacked)
#   Reference layout: pink/salmon = MILD (top of stack), green = MODERATE
#   (middle), blue = SEVERE (bottom, smallest segment).
################################################################################

cat(">> PLOT 1: Building 'AE severity distribution by treatment' (stacked bar)...\n")

# --- 4a. Prepare the plotting data ----------------------------------------
# One row per AE record (not de-duplicated by subject) since this plot
# counts AEs, not subjects -- it answers "how many AE *events* of each
# severity occurred in each arm", matching the reference chart's y-axis
# label "Count of AEs".
plot1_data <- adae_teae %>%
  dplyr::filter(!is.na(AESEV)) %>%
  dplyr::mutate(
    AESEV  = factor(AESEV, levels = c("SEVERE", "MODERATE", "MILD")),  # stack order: SEVERE at bottom, MILD at top
    ACTARM = forcats::fct_inorder(as.character(ACTARM))                 # keep ADaM's natural arm order
  )

cat("   Plot 1 input rows (TEAE records with non-missing AESEV):", nrow(plot1_data), "\n")
cat("   AESEV levels found:", paste(levels(plot1_data$AESEV), collapse = ", "), "\n")
cat("   ACTARM levels found:", paste(levels(plot1_data$ACTARM), collapse = ", "), "\n\n")

# --- 4b. Build the ggplot ---------------------------------------------------
plot1 <- ggplot2::ggplot(plot1_data, ggplot2::aes(x = ACTARM, fill = AESEV)) +
  ggplot2::geom_bar(position = "stack", width = 0.7) +
  ggplot2::scale_fill_manual(
    name   = "Severity/Intensity",
    values = c("MILD" = "#F8766D", "MODERATE" = "#00BA38", "SEVERE" = "#619CFF"),
    breaks = c("MILD", "MODERATE", "SEVERE")     # legend order: MILD, MODERATE, SEVERE (top to bottom)
  ) +
  ggplot2::labs(
    title = "AE severity distribution by treatment",
    x     = "Treatment Arm",
    y     = "Count of AEs"
  ) +
  ggplot2::theme_gray(base_size = 12) +
  ggplot2::theme(
    plot.title      = ggplot2::element_text(face = "bold", hjust = 0),
    panel.grid.minor = ggplot2::element_blank()
  )

cat("   ggplot object for Plot 1 built successfully.\n\n")

# --- 4c. Save Plot 1 as PNG -------------------------------------------------
cat(">> Saving Plot 1 PNG...\n")

ggplot2::ggsave(
  filename = "outputs/ae_severity_distribution.png",
  plot     = plot1,
  width    = 7.5, height = 5.5, dpi = 300, units = "in"
)

cat("   Saved:", normalizePath("outputs/ae_severity_distribution.png"), "\n\n")


################################################################################
# PLOT 2: Top 10 most frequent AEs, with 95% Clopper-Pearson CIs
#   - y-axis : AETERM (top 10 most frequent terms, most frequent at top)
#   - x-axis : Percentage of patients (incidence rate, %), with 95% CI
#   - Denominator (n) : total subjects in the safety population
#   - Each subject is counted ONCE per AETERM (incidence, not occurrences)
################################################################################

cat(">> PLOT 2: Building 'Top 10 Most Frequent Adverse Events' (95% CI plot)...\n")

# --- 5a. Count UNIQUE SUBJECTS per AETERM (incidence, not event count) ----
ae_subject_counts <- adae_teae %>%
  dplyr::distinct(USUBJID, AETERM) %>%             # one row per subject per term
  dplyr::count(AETERM, name = "n_subjects") %>%
  dplyr::arrange(dplyr::desc(n_subjects))

cat("   Total distinct AETERM values:", nrow(ae_subject_counts), "\n")

# --- 5b. Keep only the TOP 10 most frequent terms --------------------------
top10_ae <- ae_subject_counts %>%
  dplyr::slice_head(n = 10)

cat("   Top 10 AE terms selected:\n")
print(top10_ae)
cat("\n")

# --- 5c. Compute 95% Clopper-Pearson exact CI for each term's incidence ----
#   stats::binom.test() uses the Clopper-Pearson exact method by default,
#   so no extra package (e.g. Hmisc) is required.
cat(">> Computing 95% Clopper-Pearson confidence intervals...\n")

# For each of the top 10 terms: x = subjects with that AE, n = safety
# population size. stats::binom.test() defaults to the exact
# Clopper-Pearson method, so no extra package is needed.
ci_list <- lapply(seq_len(nrow(top10_ae)), function(i) {
  bt <- stats::binom.test(
    x = top10_ae$n_subjects[i],
    n = n_subjects,
    conf.level = 0.95
  )
  data.frame(
    pct    = 100 * top10_ae$n_subjects[i] / n_subjects,
    ci_low  = 100 * bt$conf.int[1],
    ci_high = 100 * bt$conf.int[2]
  )
})
ci_df <- do.call(rbind, ci_list)

top10_ae <- cbind(top10_ae, ci_df) %>%
  dplyr::select(AETERM, n_subjects, pct, ci_low, ci_high) %>%
  dplyr::arrange(dplyr::desc(n_subjects)) %>%
  dplyr::mutate(AETERM = forcats::fct_reorder(AETERM, n_subjects))  # plot order: most frequent at top

cat("   Final Top-10 table with 95% CIs:\n")
print(top10_ae)
cat("\n")

# --- 5d. Build the ggplot (dot + error-bar / forest-style plot) -----------
plot2 <- ggplot2::ggplot(top10_ae, ggplot2::aes(x = pct, y = AETERM)) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_low, xmax = ci_high), height = 0.15) +
  ggplot2::geom_point(size = 3) +
  ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%")) +
  ggplot2::labs(
    title    = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", n_subjects, " subjects; 95% Clopper-Pearson CIs"),
    x        = "Percentage of Patients (%)",
    y        = NULL
  ) +
  ggplot2::theme_gray(base_size = 12) +
  ggplot2::theme(
    plot.title    = ggplot2::element_text(face = "bold", hjust = 0),
    plot.subtitle = ggplot2::element_text(color = "grey30"),
    panel.grid.minor = ggplot2::element_blank()
  )

cat("   ggplot object for Plot 2 built successfully.\n\n")

# --- 5e. Save Plot 2 as PNG -------------------------------------------------
cat(">> Saving Plot 2 PNG...\n")

ggplot2::ggsave(
  filename = "outputs/top10_ae_frequency_ci.png",
  plot     = plot2,
  width    = 8, height = 5.5, dpi = 300, units = "in"
)

cat("   Saved:", normalizePath("outputs/top10_ae_frequency_ci.png"), "\n\n")

# ------------------------------------------------------------------------- #
# STEP 6: Final sanity checks / summary printed to the log.
# ------------------------------------------------------------------------- #

cat(">> STEP 6: Sanity checks...\n")
cat("   Plot 1 - severity levels plotted :", paste(levels(plot1_data$AESEV), collapse = ", "), "\n")
cat("   Plot 1 - treatment arms plotted  :", paste(levels(plot1_data$ACTARM), collapse = ", "), "\n")
cat("   Plot 2 - denominator (n)         :", n_subjects, "\n")
cat("   Plot 2 - AE terms plotted        :", paste(as.character(top10_ae$AETERM), collapse = "; "), "\n\n")

cat("================================================================\n")
cat("Run completed :", as.character(Sys.time()), "\n")
cat("STATUS: SUCCESS\n")
cat("Outputs written:\n")
cat("  - outputs/ae_severity_distribution.png\n")
cat("  - outputs/top10_ae_frequency_ci.png\n")
cat("  - outputs/02_create_visualizations_log.txt (this file)\n")
cat("================================================================\n")

# ------------------------------------------------------------------------- #
# STEP 7: Close log sinks (always last).
# ------------------------------------------------------------------------- #

sink(type = "message")
sink()
close(log_con)
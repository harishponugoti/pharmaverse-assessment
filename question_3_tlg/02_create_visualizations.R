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

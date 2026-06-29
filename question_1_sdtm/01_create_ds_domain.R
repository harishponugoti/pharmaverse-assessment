# Question 1 - SDTM DS domain creation
# Goal: Create DS and populate DSSTDY using DM.RFSTDTC
# Output: question_1_sdtm/output/DS.csv and ds_run_log.txt

# -------------------------------------------------------------------
# 0) Set up full execution logging (line 1 to end of script).
#    sink() redirects both normal console output (cat/print) and
#    messages/warnings to the log file, while split = TRUE also echoes
#    everything to the console so you can watch it run interactively.
#    The output folder + log file are created FIRST, before anything
#    else runs, so the whole script is captured from the very top.
# -------------------------------------------------------------------
output_dir <- "output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

log_path <- file.path(output_dir, "ds_run_log.txt")
log_con <- file(log_path, open = "wt")
sink(log_con, split = TRUE)              # capture stdout (cat/print)
sink(log_con, type = "message")          # capture messages/warnings too

cat("================================================================\n")
cat("Question 1 - SDTM DS Domain Creation - Execution Log\n")
cat("Run started :", as.character(Sys.time()), "\n")
cat("================================================================\n\n")

# -------------------------------------------------------------------
# 1) Install packages if needed
# -------------------------------------------------------------------
cat(">> STEP 1: Checking/installing required packages...\n")

required_packages <- c(
  "dplyr",
  "readr",
  "pharmaverseraw",
  "pharmaversesdtm",
  "lubridate"
)

new_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(new_packages) > 0) {
  cat("   Installing missing packages:", paste(new_packages, collapse = ", "), "\n")
  install.packages(new_packages)
}

# -------------------------------------------------------------------
# 2) Load libraries
# -------------------------------------------------------------------
cat(">> STEP 2: Loading libraries...\n")

library(dplyr)
library(readr)
library(pharmaverseraw)
library(pharmaversesdtm)
library(lubridate)

cat("   Libraries loaded successfully.\n\n")

# -------------------------------------------------------------------
# 3) Load source data
# -------------------------------------------------------------------
cat(">> STEP 3: Loading source data (ds_raw, dm)...\n")

ds_raw <- pharmaverseraw::ds_raw
dm <- pharmaversesdtm::dm

cat("   ds_raw rows:", nrow(ds_raw), "| cols:", ncol(ds_raw), "\n")
cat("   dm rows     :", nrow(dm), "| cols:", ncol(dm), "\n\n")

# -------------------------------------------------------------------
# 4) Controlled terminology lookup for DSDECOD
# -------------------------------------------------------------------
cat(">> STEP 4: Building controlled terminology (DSDECOD) lookup table...\n")

studyct <- data.frame(
  stringsAsFactors = FALSE,
  codelistcode = rep("C66727", 10),
  termcode = c(
    "C41331","C25250","C28554","C48226","C48227",
    "C48250","C142185","C49628","C49632","C49634"
  ),
  termvalue = c(
    "ADVERSE EVENT",
    "COMPLETED",
    "DEATH",
    "LACK OF EFFICACY",
    "LOST TO FOLLOW-UP",
    "PHYSICIAN DECISION",
    "PROTOCOL VIOLATION",
    "SCREEN FAILURE",
    "STUDY TERMINATED BY SPONSOR",
    "WITHDRAWAL BY SUBJECT"
  ),
  collectedvalue = c(
    "Adverse Event",
    "Complete",
    "Dead",
    "Lack of Efficacy",
    "Lost To Follow-Up",
    "Physician Decision",
    "Protocol Violation",
    "Trial Screen Failure",
    "Study Terminated By Sponsor",
    "Withdrawal by Subject"
  ),
  termpreferredterm = c(
    "AE","Completed","Died",NA,NA,NA,"Violation",
    "Failure to Meet Inclusion/Exclusion Criteria",NA,"Dropout"
  ),
  termsynonyms = c(
    "ADVERSE EVENT","COMPLETE","Death",NA,NA,NA,NA,NA,NA,"Discontinued Participation"
  )
)

cat("   studyct rows:", nrow(studyct), "\n\n")

# -------------------------------------------------------------------
# 5) Build DS records with DM-style USUBJID
# -------------------------------------------------------------------
cat(">> STEP 5: Building DS records (USUBJID, DSCAT, DSTERM, DSDECOD, DSSEQ)...\n")

ds1 <- ds_raw %>%
  mutate(
    STUDYID = as.character(STUDY),
    PATNUM = as.character(PATNUM),
    # DM USUBJID style for the pilot data: STUDYID-SUBJID
    # Here PATNUM is the subject id in raw data, so we mirror the DM pattern.
    STUDY_PREFIX = substr(STUDYID, nchar(STUDYID) - 1, nchar(STUDYID)),
    USUBJID = paste0(STUDY_PREFIX, "-", PATNUM),
    DOMAIN = "DS",
    DSCAT = "DISPOSITION EVENT",
    DSTERM = as.character(IT.DSTERM),
    DSDECOD = as.character(IT.DSDECOD),
    DSDTC = as.character(DSDTCOL),
    DSSTDTC = as.character(IT.DSSTDAT)
  ) %>%
  group_by(USUBJID) %>%
  mutate(DSSEQ = row_number()) %>%
  ungroup() %>%
  left_join(
    studyct %>% transmute(collectedvalue, ct_dsdecod = termvalue),
    by = c("DSTERM" = "collectedvalue")
  ) %>%
  mutate(DSDECOD = coalesce(DSDECOD, ct_dsdecod)) %>%
  select(-ct_dsdecod)

cat("   ds1 rows:", nrow(ds1), "| unique USUBJID:", n_distinct(ds1$USUBJID), "\n")
cat("   DSDECOD table after CT lookup:\n")
print(table(ds1$DSDECOD, useNA = "ifany"))
cat("\n")

# -------------------------------------------------------------------
# 6) Prepare DM key and join RFSTDTC
# -------------------------------------------------------------------
cat(">> STEP 6: Joining DM.RFSTDTC by USUBJID and deriving DSSTDY...\n")

# Prepare DM reference and join by USUBJID
dm_ref <- dm %>%
  transmute(
    USUBJID = as.character(USUBJID),
    RFSTDTC = as.character(RFSTDTC)
  ) %>%
  distinct()

cat("   dm_ref rows (distinct USUBJID):", nrow(dm_ref), "\n")

# Join and derive DSSTDY using numeric days
ds2 <- ds1 %>%
  left_join(dm_ref, by = "USUBJID") %>%
  mutate(
    # Parse DSSTDTC in MM-DD-YYYY format
    DSSTDTC_DATE = suppressWarnings(as.Date(DSSTDTC, format = "%m-%d-%Y")),
    # Parse RFSTDTC in ISO YYYY-MM-DD format
    RFSTDTC_DATE = suppressWarnings(as.Date(RFSTDTC, format = "%Y-%m-%d")),
    # Convert to numeric day counts (days since 1970-01-01)
    DSSTDTC_NUM = as.numeric(DSSTDTC_DATE),
    RFSTDTC_NUM = as.numeric(RFSTDTC_DATE),
    # Derive study day using numeric difference
    DSSTDY = case_when(
      !is.na(DSSTDTC_NUM) & !is.na(RFSTDTC_NUM) & DSSTDTC_NUM >= RFSTDTC_NUM ~
        as.integer(DSSTDTC_NUM - RFSTDTC_NUM + 1L),
      !is.na(DSSTDTC_NUM) & !is.na(RFSTDTC_NUM) & DSSTDTC_NUM < RFSTDTC_NUM ~
        as.integer(DSSTDTC_NUM - RFSTDTC_NUM),
      TRUE ~ NA_integer_
    )
  )

cat("   ds2 rows:", nrow(ds2), "\n")
cat("   Rows with DSSTDY derived:", sum(!is.na(ds2$DSSTDY)), "out of", nrow(ds2), "\n\n")

# -------------------------------------------------------------------
# 7) Final DS dataset
# -------------------------------------------------------------------
cat(">> STEP 7: Selecting final DS variables...\n")

DS <- ds2 %>%
  select(
    STUDYID,
    DOMAIN,
    USUBJID,
    DSSEQ,
    DSTERM,
    DSDECOD,
    DSCAT,
    any_of(c("VISITNUM", "VISIT")),
    DSDTC,
    DSSTDTC,
    DSSTDY
  )

cat("   Final DS rows:", nrow(DS), "| columns:", ncol(DS), "\n")
cat("   Final DS column names:", paste(names(DS), collapse = ", "), "\n\n")

# -------------------------------------------------------------------
# 8) Diagnostics / log notes
#    NOTE: the original script referenced an object called `ds` here,
#    which was never created (the working dataset at this point is
#    `ds2`). That would have thrown an "object 'ds' not found" error
#    before the log file was ever written, so it's corrected to `ds2`
#    below -- the diagnostic logic itself is unchanged.
# -------------------------------------------------------------------
cat(">> STEP 8: Building diagnostics / QC log notes...\n")

log_lines <- c(
  "Question 1 DS domain creation completed successfully.",
  paste("Rows:", nrow(DS)),
  paste("Missing RFSTDTC after join:", sum(is.na(ds2$RFSTDTC))),
  paste("Missing DSSTDTC:", sum(is.na(ds2$DSSTDTC))),
  paste("Missing DSSTDY:", sum(is.na(DS$DSSTDY))),
  "",
  "Derivation notes:",
  "1. USUBJID is built to match the DM merge key.",
  "2. RFSTDTC is joined from DM using USUBJID.",
  "3. DSSTDTC is derived from IT.DSSTDAT.",
  "4. DSSTDY is derived from DSSTDTC relative to RFSTDTC.",
  "5. If both dates exist and DSSTDTC is on/after RFSTDTC, DSSTDY = DSSTDTC - RFSTDTC + 1.",
  "6. If DSSTDTC is before RFSTDTC, DSSTDY = DSSTDTC - RFSTDTC."
)

cat(paste(log_lines, collapse = "\n"), "\n\n")

# -------------------------------------------------------------------
# 9) Write output files
# -------------------------------------------------------------------
cat(">> STEP 9: Writing DS.csv to output/...\n")

write_csv(DS, file.path(output_dir, "DS.csv"))

cat("   Saved:", normalizePath(file.path(output_dir, "DS.csv")), "\n\n")

cat("================================================================\n")
cat("Run completed :", as.character(Sys.time()), "\n")
cat("STATUS: SUCCESS\n")
cat("================================================================\n")

# -------------------------------------------------------------------
# 10) Close log sinks (always last, so the log file is fully flushed
#     and closed, and nothing after this point is lost).
# -------------------------------------------------------------------
sink(type = "message")
sink()
close(log_con)

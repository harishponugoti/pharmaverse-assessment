
# Question 1 - SDTM DS domain creation
# Goal: Create DS and populate DSSTDY using DM.RFSTDTC
# Output: question_1_sdtm/output/DS.csv and ds_run_log.txt

# -------------------------------------------------------------------
# 1) Install packages if needed
# -------------------------------------------------------------------
required_packages <- c(
  "dplyr",
  "readr",
  "pharmaverseraw",
  "pharmaversesdtm",
  "lubridate"
)

new_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(new_packages) > 0) {
  install.packages(new_packages)
}

# -------------------------------------------------------------------
# 2) Load libraries
# -------------------------------------------------------------------
library(dplyr)
library(readr)
library(pharmaverseraw)
library(pharmaversesdtm)
library(lubridate)

# -------------------------------------------------------------------
# 3) Load source data
# -------------------------------------------------------------------
ds_raw <- pharmaverseraw::ds_raw
dm <- pharmaversesdtm::dm

# -------------------------------------------------------------------
# 4) Controlled terminology lookup for DSDECOD
# -------------------------------------------------------------------
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

# -------------------------------------------------------------------
# 5) Build DS records with DM-style USUBJID
# -------------------------------------------------------------------
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

# -------------------------------------------------------------------
# 6) Prepare DM key and join RFSTDTC
# -------------------------------------------------------------------
# Prepare DM reference and join by USUBJID
dm_ref <- dm %>%
  transmute(
    USUBJID = as.character(USUBJID),
    RFSTDTC = as.character(RFSTDTC)
  ) %>%
  distinct()

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
# -------------------------------------------------------------------
# 7) Final DS dataset
# -------------------------------------------------------------------
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

# -------------------------------------------------------------------
# 8) Diagnostics / log notes
# -------------------------------------------------------------------
log_lines <- c(
  "Question 1 DS domain creation completed successfully.",
  paste("Rows:", nrow(DS)),
  paste("Missing RFSTDTC after join:", sum(is.na(ds$RFSTDTC))),
  paste("Missing DSSTDTC:", sum(is.na(ds$DSSTDTC))),
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

# -------------------------------------------------------------------
# 9) Write output files
# -------------------------------------------------------------------
output_dir <- "output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

write_csv(DS, file.path(output_dir, "DS.csv"))
writeLines(log_lines, file.path(output_dir, "ds_run_log.txt"))
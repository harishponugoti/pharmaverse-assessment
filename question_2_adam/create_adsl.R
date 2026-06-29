# -------------------------------------------------------------------
# Question 2 - ADSL creation
# Purpose: Create a reviewer-ready ADSL dataset with traceable object names
# -------------------------------------------------------------------

# Packages -----------------------------------------------------------
requiredPackages <- c("admiral", "dplyr", "stringr", "readr", "lubridate", "pharmaversesdtm")
newPackages <- requiredPackages[!requiredPackages %in% installed.packages()[, "Package"]]
if (length(newPackages) > 0) install.packages(newPackages)

library(admiral)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(pharmaversesdtm)

# Read in data -------------------------------------------------------
adsl_q2_raw_dm <- pharmaversesdtm::dm %>% mutate(across(where(is.character), ~ na_if(.x, "")))
adsl_q2_raw_ex <- pharmaversesdtm::ex %>% mutate(across(where(is.character), ~ na_if(.x, "")))
adsl_q2_raw_ds <- pharmaversesdtm::ds %>% mutate(across(where(is.character), ~ na_if(.x, "")))
adsl_q2_raw_ae <- pharmaversesdtm::ae %>% mutate(across(where(is.character), ~ na_if(.x, "")))
adsl_q2_raw_vs <- pharmaversesdtm::vs %>% mutate(across(where(is.character), ~ na_if(.x, "")))

# Base ADSL ----------------------------------------------------------
adsl_q2_base <- adsl_q2_raw_dm %>% select(-DOMAIN)

# Derive age, region, and population flags --------------------------
adsl_q2_grp <- adsl_q2_base %>%
  mutate(
    AGEGR1 = case_when(
      is.na(AGE) ~ NA_character_,
      AGE < 18 ~ "<18",
      between(AGE, 18, 64) ~ "18-64",
      AGE > 64 ~ ">64"
    ),
    AGEGR9 = case_when(
      is.na(AGE) ~ NA_character_,
      AGE < 18 ~ "<18",
      between(AGE, 18, 50) ~ "18 - 50",
      AGE > 50 ~ ">50"
    ),
    AGEGR9N = case_when(
      is.na(AGE) ~ NA_integer_,
      AGE < 18 ~ 1L,
      between(AGE, 18, 50) ~ 2L,
      AGE > 50 ~ 3L
    ),
    REGION1 = case_when(
      COUNTRY %in% c("USA", "CAN") ~ "NA",
      !is.na(COUNTRY) ~ "RoW",
      TRUE ~ NA_character_
    )
  )

# Prepare exposure data ---------------------------------------------
adsl_q2_ex_ext <- adsl_q2_raw_ex %>%
  mutate(validDose = EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    date_imputation = "first",
    time_imputation = "00:00:00",
    ignore_seconds_flag = TRUE
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    date_imputation = "last",
    time_imputation = "23:59:59"
  )

# Derive treatment variables ----------------------------------------
adsl_q2_trt <- adsl_q2_grp %>%
  derive_vars_merged(
    dataset_add = adsl_q2_ex_ext,
    by_vars = exprs(STUDYID, USUBJID),
    filter_add = validDose & !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF, TRT01A = EXTRT),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first"
  ) %>%
  derive_vars_merged(
    dataset_add = adsl_q2_ex_ext,
    by_vars = exprs(STUDYID, USUBJID),
    filter_add = validDose & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last"
  ) %>%
  mutate(
    TRT01P = ARM,
    ITTFL = if_else(!is.na(ARM) & ARM != "", "Y", "N"),
    SAFFL = if_else(!is.na(TRTSDTM), "Y", "N")
  ) %>%
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM))

# Derive duration ----------------------------------------------------
adsl_q2_trt <- adsl_q2_trt %>% derive_var_trtdurd()

# Disposition, randomization, and periods ----------------------------
adsl_q2_ds_last <- adsl_q2_raw_ds %>%
  transmute(
    STUDYID = as.character(STUDYID),
    USUBJID = as.character(USUBJID),
    DSDT = suppressWarnings(as.Date(substr(DSSTDTC, 1, 10)))
  ) %>%
  filter(!is.na(DSDT)) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(EOSDT = max(DSDT, na.rm = TRUE), .groups = "drop")

adsl_q2_final <- adsl_q2_trt %>%
  left_join(adsl_q2_ds_last, by = c("STUDYID", "USUBJID")) %>%
  mutate(
    RANDDT = TRTSDT,
    AP01SDT = as.Date(RFSTDTC),
    AP01EDT = as.Date(RFENDTC)
  )

# Last known alive date ---------------------------------------------
adsl_q2_vs_last <- adsl_q2_raw_vs %>%
  transmute(
    STUDYID = as.character(STUDYID),
    USUBJID = as.character(USUBJID),
    VSDT = suppressWarnings(as.Date(substr(VSDTC, 1, 10))),
    validVS = !is.na(VSDT) & (!is.na(VSSTRESN) | !is.na(VSSTRESC))
  ) %>%
  filter(validVS) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(VSLASTDT = max(VSDT, na.rm = TRUE), .groups = "drop")

adsl_q2_ae_last <- adsl_q2_raw_ae %>%
  transmute(
    STUDYID = as.character(STUDYID),
    USUBJID = as.character(USUBJID),
    AEDT = suppressWarnings(as.Date(substr(AESTDTC, 1, 10)))
  ) %>%
  filter(!is.na(AEDT)) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(AELASTDT = max(AEDT, na.rm = TRUE), .groups = "drop")

adsl_q2_ex_last <- adsl_q2_ex_ext %>%
  transmute(
    STUDYID = as.character(STUDYID),
    USUBJID = as.character(USUBJID),
    EXDT = suppressWarnings(as.Date(substr(EXENDTC, 1, 10))),
    validEX = validDose & !is.na(EXENDTM)
  ) %>%
  filter(validEX) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(EXLASTDT = max(EXDT, na.rm = TRUE), .groups = "drop")

adsl_q2_alive <- adsl_q2_final %>%
  left_join(adsl_q2_vs_last, by = c("STUDYID", "USUBJID")) %>%
  left_join(adsl_q2_ae_last, by = c("STUDYID", "USUBJID")) %>%
  left_join(adsl_q2_ex_last, by = c("STUDYID", "USUBJID")) %>%
  mutate(
    LSTAVLDT = pmax(VSLASTDT, AELASTDT, EOSDT, EXLASTDT, na.rm = TRUE),
    LSTAVLDT = if_else(is.infinite(LSTAVLDT), as.Date(NA), LSTAVLDT)
  )

# Final ADSL output --------------------------------------------------
adsl_q2_requested_cols <- c(
  "STUDYID", "USUBJID", "SUBJID", "SITEID", "COUNTRY",
  "AGE", "AGEU", "BRTHDT", "AAGE", "AAGEU",
  "SEX", "RACE", "ETHNIC",
  "ARM", "ACTARM", "TRT01P", "TRT01A",
  "AGEGR1", "AGEGR9", "AGEGR9N", "REGION1",
  "RANDDT", "AP01SDT", "AP01EDT",
  "TRTSDTM", "TRTSTMF", "TRTSDT", "TRTEDTM", "TRTETMF", "TRTEDT", "TRTDURD",
  "ITTFL", "SAFFL", "EOSDT", "LSTAVLDT"
)

adsl_q2_out <- adsl_q2_alive %>%
  select(any_of(adsl_q2_requested_cols), everything())

# Save outputs -------------------------------------------------------
adsl_q2_output_dir <- file.path(getwd(), "output")
if (!dir.exists(adsl_q2_output_dir)) dir.create(adsl_q2_output_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(adsl_q2_out, file.path(adsl_q2_output_dir, "adsl.csv"))

adsl_q2_log_lines <- c(
  "Question 2 ADSL dataset creation completed successfully.",
  paste("Rows:", nrow(adsl_q2_out)),
  paste("Columns:", ncol(adsl_q2_out)),
  paste("Missing AGEGR1:", sum(is.na(adsl_q2_out$AGEGR1))),
  paste("Missing AGEGR9:", sum(is.na(adsl_q2_out$AGEGR9))),
  paste("Missing RANDDT:", sum(is.na(adsl_q2_out$RANDDT))),
  paste("Missing TRTSDTM:", sum(is.na(adsl_q2_out$TRTSDTM))),
  paste("Missing ITTFL:", sum(is.na(adsl_q2_out$ITTFL))),
  paste("Missing LSTAVLDT:", sum(is.na(adsl_q2_out$LSTAVLDT)))
)

writeLines(adsl_q2_log_lines, file.path(adsl_q2_output_dir, "adsl_run_log.txt"))
# Question 2 - ADSL creation
# Purpose: Create subject-level ADaM dataset using pharmaverse SDTM inputs
# Required derivations:
# - AGEGR9, AGEGR9N
# - TRTSDTM, TRTSTMF
# - ITTFL
# - LSTAVLDT
# Notes:
# - Uses DM as the base dataset
# - Uses admiral derivation functions where possible
# - Adds clear comments for assessment review

# -------------------------------------------------------------------
# 1) Install packages if needed
# -------------------------------------------------------------------
required_packages <- c(
  "admiral",
  "dplyr",
  "stringr",
  "lubridate",
  "pharmaversesdtm"
)

new_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(new_packages) > 0) {
  install.packages(new_packages)
}

# -------------------------------------------------------------------
# 2) Load libraries
# -------------------------------------------------------------------
library(admiral)
library(dplyr)
library(stringr)
library(lubridate)
library(pharmaversesdtm)

# -------------------------------------------------------------------
# 3) Load SDTM source datasets
# -------------------------------------------------------------------
dmRaw <- pharmaversesdtm::dm
exRaw <- pharmaversesdtm::ex
dsRaw <- pharmaversesdtm::ds
aeRaw <- pharmaversesdtm::ae
vsRaw <- pharmaversesdtm::vs

# Replace blank strings with NA in character columns
dmClean <- dmRaw %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))

exClean <- exRaw %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))

dsClean <- dsRaw %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))

aeClean <- aeRaw %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))

vsClean <- vsRaw %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))

# -------------------------------------------------------------------
# 4) Base ADSL from DM
# -------------------------------------------------------------------
adslBase <- dmClean %>%
  select(-DOMAIN)

# -------------------------------------------------------------------
# 5) Derive age groups
# -------------------------------------------------------------------
adslAge <- adslBase %>%
  mutate(
    AGEGR9 = case_when(
      is.na(AGE) ~ NA_character_,
      AGE < 18 ~ "<18",
      AGE >= 18 & AGE < 50 ~ "18-50",
      AGE >= 50 ~ "50+"
    ),
    AGEGR9N = case_when(
      is.na(AGE) ~ NA_integer_,
      AGE < 18 ~ 1L,
      AGE >= 18 & AGE < 50 ~ 2L,
      AGE >= 50 ~ 3L
    )
  )

# -------------------------------------------------------------------
# 6) Prepare exposure datetimes
# -------------------------------------------------------------------
exDatetime <- exClean %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST"
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )

# -------------------------------------------------------------------
# 7) Derive treatment start and end datetimes
# -------------------------------------------------------------------
adslTrtStart <- adslAge %>%
  derive_vars_merged(
    dataset_add = exDatetime,
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(EXSTDTM, EXSEQ),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    filter_add = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    mode = "first"
  )

adslTrtEnd <- adslTrtStart %>%
  derive_vars_merged(
    dataset_add = exDatetime,
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(EXENDTM, EXSEQ),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    filter_add = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    mode = "last"
  )

# -------------------------------------------------------------------
# 8) Convert datetime to date and derive treatment duration
# -------------------------------------------------------------------
adslTrtDate <- adslTrtEnd %>%
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM)) %>%
  derive_var_trtdurd()

# -------------------------------------------------------------------
# 9) Derive ITT flag
# -------------------------------------------------------------------
adslItt <- adslTrtDate %>%
  mutate(
    ITTFL = if_else(!is.na(ARM), "Y", "N")
  )

# -------------------------------------------------------------------
# 10) Derive last known alive date components
# -------------------------------------------------------------------
vsAlive <- vsRaw %>%
  transmute(
    STUDYID = as.character(STUDYID),
    USUBJID = as.character(USUBJID),
    VSDT = suppressWarnings(as.Date(substr(VSDTC, 1, 10))),
    validVS = !is.na(VSDT) & (!is.na(VSSTRESN) | !is.na(VSSTRESC))
  ) %>%
  filter(validVS) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(VSLASTDT = max(VSDT, na.rm = TRUE), .groups = "drop")

aeAlive <- aeClean %>%
  transmute(
    STUDYID = as.character(STUDYID),
    USUBJID = as.character(USUBJID),
    AEDT = suppressWarnings(as.Date(substr(AESTDTC, 1, 10)))
  ) %>%
  filter(!is.na(AEDT)) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(AELASTDT = max(AEDT, na.rm = TRUE), .groups = "drop")

dsAlive <- dsClean %>%
  transmute(
    STUDYID = as.character(STUDYID),
    USUBJID = as.character(USUBJID),
    DSDT = suppressWarnings(as.Date(substr(DSSTDTC, 1, 10)))
  ) %>%
  filter(!is.na(DSDT)) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(DSLASTDT = max(DSDT, na.rm = TRUE), .groups = "drop")

exAlive <- exClean %>%
  transmute(
    STUDYID = as.character(STUDYID),
    USUBJID = as.character(USUBJID),
    EXDT = suppressWarnings(as.Date(substr(EXENDTC, 1, 10))),
    validEX = !is.na(EXDT) & (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO")))
  ) %>%
  filter(validEX) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(EXLASTDT = max(EXDT, na.rm = TRUE), .groups = "drop")

# -------------------------------------------------------------------
# 11) Join all last-alive date sources
# -------------------------------------------------------------------
adslAlive1 <- adslItt %>%
  left_join(vsAlive, by = c("STUDYID", "USUBJID"))

adslAlive2 <- adslAlive1 %>%
  left_join(aeAlive, by = c("STUDYID", "USUBJID"))

adslAlive3 <- adslAlive2 %>%
  left_join(dsAlive, by = c("STUDYID", "USUBJID"))

adslAlive4 <- adslAlive3 %>%
  left_join(exAlive, by = c("STUDYID", "USUBJID"))

# -------------------------------------------------------------------
# 12) Derive LSTAVLDT
# -------------------------------------------------------------------
adslLstAlv <- adslAlive4 %>%
  mutate(
    LSTAVLDT = pmax(VSLASTDT, AELASTDT, DSLASTDT, EXLASTDT, na.rm = TRUE),
    LSTAVLDT = if_else(is.infinite(LSTAVLDT), as.Date(NA), LSTAVLDT)
  )

# -------------------------------------------------------------------
# 13) Derive treatment assignment variables
# -------------------------------------------------------------------
adslTrtAssign <- adslLstAlv %>%
  mutate(
    TRT01P = ARM,
    TRT01A = ACTARM,
    TRT01PN = NA_integer_,
    TRT01AN = NA_integer_
  )

# -------------------------------------------------------------------
# 14) Final ADSL output
# -------------------------------------------------------------------
adslOut <- adslTrtAssign %>%
  select(
    STUDYID, USUBJID, SUBJID,
    SEX, RACE, COUNTRY, AGE,
    AGEGR9, AGEGR9N,
    ARM, ACTARM, TRT01P, TRT01A, TRT01PN, TRT01AN,
    TRTSDTM, TRTSTMF, TRTSDT, TRTEDTM, TRTEDT, TRTDURD,
    ITTFL, LSTAVLDT
  )

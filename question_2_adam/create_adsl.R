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
dm <- pharmaversesdtm::dm
ex <- pharmaversesdtm::ex
ds <- pharmaversesdtm::ds
ae <- pharmaversesdtm::ae
vs <- pharmaversesdtm::vs

# Replace blank character values with NA if needed
convert_blanks_to_na <- function(df) {
  df %>% mutate(across(where(is.character), ~ na_if(.x, "")))
}

dm <- convert_blanks_to_na(dm)
ex <- convert_blanks_to_na(ex)
ds <- convert_blanks_to_na(ds)
ae <- convert_blanks_to_na(ae)
vs <- convert_blanks_to_na(vs)

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
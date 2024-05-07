#-------------------Header------------------------------------------------
# Project: IHME ASHER Decomposition
# Purpose: Prepare variables from woman's file from MICS surveys
# Date: 1/9/2024
# Notes:
#***************************************************************************

# SET-UP -----------------------------------------------------------

# clear environment
rm(list=ls())
username <- Sys.info()[["user"]]

if (Sys.info()["sysname"] == "Linux") {
  j <- "FILEPATH"
  h <- "FILEPATH"
  r <- "FILEPATH"
  l <- "FILEPATH"
} else {
  j <- "FILEPATH"
  h <- "FILEPATH"
  r <- "FILEPATH"
  l <- "FILEPATH"
}

# load packages
pacman::p_load(magrittr,tidyverse,parallel,plyr,dplyr,haven,survey,tools,devtools)

# in/out
out.dir <- "FILEPATH"

# create function for the opposite of %in%
'%ni%' <- Negate('%in%')

# create function to convert CMC date to year and month
get_cmc_year <- function(x) 1900 + as.integer((as.integer(x) - 1) / 12)
get_cmc_month <- function(x) as.integer(x) - 12 * (get_cmc_year(x) - 1900)


# EXTRACT DATA FUNCTION -----------------------------------------------

extract_data <- function(survey, cur_country){
  
  # read in data 
  dt <- data.table(read_dta(survey))
  
  
  ## SURVEY CHARACTERISTICS ----------------------------------------
  message("||---Survey characteristics")
  
  # convert variable names to lowercase
  names(dt) <- tolower(names(dt))
  
  # convert character columns to lowercase
  dt[] <- lapply(dt, function(x) if(is.character(x)) tolower(x) else x)
  
  # extract survey name and add to dt
  survey_name <- str_extract(survey, "[A-Z]{3}_MICS[0-9]_[0-9]{4}(_[0-9]{4})*")
  dt[, survey := survey_name]
  
  # identifiers
  dt[, cluster := hh1] 
  dt[, hh_id := hh2]
  dt[, id := ln]
  dt[, area_unit := hh6]
  dt[, country := cur_country]
  
  # interview date
  dt[, cmc_interview_date := ifelse(grepl("MWI_MICS3", survey), wm6c, wdoi)]
  dt[, year := get_cmc_year(cmc_interview_date)]
  dt[, month := get_cmc_month(cmc_interview_date)]
  
  # unique hh id for each household
  dt[, hhid_unique := paste(hh_id, cluster)]
  
  # unique individual id for each woman
  dt[, id_unique := paste(hhid_unique, id)]
  
  # DOB of woman (CMC)
  dt[, cmc_woman_dob := ifelse(grepl("MWI_MICS3", survey), wm8c, wdob)]
  
  
  ## BIRTH HISTORIES ----------------------------------------
  message("||---Birth histories")
  
  # extract date of birth of child
  dt[, child_birth_cmc := bh4c]
  dt[, child_birth_year := get_cmc_year(child_birth_cmc)]
  dt[, child_birth_month := get_cmc_month(child_birth_cmc)]
  
  # calculate time since birth from interview date
  dt[, child_age_months := cmc_interview_date - child_birth_cmc]
  
  # birth order (1 = first birth, 2 = second birth, so on)
  dt[, birth_ord := ifelse(grepl("MWI_MICS3", survey), bh1, brthord)]
  
  # age at first birth: find child with birth order of 1 (brthord) and subtract mother's DOB from child's DOB
  # this variable is also extracted from the WM file, adding _bh suffix to differentiate
  dt[, age_1st_birth_bh := ifelse(birth_ord == 1, (child_birth_cmc - cmc_woman_dob) / 12, 0)]
  
  # if birth was in last 3 years
  dt[, birth_3_yr := ifelse(child_age_months <= 36, 1, 0)]
  
  # if birth was in last 5 years
  dt[, birth_5_yr := ifelse(child_age_months <= 60, 1, 0)]
  
  # sum birth_3_yr by woman, birth_5_yr by woman 
  dt[, births_3_yr := sum(birth_3_yr), by = "id_unique"]
  dt[, births_5_yr := sum(birth_5_yr), by = "id_unique"]
  
  
  # EXPORT CLEANED DATA --------------------------------------------
  message("||---Export cleaned data")
  
  # subset to one row per woman, keeping first birth row
  dt <- dt[birth_ord == 1]
  
  # some entries are incorrectly marked as twins, or women have 2 birth_ord == 1 rows
  # keep youngest age at 1st birth as true 1st birth
  dt[, min_age_1st_birth := min(age_1st_birth_bh), by = "id_unique"]
  dt <- dt[age_1st_birth_bh == min_age_1st_birth]
  
  # filter to variables of interest
  vars_interest <- c("survey","cluster","hh_id","id","area_unit","country","cmc_interview_date",
                     "year","month","hhid_unique","id_unique","cmc_woman_dob",
                     "child_birth_cmc","child_birth_year","child_birth_month","child_age_months",
                     "birth_order","age_1st_birth_bh","births_3_yr","births_5_yr")
  cur_vars <- names(dt)[names(dt) %in% vars_interest]
  dt_filter <- dt[, ..cur_vars]
  
  # save the cleaned data
  file_name <- gsub("\\.DTA", "", basename(survey))
  output_file_path <- file.path(out.dir, paste0(file_name, "_bh_extract.csv"))
  write.csv(dt_filter, output_file_path, row.names = FALSE)
  
  message(paste0("Successfully saved ", file_name))
}


# RUN EXTRACTIONS ---------------------------------------------------

# Cameroon
extract_data("/FILEPATH/CMR_MICS5_2014_BH.DTA", "cm")

# Nepal
extract_data("/FILEPATH/NPL_MICS5_2014_BH.DTA", "np")
extract_data("/FILEPATH/NPL_MICS6_2019_BH.DTA", "np")

# Malawi
extract_data("/FILEPATH/MWI_MICS3_2006_BH.DTA", "mw")
extract_data("/FILEPATH/MWI_MICS5_2013_2014_BH.DTA", "mw")
extract_data("/FILEPATH/MWI_MICS6_2019_2020_BH.DTA", "mw")

# Ghana
extract_data("/FILEPATH/GHA_MICS4_2011_BH.DTA", "gh")
extract_data("/FILEPATH/GHA_MICS6_2017_2018_BH.DTA", "gh")


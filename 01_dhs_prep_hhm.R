#-------------------Header------------------------------------------------
# Project: IHME ASHER Decomposition
# Purpose: Extract school attendance from household member file from DHS surveys 
# Date: 1/19/2024
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


# EXTRACT DATA FUNCTION -----------------------------------------------

extract_data <- function(survey, cur_country) {
  
  # read in data 
  dt <- data.table(read_dta(survey))

  
  # SURVEY CHARACTERISTICS --------------------------------------
  message("||---Survey characteristics")
  
  # convert variable names to lowercase
  names(dt) <- tolower(names(dt))
  
  # extract survey name and add to dt
  survey_name <- str_extract(survey, "[A-Z]{3}_DHS[0-9]_[0-9]{4}(_[0-9]{4})*")
  dt[, survey := survey_name]
  
  # identifiers
  dt[, cluster := hv001]
  dt[, hh_id := hv002]
  dt[, id := hvidx] 
  dt[, area_unit := hv004]
  dt[, country := cur_country]

  # unique hh id for each household
  dt[, hhid_unique := paste(hh_id, cluster)]
  
  # unique individual id for each household member
  dt[, id_unique := paste(hhid_unique, id)]

  
  # EDUCATION VARIABLES ----------------------------------------
  message("||---Education variables")
  
  # ever attended school
  if ("sh16" %in% names(dt)) dt[, ever_attend_school := ifelse(sh16 == 1, 1, 0)]
  
  # attended school during current school year 
  dt[, attend_school := ifelse(hv121 %in% c(1,2), 1,
                               ifelse(hv121 == 0, 0, NA))]
  
  # level if attended school during current school year
  dt[, attend_school_level := ifelse(attend_school == 1, hv124, NA)]
  
  # education level of household head
  dt[, education_hh_head := ifelse(id == hv218, as.character(as_factor(hv106)), NA)]
  dt[, education_hh_head := education_hh_head[!is.na(education_hh_head)][1L], by = "hhid_unique"]  # fill in rest of hh mems
  
  # years of schooling of household head
  dt[, mean_yrs_schooling_head := ifelse(id == hv218 & hv108 < 90, hv108, NA)]
  dt[, mean_yrs_schooling_head := mean_yrs_schooling_head[!is.na(mean_yrs_schooling_head)][1L], by = "hhid_unique"]  # fill in rest of hh mems
  
  # highest education level attended
  dt[, edu_level_categ := as.character(as_factor(hv106))]
  
  # education level in single years
  dt[, educ_single_yrs := ifelse(hv108 < 90, hv108, NA)]   # marks dk responses as NA
  
  
  # EXPORT CLEANED DATA --------------------------------------------
  message("||---Export cleaned data")
  
  # filter to variables of interest
  vars_interest <- c("survey","cluster","hh_id","id","area_unit","country","hhid_unique","id_unique", "ever_attend_school",
                     "attend_school","attend_school_level","education_hh_head","mean_yrs_schooling_head",
                     "edu_level_categ","educ_single_yrs")
  cur_vars <- names(dt)[names(dt) %in% vars_interest]
  dt_filter <- dt[, ..cur_vars]
  
  # save the cleaned data
  file_name <- gsub("\\.DTA", "", basename(survey))
  output_file_path <- file.path(out.dir, paste0(file_name, "_hhm_extract.csv"))
  write.csv(dt_filter, output_file_path, row.names = FALSE)
  
  message(paste0("Successfully saved ", file_name))
}


# RUN EXTRACTIONS ---------------------------------------------------

# Cameroon
extract_data("/FILEPATH/CMR_DHS4_2004_HHM.DTA", "cm")
extract_data("/FILEPATH/CMR_DHS5_2011_HHM.DTA", "cm")
extract_data("/FILEPATH/CMR_DHS7_2018_2019_HHM.DTA", "cm")

# Ghana
extract_data("/FILEPATH/GHA_DHS4_2003_HHM.DTA", "gh")
extract_data("/FILEPATH/GHA_DHS5_2008_HHM.DTA", "gh")
extract_data("/FILEPATH/GHA_DHS6_2014_HHM.DTA", "gh")
extract_data("/FILEPATH/GHA_DHS8_2022_2023_HHM.DTA", "gh")

# Malawi
extract_data("/FILEPATH/MWI_DHS4_2000_HHM.DTA", "mw")
extract_data("/FILEPATH/MWI_DHS4_2004_2005_HHM.DTA", "mw")
extract_data("/FILEPATH/MWI_DHS6_2010_HHM.DTA", "mw")
extract_data("/FILEPATH/MWI_DHS7_2015_2016_HHM.DTA", "mw")

# Nepal
extract_data("/FILEPATH/NPL_DHS4_2001_HHM.DTA", "np")
extract_data("/FILEPATH/NPL_DHS5_2006_HHM.DTA", "np")
extract_data("/FILEPATH/NPL_DHS6_2011_HHM.DTA", "np")
extract_data("/FILEPATH/NPL_DHS7_2016_2017_HHM.DTA", "np")
extract_data("/FILEPATH/NPL_DHS8_2022_HHM.DTA", "np")

# Rwanda
extract_data("/FILEPATH/RWA_DHS4_2000_HHM.DTA", "rw")
extract_data("/FILEPATH/RWA_DHS4_2005_HHM.DTA", "rw")
extract_data("/FILEPATH/RWA_DHS6_2010_2011_HHM.DTA", "rw")
extract_data("/FILEPATH/RWA_DHS7_2014_2015_HHM.DTA", "rw")
extract_data("/FILEPATH/RWA_DHS8_2019_2020_HHM.DTA", "rw")

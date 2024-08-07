#-------------------Header------------------------------------------------
# Project: IHME ASHER Decomposition
# Purpose: Extract religion and ethnicity of household head from MICS surveys
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


# EXTRACT DATA FUNCTION -----------------------------------------------

extract_data <- function(survey, cur_country){
  
  # read in data 
  if (grepl("NPL_MICS6", survey)) {
    dt <- data.table(read_sav(survey))
  } else {
    dt <- data.table(read_dta(survey))
  }
  
  
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
  dt[, cluster := ifelse(grepl("MICS2", survey), hi1, hh1)] 
  dt[, hh_id := ifelse(grepl("MICS2", survey), hi2, hh2)]
  dt[, area_unit := ifelse(grepl("MICS2", survey), hi6, hh6)]
  dt[, country := cur_country]
  
  # strata 
  dt[, strata := ifelse(grepl("CMR_MICS2", survey), paste(hi7, strates), 
                        ifelse(grepl("RWA_MICS2", survey), paste(province, hi6),
                               ifelse(grepl("MWI_MICS3", survey), paste(hhdis, hh6),
                                      ifelse(grepl("MWI_MICS5", survey), strata,
                                             ifelse(grepl("CMR_MICS3|GHA_MICS3", survey), paste(hh7, hh6), stratum)))))]

  # household's sample weight
  dt[, hhweight := hhweight]
  
  # unique hh id for each household
  dt[, hhid_unique := paste(hh_id, cluster)]
  
  
  ## HOUSEHOLD CHARACTERISTICS ----------------------------------------
  message("||---Household characteristics")
  
  # religion
  # MISSING FROM RWA 2000, CMR 2000
  if (grepl("MICS[3-6]", survey)) dt[, religion := as.character(as_factor(hc1a))]
  
  # ethnicity
  # MISSING FROM CMR 2000, RWA 2000, NPL 2019
  if (grepl("MICS[3-6]", survey) & !grepl("NPL_MICS6", survey)) {
    dt[, ethnicity_hh := ifelse(grepl("GHA_MICS4|MWI_MICS3", survey), as.character(as_factor(hc1b)),
                                ifelse(grepl("CMR_MICS3|CMR_MICS5|NPL_MICS5|GHA_MICS3", survey), as.character(as_factor(hc1c)), 
                                       as.character(as_factor(ethnicity))))]
  }
  
  
  ## EXPORT CLEANED DATA --------------------------------------------
  message("||---Export cleaned data")
  
  # filter just to variables of interest
  vars_interest <- c("survey","cluster","hh_id","area_unit","country","strata","hhweight","hhid_unique", 
                     "religion","ethnicity_hh")
  cur_vars <- names(dt)[names(dt) %in% vars_interest]
  dt_filter <- dt[, ..cur_vars]
  
  
  file_name <- gsub("\\.DTA|\\.SAV", "", basename(survey))
  output_file_path <- file.path(out.dir, paste0(file_name, "_hh_extract.csv"))
  write.csv(dt_filter, output_file_path, row.names = FALSE)
  
  message(paste0("Successfully saved ", file_name))
}


# RUN EXTRACTIONS ---------------------------------------------------

# Cameroon
extract_data("/FILEPATH/CMR_MICS2_2000_HH.DTA", "cm")
extract_data("/FILEPATH/CMR_MICS3_2006_HH.DTA", "cm")
extract_data("/FILEPATH/CMR_MICS5_2014_HH.DTA", "cm")

# Nepal
extract_data("/FILEPATH/NPL_MICS4_2010_HH.DTA", "np")
extract_data("/FILEPATH/NPL_MICS5_2014_HH.DTA", "np")
extract_data("/FILEPATH/NPL_MICS6_2019_HH.SAV", "np")

# Malawi
extract_data("/FILEPATH/MWI_MICS3_2006_HH.DTA", "mw")
extract_data("/FILEPATH/MWI_MICS5_2013_2014_HH.DTA", "mw")
extract_data("/FILEPATH/MWI_MICS6_2019_2020_HH.DTA", "mw")

# Ghana
extract_data("/FILEPATH/GHA_MICS3_2006_HH.DTA", "gh")
extract_data("/FILEPATH/GHA_MICS4_2011_HH.DTA", "gh")
extract_data("/FILEPATH/GHA_MICS6_2017_2018_HH.DTA", "gh")

# Rwanda
extract_data("/FILEPATH/RWA_MICS2_2000_HH.DTA", "rw")
#-------------------Header------------------------------------------------
# Project: IHME ASHER Decomposition
# Purpose: Extract education variables from household member file from MICS surveys 
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

extract_data <- function(survey, cur_country) {
  
  # read in data 
  dt <- data.table(read_dta(survey))
  
  
  # SURVEY CHARACTERISTICS ----------------------------------------
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
  dt[, id := hl1]
  dt[, area_unit := ifelse(grepl("MICS2", survey), hi6, hh6)]
  dt[, country := cur_country]
  
  # unique hh id for each household
  dt[, hhid_unique := paste(hh_id, cluster)]
  
  # unique individual id for each household member
  dt[, id_unique := paste(hhid_unique, id)]
  
  
  # EDUCATION VARIABLES ----------------------------------------
  message("||---Education variables")
  
  # education of household head
  # MISSING CMR 2000, RWA 2000
  if ("helevel" %in% names(dt)) dt[, education_hh_head := as.character(as_factor(helevel))]
  if (grepl("NPL_MICS6", survey)) dt[, education_hh_head := as.character(as_factor(helevel2))]
  
  # never attended school
  if (grepl("MICS2", survey)) dt[, never_attend_school := ifelse(ed15 == 2, 1, ifelse(ed15 == 1,0,NA))]
  if (grepl("MICS3", survey)) dt[, never_attend_school := ifelse(ed2 == 2, 1, ifelse(ed2 == 1,0,NA))]
  if (grepl("MICS[4-5]", survey)) dt[, never_attend_school := ifelse(ed3 == 2, 1, ifelse(ed3 == 1,0,NA))]
  if (grepl("MICS6", survey)) dt[, never_attend_school := ifelse(ed4 == 2, 1, ifelse(ed4 == 1,0,NA))]
  
  # attended school during current school year 
  if (grepl("MICS2", survey)) dt[, attend_school := ifelse(ed18 ==1,1,ifelse(ed18 == 2,0,NA))]
  if (grepl("MICS3", survey)) dt[, attend_school := ifelse(ed4 ==1,1,ifelse(ed4 == 2,0,NA))]
  if (grepl("MICS[4-5]", survey)) dt[, attend_school := ifelse(ed5 ==1,1,ifelse(ed5 == 2,0,NA))]
  if (grepl("MICS6", survey)) dt[, attend_school := ifelse(ed9 ==1,1,ifelse(ed9 == 2,0,NA))]
  
  # highest education level attended
  # MISSING FROM NPL 2010, 2014 HHM FILE, CAN GET FROM WN
  if (!grepl("NPL_MICS[4-5]", survey)) {
    dt[, edu_level_categ := ifelse(grepl("MICS2", survey), as.character(as_factor(ed16a)),
                                   ifelse(grepl("MICS3", survey), as.character(as_factor(ed3a)),
                                          ifelse(grepl("MICS[4-5]", survey), as.character(as_factor(ed4a)), 
                                                 as.character(as_factor(ed5a)))))]
    dt[, edu_level_categ := tolower(edu_level_categ)]
  }
  
  # highest class/grade within each education level
  dt[, edu_years_in_level := ifelse(grepl("MICS2", survey), ed16b,
                                    ifelse(grepl("MICS3", survey), ed3b,
                                           ifelse(grepl("MICS[4-5]", survey), ed4b, ed5b)))]

  # calculate education level achieved
  
  # set responses such as "dk", "missing", "none" or "preschool" to NA
  mask_edu_years_level <- dt$edu_years_in_level > 90
  mask_edu_years_level <- mask_edu_years_level | dt$edu_level_categ == 0
  dt[mask_edu_years_level == TRUE, edu_years_in_level := NA]
  
  # impute educ_single_yrs using years already completed based on edu_level_categ
  
  # Malawi
  # 8-4-4 system
  # primary = 1-8, secondary (lower and secondary) = 9-12, higher = 13+
  if (grepl("MWI", survey)) {
    dt[edu_level_categ == "ece", educ_single_yrs := 0]
    dt[edu_level_categ == "primary", educ_single_yrs := edu_years_in_level]
    dt[grepl("secondary", edu_level_categ), educ_single_yrs := 8 + edu_years_in_level]
    dt[edu_level_categ == "higher", educ_single_yrs := 12 + edu_years_in_level]
  }
  
  # Ghana
  # 6-3-4-4 system
  # primary = 1-6, JSS/JHS/middle = 7-9, SSS/SHS/secondary = 10-13, higher = 14+
  if (grepl("GHA", survey)) {
    dt[edu_level_categ == "primary", educ_single_yrs := edu_years_in_level]
    dt[grepl("jss|middle", edu_level_categ), educ_single_yrs := 6 + edu_years_in_level]
    dt[grepl("secondary|voc", edu_level_categ), educ_single_yrs := 9 + edu_years_in_level]
    dt[grepl("post sec|tertiary|higher", edu_level_categ), educ_single_yrs := 13 + edu_years_in_level]
  }
  
  # Nepal
  # 8-4 system
  # edu_level_categ is missing for NPL 2010 and NPL 2014
  # educ_single_yrs is already coded as total years, no need to add
  if (grepl("NPL_MICS6", survey)) {
    dt[grepl("basic|secondary", edu_level_categ), educ_single_yrs := edu_years_in_level]
    dt[grepl("higher", edu_level_categ), educ_single_yrs := 13]  # do not have specific years, min is 13 tho
  }
  
  # Cameroon
  # two systems: francophone 6-4-3, anglophone 6-5-2
  # before 2006 primary was either 6 or 7 years, post-2006 it's only 6 years
  # primary = 1-6 (or 7), secondary = 7-13, tertiary = 14+
  if (grepl("CMR", survey)) {
    if (grepl("MICS2", survey)) {
      dt[grepl("primaire", edu_level_categ), educ_single_yrs := edu_years_in_level]
      dt[grepl("secondaire", edu_level_categ), educ_single_yrs := 6 + edu_years_in_level]
      dt[grepl("superieur", edu_level_categ), educ_single_yrs := 13 + edu_years_in_level]
    } else if (grepl("MICS3", survey)) {
      dt[, educ_single_yrs := edu_years_in_level]
    } else if (grepl("MICS5", survey)) {
      dt[edu_years_in_level %in% 10:16, educ_single_yrs := edu_years_in_level - 10]
      dt[edu_years_in_level %in% 20:27, educ_single_yrs := 6 + (edu_years_in_level - 20)]
      dt[edu_years_in_level %in% 30:35, educ_single_yrs := 13 + (edu_years_in_level - 30)]
    }
  }
  
  # Rwanda
  # 6-3-3-4 system 
  # primary = 1-6, junior secondary = 7-9, senior secondary = 10-12, higher = 13+
  if (grepl("RWA", survey)) {
    dt[edu_level_categ == "primaire", educ_single_yrs := edu_years_in_level]
    dt[grepl("secondaire", edu_level_categ), educ_single_yrs := 6 + edu_years_in_level]
    dt[grepl("superieur", edu_level_categ), educ_single_yrs := 13 + edu_years_in_level]
    # post-primary is from the old education system where primary was 8 yrs
    dt[grepl("post-primaire", edu_level_categ), educ_single_yrs := 8 + edu_years_in_level]  
  }
 
  # if never attended any school, set educ_single_yrs to 0
  dt[never_attend_school == 1, educ_single_yrs := 0]
  
  # years of schooling of household head
  dt[, mean_yrs_schooling_head := ifelse(id == 1, educ_single_yrs, NA)]
  dt[, mean_yrs_schooling_head := mean_yrs_schooling_head[!is.na(mean_yrs_schooling_head)][1L], by = "hhid_unique"]  # fill in rest of hh mems
  
  
  # EXPORT CLEANED DATA --------------------------------------------
  message("||---Export cleaned data")
  
  # filter to variables of interest
  vars_interest <- c("survey","cluster","hh_id","id","area_unit","country","hhid_unique","id_unique",
                     "education_hh_head","mean_yrs_schooling_head","attend_school","never_attend_school",
                     "edu_years_in_level","edu_level_categ","educ_single_yrs")
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
extract_data("/FILEPATH/CMR_MICS2_2000_HHM.DTA", "cm")
extract_data("/FILEPATH/CMR_MICS3_2006_HHM.DTA", "cm")
extract_data("/FILEPATH/CMR_MICS5_2014_HHM.DTA", "cm")

# Nepal
extract_data("/FILEPATH/NPL_MICS4_2010_HHM.DTA", "np")
extract_data("/FILEPATH/NPL_MICS5_2014_HHM.DTA", "np")
extract_data("/FILEPATH/NPL_MICS6_2019_HHM.DTA", "np")

# Malawi
extract_data("/FILEPATH/MWI_MICS3_2006_HHM.DTA", "mw")
extract_data("/FILEPATH/MWI_MICS5_2013_2014_HHM.DTA", "mw")
extract_data("/FILEPATH/MWI_MICS6_2019_2020_HHM.DTA", "mw")

# Ghana
extract_data("/FILEPATH/GHA_MICS3_2006_HHM.DTA", "gh")
extract_data("/FILEPATH/GHA_MICS4_2011_HHM.DTA", "gh")
extract_data("/FILEPATH/GHA_MICS6_2017_2018_HHM.DTA", "gh")

# Rwanda
extract_data("/FILEPATH/RWA_MICS2_2000_HHM.DTA", "rw")
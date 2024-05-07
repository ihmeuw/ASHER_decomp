#-------------------Header------------------------------------------------
# Author: Corinne Bintz, last updated by Olivia Angelino 2/12/2024
# Project: ASHER
# Purpose: Extract school attendance from HHM file 
# Date: 1/19/2024
# Notes:
#***************************************************************************

# SET-UP -----------------------------------------------------------

# clear environment
rm(list=ls())
username <- Sys.info()[["user"]]

if (Sys.info()["sysname"] == "Linux") {
  j <- "/snfs1/"
  h <- paste0("/homes/", username, "/")
  r <- "/mnt/"
  l <-"/ihme/limited_use/"
} else {
  j <- "J:/"
  h <- "H:/"
  r <- "R:/"
  l <- "L:/"
}

# load packages
pacman::p_load(magrittr,tidyverse,parallel,plyr,dplyr,haven,survey,tools,devtools)

# in/out
out.dir <- '/share/scratch/projects/hssa/asher/data/01_processed/'

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
extract_data("/snfs1/DATA/DHS_PROG_DHS/CMR/2004/CMR_DHS4_2004_HHM_CMPR45FL_Y2018M05D29.DTA", "cm")
extract_data("/snfs1/DATA/DHS_PROG_DHS/CMR/2011/CMR_DHS5_2011_HHM_CMPR61FL_Y2018M06D11.DTA", "cm")
extract_data("/snfs1/DATA/DHS_PROG_DHS/CMR/2018_2019/CMR_DHS7_2018_2019_HHM_CMPR71FL_Y2020M06D10.DTA", "cm")

# Ghana
extract_data("/snfs1/DATA/DHS_PROG_DHS/GHA/2003/GHA_DHS4_2003_HHM_GHPR4BFL_Y2018M10D15.DTA", "gh")
extract_data("/snfs1/DATA/DHS_PROG_DHS/GHA/2008/GHA_DHS5_2008_HHM_GHPR5AFL_Y2018M10D15.DTA", "gh")
extract_data("/snfs1/DATA/DHS_PROG_DHS/GHA/2014/GHA_DHS6_2014_HHM_GHPR72FL_Y2018M12D10.DTA", "gh")
extract_data("/snfs1/DATA/DHS_PROG_DHS/GHA/2022_2023/GHA_DHS8_2022_2023_HHM_GHPR8AFL_Y2024M01D22.DTA", "gh")

# Malawi
extract_data("/snfs1/DATA/DHS_PROG_DHS/MWI/2000/MWI_DHS4_2000_HHM_MWPR41FL_Y2019M01D07.DTA", "mw")
extract_data("/snfs1/DATA/DHS_PROG_DHS/MWI/2004_2005/MWI_DHS4_2004_2005_HHM_MWPR4EFL_Y2018M12D10.DTA", "mw")
extract_data("/snfs1/DATA/DHS_PROG_DHS/MWI/2010/MWI_DHS6_2010_HHM_MWPR61FL_Y2018M12D10.DTA", "mw")
extract_data("/snfs1/DATA/DHS_PROG_DHS/MWI/2015_2016/MWI_DHS7_2015_2016_HHM_MWPR7AFL_Y2019M12D11.DTA", "mw")

# Nepal
extract_data("/snfs1/DATA/DHS_PROG_DHS/NPL/2001/NPL_DHS4_2001_HHM_NPPR41FL_Y2019M02D19.DTA", "np")
extract_data("/snfs1/DATA/DHS_PROG_DHS/NPL/2006/NPL_DHS5_2006_HHM_NPPR51FL_Y2018M11D05.DTA", "np")
extract_data("/snfs1/DATA/DHS_PROG_DHS/NPL/2011/NPL_DHS6_2011_HHM_NPPR60FL_Y2018M11D05.DTA", "np")
extract_data("/snfs1/DATA/DHS_PROG_DHS/NPL/2016_2017/NPL_DHS7_2016_2017_HHM_NPPR7HFL_Y2018M11D05.DTA", "np")
extract_data("/snfs1/DATA/DHS_PROG_DHS/NPL/2022/NPL_DHS8_2022_HHM_NPPR81FL_Y2023M06D23.DTA", "np")

# Rwanda
extract_data("/snfs1/DATA/DHS_PROG_DHS/RWA/2000/RWA_DHS4_2000_HHM_RWPR41FL_Y2019M03D18.DTA", "rw")
extract_data("/snfs1/DATA/DHS_PROG_DHS/RWA/2005/RWA_DHS4_2005_HHM_RWPR53FL_Y2019M03D18.DTA", "rw")
extract_data("/snfs1/DATA/DHS_PROG_DHS/RWA/2010_2011/RWA_DHS6_2010_2011_HHM_RWPR61FL_Y2019M04D16.DTA", "rw")
extract_data("/snfs1/DATA/DHS_PROG_DHS/RWA/2014_2015/RWA_DHS7_2014_2015_HHM_RWPR70FL_Y2019M04D16.DTA", "rw")
extract_data("/snfs1/DATA/DHS_PROG_DHS/RWA/2019_2020/RWA_DHS8_2019_2020_HHM_RWPR81FL_Y2021M10D05.DTA", "rw")
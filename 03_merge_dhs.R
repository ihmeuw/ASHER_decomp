#-------------------Header------------------------------------------------
# Author: Corinne Bintz, last updated by Olivia Angelino 2/13/2024
# Project: ASHER
# Purpose: Merge extracted DHS wn, hhm, and wealth quintile files
# Date: 1/10/2024
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
pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer,haven,survey)

# in/out
in.dir <- '/share/scratch/projects/hssa/asher/data/01_processed/'
out.dir <- '/share/scratch/projects/hssa/asher/data/02_merged'

# create function for the opposite of %in%
'%ni%' <- Negate('%in%')


# PREP FILES ---------------------------------------------------------

# countries to merge files for
countries <- c("cm", "gh", "mw", "np", "rw")

# loop through each country to merge
for (cur_country in countries) {
  
  print(cur_country)
  
  # list csv files in processed directory
  files <- list.files(in.dir, pattern = ".csv")
  
  # subset to files for country of interest
  files <- files[grepl(cur_country, tolower(files))]
  
  # subset to DHS files 
  files <- files[grepl("DHS", files)]
  
  # list of surveys
  surveys <- unique(str_extract(files, "[A-Z]{3}_DHS[0-9]_[0-9]{4}(_[0-9]{4})*"))
  
  # loop through each survey to merge
  for (survey in surveys) {
    
    print(survey)
    
    ## wn: _wn_extract.csv
    ## hhm: _hhm_extract.csv
    ## wealth quintile: _wealth_quintile_prep.csv
    ## gps: _gps_extract.csv

    wn_file <- files[grepl(survey, files) & grepl("_wn_extract", files)]
    hhm_file <- files[grepl(survey, files) & grepl("_hhm_extract", files)]
    wq_file <- files[grepl(survey, files) & grepl("_wealth_quintile_prep", files)]
    gps_file <- files[grepl(survey, files) & grepl("_gps_extract", files)]

    ## read in wn level extracted data
    wn_dt <- fread(file.path(in.dir, wn_file))
    
    ## read in hhm level extracted data
    hhm_dt <- fread(file.path(in.dir, hhm_file))
    
    ## read in wealth quintile extracted data
    wq_dt <- fread(file.path(in.dir, wq_file))
    
    ## read in GPS extracted data, if available
    if (!is_empty(gps_file)) gps_dt <- fread(file.path(in.dir, gps_file))
    
    
    # MERGE FILES ---------------------------------------------------------
    
    ## merge wn and hhm files, all women should appear in the hhm file
    merged_dt_1 <- merge(wn_dt, hhm_dt, by = intersect(names(wn_dt), names(hhm_dt)))
    
    # quick check
    try(if(nrow(merged_dt_1) != nrow(wn_dt)) stop("Women getting dropped! Misalignment between wn and hhm files!"))
    
    ## merge merged_dt_1 (women) and wq file, keeping only households that match onto a women
    merged_dt_2 <- merge(merged_dt_1, wq_dt, by = intersect(names(merged_dt_1), names(wq_dt)))
    
    # quick check
    try(if(nrow(merged_dt_2) != nrow(merged_dt_1)) stop("Women getting dropped! Misalignment between merged_dt_1 and wq files!"))
    
    
    # MERGE CONSTRUCTED WEALTH QUINTILES ------------------------------
    
    ## read in wealth quintile estimates for country of interest
    wq_estimates <- fread(file.path(in.dir,"wealth_quintile_construction",paste0(cur_country,"_wealth_quintile_estimates.csv")))
    
    ## subset to survey of interest
    wq_estimates <- wq_estimates[survey == get("survey", envir = 1)][, -c("wscore","windex5")]
    
    ## merge onto merged_dt_3, keep all women
    merged_dt_3 <- merge(merged_dt_2, wq_estimates, by = c("survey", "hhid_unique"), all.x = T)
    
    
    # MERGE ON GPS COORDINATES ----------------------------------------
    
    ## merge gps file onto merged_dt_3, if gps file is available, keep all wn
    if (!is_empty(gps_file)) merged_dt_3 <- merge(merged_dt_3, gps_dt, by = intersect(names(merged_dt_3), names(gps_dt)), all.x = T)
    
    
    # SAVE MERGED FILE ------------------------------------------------
    
    # export
    write.csv(merged_dt_3, file.path(out.dir, paste0(survey, "_merged.csv")), row.names = F) 
    
    message(paste0("Successfully saved ", survey))
  }
}

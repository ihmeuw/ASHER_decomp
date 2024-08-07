#-------------------Header------------------------------------------------
# Project: IHME ASHER Decomposition
# Purpose: Merge extracted MICS wn, hh, hhm, bh, and wealth quintile files
# Date: 1/10/2024
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
pacman::p_load(data.table,magrittr,tidyverse,parallel,plyr,dplyr,haven,survey)

# in/out
in.dir <- "FILEPATH"
out.dir <- "FILEPATH"

# create function for the opposite of %in%
'%ni%' <- Negate('%in%')


# PREP FILES --------------------------------------------------------

# countries to merge files for
countries <- c("gh", "mw", "cm", "np")

# loop through each country to merge
for (cur_country in countries) {
  
  print(cur_country)
  
  # list csv files in processed directory
  files <- list.files(in.dir, pattern = ".csv")
  
  # subset to files for country of interest
  files <- files[grepl(cur_country, tolower(files))]
  
  # subset to extracted MICS files 
  files <- files[grepl("MICS[3-6]", files) & grepl("_extract|_prep", files)]
  
  # list of surveys
  surveys <- unique(str_extract(files, "[A-Z]{3}_MICS[3-6]_[0-9]{4}(_[0-9]{4})*"))
  
  # loop through each survey to merge
  for (survey in surveys) {
    
    print(survey)
    
    ## wn: _wn_extract.csv                 women file
    ## hh: _hh_extract.csv                 household file
    ## hhm: _hhm_extract.csv               household member file
    ## bh: _bh_extract.csv                 birth history file
    ## wq: _wealth_quintile_prep.csv       wealth quintile file
    
    wn_file <- files[grepl(survey, files) & grepl("_wn_extract", files)]
    hh_file <- files[grepl(survey, files) & grepl("_hh_extract", files)]
    hhm_file <- files[grepl(survey, files) & grepl("_hhm_extract", files)]
    bh_file <- files[grepl(survey, files) & grepl("_bh_extract", files)]
    wq_file <- files[grepl(survey, files) & grepl("_wealth_quintile_prep", files)]
    
    ## read in wn level extracted data
    wn_dt <- fread(file.path(in.dir, wn_file))
    
    ## read in hh level extracted data
    hh_dt <- fread(file.path(in.dir, hh_file))
    
    ## read in hhm level extracted data
    hhm_dt <- fread(file.path(in.dir, hhm_file))
    
    ## read in wealth quintile extracted data
    wq_dt <- fread(file.path(in.dir, wq_file))
    
    ## read in bh level extracted data, if available
    if (!is_empty(bh_file)) bh_dt <- fread(file.path(in.dir, bh_file))
    
    
    # MERGE FILES -----------------------------------------------------
    
    ## merge hh and wq files, both came from hh module so should have same number of rows
    merged_dt_1 <- merge(hh_dt, wq_dt, by = intersect(names(wq_dt), names(hh_dt)))
    
    # quick check
    try(if(nrow(merged_dt_1) != nrow(hh_dt)) stop("Households getting dropped! Misalignment between hh and wq files!"))
    
    
    ## merge wn and hhm files, all women should appear in the hhm file
    merged_dt_2 <- merge(wn_dt, hhm_dt, by = intersect(names(wn_dt), names(hhm_dt)))
    
    # quick check
    try(if(nrow(merged_dt_2) != nrow(wn_dt)) stop("Women getting dropped! Misalignment between wn and hhm files!"))
    
    
    ## merge bh file onto merged_dt_2, if bh file is available, keep all wn
    if (!is_empty(bh_file)) merged_dt_2 <- merge(merged_dt_2, bh_dt, by = intersect(names(wn_dt), names(bh_dt)), all.x = T)
    
    
    ## merge merged_dt_1 (households) and merged_dt_2 (women), keeping only households that match onto a woman
    merged_dt_3 <- merge(merged_dt_2, merged_dt_1, by = intersect(names(merged_dt_2), names(merged_dt_1)))
    
    # quick check
    try(if(nrow(merged_dt_3) != nrow(wn_dt)) stop("Women getting dropped! Misalignment between merged_dt_1 and merged_dt_2!"))
    
    
    # MERGE CONSTRUCTED WEALTH QUINTILES ------------------------------
    
    ## read in wealth quintile estimates for country of interest
    wq_estimates <- fread(file.path(in.dir,"wealth_quintile_construction",paste0(cur_country,"_wealth_quintile_estimates.csv")))
    
    ## subset to survey of interest
    wq_estimates <- wq_estimates[survey == get("survey", envir = 1)][, -c("wscore","windex5")]
    
    ## merge onto merged_dt_3, keep all women
    merged_dt_4 <- merge(merged_dt_3, wq_estimates, by = c("survey", "hhid_unique"), all.x = T)
    
    
    # SAVE MERGED FILE ------------------------------------------------
    
    # export
    write.csv(merged_dt_4, file.path(out.dir, paste0(survey, "_merged.csv")), row.names = F) 
    
    message(paste0("Successfully saved ", survey))
  }
}

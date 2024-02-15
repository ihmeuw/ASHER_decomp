## Created by: Corinne Bintz
## Creation date: 11/30/2023
## merge hh, wn, school attendance, and wealth quintile for DHS
## specify current country with cur_country

## Last updated by: Olivia Angelino
## Update date: 1/17/24


# SET UP --------------------------------------------------------------

rm(list=ls())
username <- Sys.info()[["user"]]

if (Sys.info()["sysname"] == "Linux") {
  j <- "/home/j/"
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
pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey)

# in/out
in.dir <- '/share/scratch/projects/hssa/asher/processed/'
out.dir <- '/share/scratch/projects/hssa/asher/processed/merged'


# PREP FILES ---------------------------------------------------------

# countries to merge files for
countries <- c("cm", "gh", "mw", "np", "rw")

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
    
    ## wn: _wn_processsed.csv
    ## school attendance: _indiv_in_household.csv
    ## hh: _wn_hh_processsed.csv
    ## wealth quintile: _hh_wealth_quintile.csv
    
    school_attendance_file <- files[grepl(survey, files) & grepl("_indiv_in_household.csv", files)]
    hh_file <- files[grepl(survey, files) & grepl("_wn_hh_processsed.csv", files)] 
    wealth_quintile_file <- files[grepl(survey, files) & grepl("_wealth_quintile_dt_summary.csv", files)] 
    wn_file <- files[grepl(survey, files) & grepl("_wn_processsed.csv", files)]
    
    ## read in wn level extracted data
    wn_dt <- fread(file.path(in.dir, wn_file))[, survey := survey]
    wn_dt[, country := tolower(country)]
    wn_dt[, hhid_unique := tolower(paste(country, year, hh_id, cluster))]
    wn_dt[, cluster_unique := tolower(cluster_unique)]
    wn_dt[, psu_unique := tolower(psu_unique)]
    
    ## read in hh level extracted data
    hh_dt <- fread(file.path(in.dir, hh_file))[, survey := survey]
    hh_dt[, country := tolower(country)]
    hh_dt[, hhid_unique := tolower(paste(country, year, hh_id, cluster))]
    
    ## read in school attendance extracted data
    school_attendance_dt <- fread(file.path(in.dir, school_attendance_file))[, survey := survey]
    
    ## read in wealth quintile extracted data
    wealth_quintile_dt <- fread(file.path(in.dir, wealth_quintile_file))
    
    
    # MERGE FILES ---------------------------------------------------------
    
    ## merge wealth quintile and household files together
    merged_dt_1 <- merge(wealth_quintile_dt, hh_dt[,-c("id")], by = c( "survey","year", "hhid_unique"))
    
    ## merge merged wealth quintile and hh file onto wn file, keeping all wn
    merged_dt_2 <- merge(merged_dt_1, wn_dt[, -c("id_unique", "region", "strata", "cmc_interview_date")],
                         by = c("survey", "year", "hhid_unique", "country", "cluster", "hh_id"))
    
    ## merge merged wn, hh, wealth quintile file onto school attendance file, keeping all wn
    merged_dt_3 <- merge(merged_dt_2, school_attendance_dt[, c("survey", "hh_id", "country", "attend_school", "year", "cluster", "id")],
                         by = c("survey", "hh_id", "country", "year", "cluster", "id"))
    
    write.csv(merged_dt_3, file.path(out.dir, paste0(survey, "_merged.csv")), row.names = F) 
  }
}
## Created by: Corinne Bintz
## Creation date: 1/10/24
## merge wn, hhm, hh, and birth history for mics
## eventually add wealth quintile once fixed
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
out.dir <- '/share/scratch/projects/hssa/asher/processed/'
files<-list.files(in.dir)

#countries <- c("gh", "mw")
countries <- c( "mw")# can only do mw for now because haven't done ghana wealth quintile endline 
for (cur_country in countries) {

country_title <- ifelse(cur_country == "cm", "Cameroon",
                        ifelse(cur_country == "gh", "Ghana", 
                               ifelse(cur_country== 'mw', "Malawi",
                                      ifelse(cur_country == "np", "Nepal", "Rwanda"))))
## for now, filter to first and last years
first_year <- ifelse(cur_country == "cm", 2000,
                     ifelse(cur_country == "gh", 2006, 
                            ifelse(cur_country== 'mw',2000,
                                   ifelse(cur_country == "np",2006,2000))))

last_year <- ifelse(cur_country == "cm", 2014,
                    ifelse(cur_country == "gh", 2017, 
                           ifelse(cur_country== 'mw',2019,
                                  ifelse(cur_country == "np",2021,2019))))

first_last_years <- c(first_year,last_year)

  
  # list csv files in processed directory
  files <- list.files(in.dir, pattern = ".csv")
  
  # subset to files for country of interest
  files <- files[grepl(cur_country, tolower(files))]
  
  # subset wealth quintile files which do not have "_extract" in the name
  wealth_index_files <- files[grepl("MICS",files) & grepl("_wealth_quintile_dt_summary.csv", tolower(files))]
  
  # subset to extracted MICS files 
  files <- files[grepl("MICS", files) & grepl("_extract", files)]
  
  wn_files <- files[grepl("_wn_extract.csv", tolower(files))]
  hh_files <- files[grepl("_hh_extract.csv", tolower(files))]
  hhm_files <- files[grepl("_hhm_extract.csv", tolower(files))]
  bh_files <- files[grepl("_bh_extract.csv", tolower(files))]

  
  adjust_years <- function(dt){
    ## adjust years to just be one 
    if (cur_country == "cm"){
      dt[, year := ifelse(year %in% c(2018,2019), 2018, year)]
    }
    if (cur_country == "mw"){
      dt[, year := ifelse(year %in% c(2019,2020), 2019, year)]
    }
    
    if (cur_country == "np"){
      dt[, year := ifelse(year %in% c(2005,2006), 2006,
                          ifelse(year %in% c(2021,2022), 2021,year))]
    }
    if (cur_country == "rw"){
      dt[, year := ifelse(year %in% c(2020,2019), 2017, year)]
      
    }
    if (cur_country == "gh"){
      dt[, year := ifelse(year %in% c(2017,2018), 2017, year)]
    }
  }
  
## read in wn level extracted data
wn_dt <-  rbindlist(lapply(file.path(in.dir, wn_files), fread), fill = TRUE)
adjust_years(wn_dt)
wn_dt <- wn_dt[year %in% first_last_years]
## small fixed for merging
wn_dt[, region := as.numeric(region)]

## read in hh level extracted data
hh_dt <-  rbindlist(lapply(file.path(in.dir, hh_files), fread), fill = TRUE)
adjust_years(hh_dt)
hh_dt <- hh_dt[year %in% first_last_years]
hh_dt[, strata := as.numeric(strata)]

## read in hhm level extracted data
hhm_dt <-  rbindlist(lapply(file.path(in.dir, hhm_files), fread), fill = TRUE)
adjust_years(hhm_dt)
hhm_dt <- hhm_dt[year %in% first_last_years]

## read in bh level extracted data
bh_dt <-  rbindlist(lapply(file.path(in.dir, bh_files), fread), fill = TRUE)
adjust_years(bh_dt)
bh_dt <- bh_dt[year %in% first_last_years]

## read in wealth quintile level extracted data
wealth_index_dt <-  rbindlist(lapply(file.path(in.dir, wealth_index_files), fread), fill = TRUE)
adjust_years(wealth_index_dt)
wealth_index_dt <- wealth_index_dt[year %in% first_last_years]

## merge wn files onto bh files, keep all women regardless of if they are in bh file
merged_dt_1 <- merge(wn_dt[, -c('survey')], bh_dt[, -c('survey')], by = c("year", "hhid_unique","id_unique", "psu", "cluster", "hh_id",
                                          "area_unit", "country", "wpweight", "pweight", "strata", "region",
                                          "cluster_unique", "psu_unique"), all.x=T)


## merge merged wn and bh file onto hhm file, keeping only hhm in wn files
merged_dt_2 <- merge(merged_dt_1, hhm_dt[, -c('survey')], by = c("year", "hhid_unique","id_unique","psu", "cluster",
                                                 "area_unit", "country", "strata", "region",
                                                 "cluster_unique", "psu_unique"))

## merge merged wn, bh, and hhm file onto hh file, keeping only hh in wn files
merged_dt_3 <- merge(merged_dt_2, hh_dt[, -c('survey')], by = c("year", "hhid_unique","psu", "cluster",
                                                "area_unit", "country", "strata", "region",
                                                "cluster_unique", "psu_unique", 'hhweight'))

## merge merged wn, bh, hhm, and hh file onto wealth quintile files. All households should have a wealth quintile value 
merged_dt_4 <- merge(merged_dt_3, wealth_index_dt, by = c("year", "hhid_unique"))

setnames(merged_dt_4, 'hhweight', 'hhpweight') ## to match dhs

# unique strata for each nid
merged_dt_4[, strata_unique := tolower(paste(country, year, strata))]

write.csv(merged_dt_4, file.path(out.dir, paste0(cur_country, "_merged_endline_mics.csv")), row.names = F)
}


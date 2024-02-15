## Created by: Corinne Bintz
## Creation date: 11/30/2023
## merge hh, wn, school attendance, and wealth quintile for first and last years of a given country
## specify current country with cur_country
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
pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source)) # IHME-specific line. comment out if running outside IHME

in.dir <- '/share/scratch/projects/hssa/asher/processed/'
out.dir <- '/share/scratch/projects/hssa/asher/processed/'
files<-list.files(in.dir)

## wn: _wn_processsed.csv
## school attendance: _indiv_in_household.csv
## hh: _wn_hh_processsed.csv
## wealth quintile: _hh_household.csv
countries <- c("cm", "gh", "mw", "np", "rw")

for (cur_country in countries) {
print(cur_country)
country_title <- ifelse(cur_country == "cm", "Cameroon",
                        ifelse(cur_country == "gh", "Ghana", 
                               ifelse(cur_country== 'mw', "Malawi",
                                      ifelse(cur_country == "np", "Nepal", "Rwanda"))))
first_year <- ifelse(cur_country == "cm", 2004,
                     ifelse(cur_country == "gh", 2003, 
                            ifelse(cur_country== 'mw',2000,
                                   ifelse(cur_country == "np",2006,2000))))

last_year <- ifelse(cur_country == "cm", 2018,
                        ifelse(cur_country == "gh", 2022, 
                               ifelse(cur_country== 'mw',2015,
                                      ifelse(cur_country == "np",2021,2019))))

first_last_years <- c(first_year,last_year)


files<-list.files(in.dir)
files <-files[grepl(cur_country, tolower(files))]
files <-files[!grepl(".png", tolower(files))]
#files <-files[!grepl("hh_wealth_quintile_dt", tolower(files))]

#hh_data_current <- dplyr::filter(wn_data, grepl(toupper(cur_country), country))

#cur_data <- rbindlist(lapply(file.path(in.dir, files), fread), fill = TRUE)

school_attendance_files <- files[grepl("_indiv_in_household.csv", tolower(files))]
hh_files <- files[grepl("_wn_hh_processsed.csv", tolower(files))]
hh_files <- hh_files[!grepl("GHA_DHS7_2014", hh_files)] ## TODO: fix this survey extraction 

wealth_quintile_files <- files[grepl("_wealth_quintile_dt_summary", tolower(files))]
wn_files <- files[grepl("_wn_processsed.csv", tolower(files))]
wn_files <- wn_files[!grepl("GHA_DHS7_2014", wn_files)] ## TODO: fix this survey extraction 

adjust_years <- function(dt){
  ## adjust years to just be one 
  if (cur_country == "cm"){
    dt[, year := ifelse(year %in% c(2018,2019), 2018, year)]
  }
  if (cur_country == "mw"){
    dt[, year := ifelse(year %in% c(2015,2016), 2015, year)]
  }
  
  if (cur_country == "np"){
    dt[, year := ifelse(year %in% c(2005,2006), 2006,
                                  ifelse(year %in% c(2021,2022), 2021,year))]
  }
  if (cur_country == "rw"){
    dt[, year := ifelse(year %in% c(2020,2019), 2019, year)]
    
  }
  if (cur_country == "gh"){
    dt[, year := ifelse(year %in% c(2022,2023), 2022, year)]
    
  }
}

wealth_quintile_dt <-  rbindlist(lapply(file.path(in.dir, wealth_quintile_files), fread), fill = TRUE)
wealth_quintile_dt <- wealth_quintile_dt[year %in% first_last_years]

school_attendance_dt <-  rbindlist(lapply(file.path(in.dir, school_attendance_files), fread), fill = TRUE)
adjust_years(school_attendance_dt)
school_attendance_dt <- school_attendance_dt[year %in% first_last_years]

hh_dt <-  rbindlist(lapply(file.path(in.dir, hh_files), fread), fill = TRUE)

wn_dt <-  rbindlist(lapply(file.path(in.dir, wn_files), fread), fill = TRUE)

hh_dt[, hhid_unique := tolower(paste(country, year, hh_id, cluster))] ## added cluster for cameroon 
hh_dt[, country := tolower(country)]

wn_dt[, hhid_unique := tolower(paste(country, year, hh_id, cluster))] ## added cluster for cameroon 
#unique cluster id for each nid
wn_dt[, cluster_unique := tolower(paste(country, year, cluster))]
# unique psu for each nid
wn_dt[, psu_unique := tolower(paste(country, year, psu))]
# unique strata for each nid
wn_dt[, strata_unique := tolower(paste(country, year, strata))]
# unique individual id for each nid
wn_dt[, id_unique := tolower(paste(country, year, id))]
wn_dt[, country := tolower(country)]

#filter to just first and last years
adjust_years(hh_dt)
hh_dt <- hh_dt[year %in% first_last_years]

adjust_years(wn_dt)
wn_dt <- wn_dt[year %in% first_last_years]

adjust_years(wealth_quintile_dt)
wealth_quintile_dt <- wealth_quintile_dt[year %in% first_last_years]

merged_dt_1 <- merge(wealth_quintile_dt, hh_dt[,-c("id")], by = c("year","hhid_unique"))
merged_dt_2 <- merge(merged_dt_1, wn_dt,
                     by = c("year","hhid_unique", "country", "cluster", "hh_id"))
merged_dt_3 <- merge(merged_dt_2, school_attendance_dt[, c("hh_id", "country", "attend_school", "year", "cluster", "id")],
                     by = c( "hh_id", "country", "year", "cluster", "id"))

write.csv(merged_dt_3, file.path(out.dir, paste0(cur_country, "_merged_first_last.csv")), row.names = F)
}

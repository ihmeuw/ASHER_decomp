#-------------------Header------------------------------------------------
# Project: IHME ASHER Decomposition
# Purpose: Calculate means in covariates and outcomes by admin 1
# Date: 6/19/2024
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
pacman::p_load(magrittr,tidyverse,parallel,plyr,dplyr,haven,survey,tools,devtools,sf)

# in/out
out.dir <- 'FILEPATH'

# create function to convert CMC date to year and month
get_cmc_year <- function(x) 1900 + as.integer((as.integer(x) - 1) / 12)
get_cmc_month <- function(x) as.integer(x) - 12 * (get_cmc_year(x) - 1900)

# current admin 2 shapefile
shp2 <- st_read(file.path("FILEPATH"))

files <- list.files(file.path(out.dir, "01_processed"))
gps_files <- files[grepl("gps", files)]
get_admin_1_means <- function(dt_file, cur_country){
  dt <- fread(file.path('FILEPATH', dt_file))
  # read in gps data
  gps_files_tmp <- gps_files[grepl(cur_country, gps_files)]
  gps_data <- rbindlist(lapply(file.path(out.dir, "01_processed", gps_files_tmp), fread), fill = T)
  
  ## construct outcome to use with dhs endline: in the last 2 years or currently preg, include terminations 
  dt[, term_2_years := ifelse(term_recall<25, 1, 0)] # termination within 3 years of interview
  dt[any_births_2_yr ==1 | curr_preg ==1 | term_2_years ==1, any_birth_preg_2_yr_dhs := 1]
  dt[number_births == 0 & curr_preg ==0 & (is.na(term_2_years) |  term_2_years == 0), any_birth_preg_2_yr_dhs := 0]
  
  ## construct outcome to use with mics endline: in the last 2 years or currently preg, exclude terminations 
  dt[any_births_2_yr ==1 | curr_preg ==1 , any_birth_preg_2_yr_mics := 1]
  dt[number_births == 0 & curr_preg ==0, any_birth_preg_2_yr_mics := 0]
  
  ## adjust years
  dt[country == "np", year := get_cmc_year(cmc_interview_date - ((56*12)+8))]
  dt[, year := min(year), by = "survey"]
  
  ## rename wealth quintiles to wealth index
  setnames(dt, "wealth_quintiles", "wealth_index")
  
  # variables of interest 
  variable_list <- data.table(read_xlsx('FILEPATH'))
  variable_list <- variable_list[DHS_phase1 == 'Y']
  
  variables <- c(variable_list$Variable, "country", 'year','any_birth_preg_2_yr_dhs', 'any_birth_preg_2_yr_mics', 'cluster', 
                 'pweight', 'survey', 'mean_yrs_schooling_head', 'mcpr')
  
  dt <- dt[,..variables]
  dt <- dt[age %in% c(seq(15,19))]
  # list of variables to take the mean of 
  variables_means <- c(variable_list$Variable,'any_birth_preg_2_yr_dhs', 'any_birth_preg_2_yr_mics')
  
  # merge data onto gps data
  dt <- merge(dt, gps_data, by = c("country", "cluster","survey"))
  
  # create survey object 
  data_svy <- svydesign(id= ~cluster,  weights = ~pweight, data = dt, nest = TRUE)
  
  # take survey weighted mean of each variable by year, survey, and admin 1
  means_dt <- data.table()
  for (var in variables_means){
    means_tmp <- svyby(~get(var), ~year+survey+admin_1_shp, data_svy , svymean,vartype = c("se", "ci"), na.rm=T) 
    means_tmp <- data.table(means_tmp)
    setnames(means_tmp, "get(var)", "mean")
    means_tmp[, variable := eval(var)]
    means_dt <- rbind(means_dt, means_tmp)
  }
  
  means_dt[, country := cur_country]
  # save csv
  write.csv(means_dt, file.path(out.dir, '01_processed', paste0(cur_country, '_admin_1_means.csv')), row.names=F)
}

get_admin_1_means("cm_all_yrs_prepped.csv", "CMR")
get_admin_1_means("gh_all_yrs_prepped.csv", "GHA")
get_admin_1_means("mw_all_yrs_prepped.csv", "MWI")
get_admin_1_means("np_all_yrs_prepped.csv", "NPL")
get_admin_1_means("rw_all_yrs_prepped.csv", "RWA")



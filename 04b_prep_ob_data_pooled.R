## HEADER --------
## Project: IHME ASHER Decomposition
## Creation date: 6/26/2024
## Set up data for ASHER Oaxaca-Blinder analysis using pooled data instead of just baseline endline

rm(list=ls())
username <- Sys.info()[["user"]]

if (Sys.info()["sysname"] == "Linux") {
  j <- "filepath"
  h <- "filepath"
  r <- "filepath"
  l <- "filepath"
} else {
  j <- "filepath"
  h <- "filepath"
  r <- "filepath"
  l <- "filepath"
}

'%ni%' <- Negate('%in%')

pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey,tidyr,glmnet)

# create function to convert CMC date to year and month
get_cmc_year <- function(x) 1900 + as.integer((as.integer(x) - 1) / 12)
get_cmc_month <- function(x) as.integer(x) - 12 * (get_cmc_year(x) - 1900)

# directories
out.dir <- file.path('filepath', Sys.Date())
dir.create(out.dir, recursive = T)

in.dir <- 'filepath'

prep_data <- function(endline){
  files <- list.files(in.dir)
  files <- files[grepl("all_yrs_prepped", files)]
  ind_dt <-rbindlist(lapply(file.path(in.dir, files), fread), fill = TRUE)
  ind_dt[country == "MW", wealth_quintiles := ifelse(is.na(wealth_quintiles), windex5, wealth_quintiles)]
  
    ## rename wealth quintiles to wealth index
  setnames(ind_dt, "wealth_quintiles", "wealth_index")
  
  # exclude surveys:
  # CMR MICS 2000: missing sexual activity (already excluded)
  # RWA MICS 2000: missing any live births in the past 2 years (already excluded)
  # NPL DHS 2001: ever married only
  # NPL MICS 2010: missing sexual activity
  # NPL MICS 2014: missing sexual activity
  # NPL MICS 2018-2019: ever married only
  
  ind_dt <- ind_dt[survey %ni% c("NPL_DHS4_2001","NPL_MICS4_2010", "NPL_MICS5_2014", "NPL_MICS6_2019")]
  
  ## CREATE OUTCOMES ----
  ## construct outcome to use with dhs endline: in the last 2 years or currently preg, include terminations 
  ind_dt[, term_2_years := ifelse(term_recall<25, 1, 0)] # termination within 3 years of interview
  ind_dt[any_births_2_yr ==1 | curr_preg ==1 | term_2_years ==1, any_birth_preg_2_yr_dhs := 1]
  ind_dt[number_births == 0 & curr_preg ==0 & (is.na(term_2_years) |  term_2_years == 0), any_birth_preg_2_yr_dhs := 0]
  
  ## construct outcome to use with mics endline: in the last 2 years or currently preg, exclude terminations 
  ind_dt[any_births_2_yr ==1 | curr_preg ==1 , any_birth_preg_2_yr_mics := 1]
  ind_dt[number_births == 0 & curr_preg ==0, any_birth_preg_2_yr_mics := 0]
  
  ## adjust years
  ind_dt[country == "np", year := get_cmc_year(cmc_interview_date - ((56*12)+8))]
  ind_dt[, year := min(year), by = "survey"]
  
  # CLEAN DATA --------
  ## read in list of variable names
  variable_list <- data.table(read_xlsx('filepath/variable_availability.xlsx'))
  variable_list <- variable_list[DHS_phase1 == 'Y']
  if (endline == "DHS"){
    variables <- c(variable_list$Variable, "country", 'year','any_birth_preg_2_yr_dhs', 'cluster', 'pweight')
  } else{
    variables <- c(variable_list$Variable, "country", 'year','any_birth_preg_2_yr_mics', 'cluster', 'pweight')
  }
  
  ## subset input data just to these variables
  input_df <- ind_dt[,names(ind_dt)%in% variables,with=FALSE]
  
  ## filter to just women aged 15-24
  input_df_15_24 <- input_df[age %in% c(seq(15,24))] 
  
  ## subpop: had sex
  # age_1st_sex_imp
  input_df_15_24[had_intercourse == 0 & is.na(age_1st_sex_imp), age_1st_sex_imp := 0]
  
  ## determine percent missing per variable
  missing_dt_pooled <- data.table()
  for (var in colnames(input_df_15_24)){
    nobs_var <- nrow(input_df_15_24[!is.na(get(var))])
    nrows_missing <- nrow(input_df_15_24[is.na(get(var))])
    nrows_total <- nrow(input_df_15_24) 
    missing <- nrows_missing/nrows_total
    tmp_dt <- data.table(variable = var, percent_missing = missing, nobs = nobs_var)
    missing_dt_pooled <- rbind(missing_dt_pooled, tmp_dt)
  }
  
  ## filter out na's and determine how many rows we drop
  nrow(input_df_15_24)
  input_df_15_24_no_na <- na.omit(input_df_15_24) 
  nrow(input_df_15_24_no_na)
  
  ## make wealth index categorical as it is specified in ob
  input_df_15_24_no_na[, wealth_dummies1 := ifelse(wealth_index ==1, 1, 0)]
  input_df_15_24_no_na[, wealth_dummies2 := ifelse(wealth_index ==2, 1, 0)]
  input_df_15_24_no_na[, wealth_dummies3 := ifelse(wealth_index ==3, 1, 0)]
  input_df_15_24_no_na[, wealth_dummies4 := ifelse(wealth_index ==4, 1, 0)]
  input_df_15_24_no_na[, wealth_dummies5 := ifelse(wealth_index ==5, 1, 0)]
  
  # unique psu for each nid
  input_df_15_24_no_na[, psu_unique := paste(country, year, cluster)]
  
  ## EXPORT DATA ---------
  write.csv(input_df_15_24_no_na, file.path(out.dir, paste0('ob_input_prepped_df_pooled_', tolower(endline), '.csv')), row.names=F)
}
prep_data("MICS")
prep_data("DHS")
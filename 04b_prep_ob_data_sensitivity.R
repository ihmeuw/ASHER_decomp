##-------------------Header------------------------------------------------
## Project: IHME ASHER Decomposition
## Creation date: 2/28/24
## Set up data for ASHER Oaxaca-Blinder sensitivity analysis with extended covariates

rm(list=ls())
username <- Sys.info()[["user"]]

if (Sys.info()["sysname"] == "Linux") {
  j <- "filepath"
  h <- "filepath"
  r <- "filepath"
  l <- "filepath"
} else {
  j <-"filepath"
  h <-"filepath"
  r <-"filepath"
  l <-"filepath"
}

'%ni%' <- Negate('%in%')

## set endline to either mics or dhs (for Malawi)
endline <- "DHS"

pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey,tidyr,glmnet)

# create function to convert CMC date to year and month
get_cmc_year <- function(x) 1900 + as.integer((as.integer(x) - 1) / 12)
get_cmc_month <- function(x) as.integer(x) - 12 * (get_cmc_year(x) - 1900)

# directories
out.dir <- file.path("filepath", Sys.Date())
dir.create(out.dir, recursive = T)

in.dir <- "filepath"

## LOAD DATA --------
if (endline == "DHS"){
  files <- list.files(in.dir)
  ind_dt <-rbindlist(lapply(file.path(in.dir, files), fread), fill = TRUE)
} else{
  files <- list.files(in.dir)
  files <- files[!grepl("mw", files)]
  ind_dt <-rbindlist(lapply(file.path(in.dir, files), fread), fill = TRUE)
  
  ## read in malawi data
  files <- list.files(in.dir)
  files <- files[grepl("mw_baseline_endline_mics.csv", files)]
  mw_dt <-rbindlist(lapply(file.path(in.dir, files), fread), fill = TRUE)
  mw_dt[, wealth_quintiles := ifelse(is.na(wealth_quintiles), windex5, wealth_quintiles)]
  mw_dt <- mw_dt[, -c("windex5")]
  
  ## add on malawi
  ind_dt <- rbind(ind_dt, mw_dt, fill = T)
}

## rename wealth quintiles to wealth index
setnames(ind_dt, "wealth_quintiles", "wealth_index")

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

## create rural
ind_dt[, rural := ifelse(urban == 0,1,0)]

# CLEAN DATA --------
## read in list of variable names
variable_list <- data.table(read_xlsx('/share/scratch/projects/hssa/asher/variable_availability.xlsx'))
variable_list_dhs <- variable_list[DHS_sensitivity == 'Y']
variable_list_mics <- variable_list[MICS_sensitivity == 'Y']

if (endline == "DHS"){
  variables <- c(variable_list_dhs$Variable, "country", 'year','any_birth_preg_2_yr_dhs', 'cluster', 'pweight', 'rural')
} else{
  variables <- c(variable_list_mics$Variable, "country", 'year','any_birth_preg_2_yr_mics', 'cluster', 'pweight', 'rural')
}

## subset input data just to these variables
input_df <- ind_dt[,names(ind_dt)%in% variables,with=FALSE]

## label baseline so we can group data for oaxaca-blinder
input_df[, latest := max(year), by = "country"]
input_df[, baseline := ifelse(year == latest, 0,1)]

input_df <- input_df[, -c("latest")] # don't need this column anymore

## filter to just women aged 15-24
input_df_15_24 <- input_df[age %in% c(seq(15,24))] #54901 rows

## determine percent missing
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
if (endline == 'DHS'){
  write.csv(input_df_15_24_no_na, file.path(out.dir, 'ob_input_prepped_df_dhs_sensitivity.csv'), row.names=F)
} else{
  write.csv(input_df_15_24_no_na, file.path(out.dir, 'ob_input_prepped_df_mics_sensitivity.csv'), row.names=F)
}


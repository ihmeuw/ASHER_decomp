## Created by: Corinne Bintz
## Creation date: 1/23/2024
##  set up data for asher decomp lasso regressions: for by country OB
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

'%ni%' <- Negate('%in%')

pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey,tidyr,glmnet)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source)) # IHME-specific line. comment out if running outside IHME

## select input data date
in.date <- '2024-01-30'
in.dir <- file.path('/share/scratch/projects/hssa/asher/phase1/data/', in.date)

out.dir <- file.path('/share/scratch/projects/hssa/asher/phase1/data', Sys.Date())
dir.create(out.dir, recursive = T)

## set endline to either MICS (to use Malawi MICS 2019-2020) or DHS (to use Malawi DHS 2015-2016)
endline <- 'MICS'

## read in data
if (endline == "MICS"){
  ind_dt <- data.table(fread(file.path(in.dir, '/merged_baseline_endline_mics.csv')))
} else{
  ind_dt <- data.table(fread(file.path(in.dir, '/merged_baseline_endline_dhs.csv')))
}

## read in list of variable names
variable_list <- data.table(read_xlsx('/share/scratch/projects/hssa/asher/variable_availability.xlsx'))

variable_list <- variable_list[DHS == 'Y']
variables <- c(variable_list$Variable, "country", 'year', 'any_birth_preg')

## subset input data just to these variables
input_df <- ind_dt[,names(ind_dt)%in% variables,with=FALSE]

input_df[, country := str_sub(country, 0,2)]

## label baseline so we can group data for oaxaca-blinder
input_df[, latest := max(year), by = "country"]
input_df[, baseline := ifelse(year == latest, 0,1)]
input_df[, endline := ifelse(year == latest, 1,0)]

input_df <- input_df[, -c("latest")] # don't need this column anymore

## filter to just women aged 15-24
input_df_15_24 <- input_df[age %in% c(seq(15,24))] #54901 rows

## determine percent missing by country
missing_dt_by_country <- data.table()

for (cur_country in unique(input_df_15_24$country)){ ## probably want to do this by age group
  tmp_dt <- input_df_15_24[country == cur_country]
  for (version in unique(tmp_dt$endline)){
    tmp_dt_year <- tmp_dt[endline == version]
    for (var in colnames(tmp_dt_year)){
      nobs_var <- nrow(tmp_dt_year[!is.na(get(var))])
      nrows_missing <- nrow(tmp_dt_year[is.na(get(var))])
      nrows_total <- nrow(tmp_dt_year) 
      missing <- nrows_missing/nrows_total
      tmp_missing_dt <- data.table(variable = var, percent_missing = missing, nobs = nobs_var, country = cur_country, endline = version)
      missing_dt_by_country <- rbind(missing_dt_by_country, tmp_missing_dt)
    }
  }
 
}

input_df <- input_df[, -c("endline")] # don't need this column anymore

missing_dt_by_country_wide <- dcast(missing_dt_by_country[, -c( 'nobs')], variable ~ country + endline, value.var = 'percent_missing')
write.csv(missing_dt_by_country_wide, file.path(out.dir, paste0('percent_missing_variables_', tolower(endline), '.csv')), row.names=F)

## remove outcomes from data to be input data as well as data that is largely missing and will need to be re-processed
## also remove categorical education variables because we will use continuous ed variable 
input_df_15_24_filter <- input_df_15_24[, -c( "gap_sex_mar", "age_1st_birth", "age_1st_sex_imp", "mean_yrs_schooling_head", "highest_ed_level")]
nrow(input_df_15_24_filter)

## recode categorical into dummy 
for (col in colnames(input_df_15_24_filter)){
  if ( class(input_df_15_24_filter[,get(col)]) == "character"){
    print(col)
  }
}

## hv025
input_df_15_24_filter[, rural := ifelse(hv025 == "rural", 1,
                                       ifelse(hv025 == "urban", 0, NA))]

## recent sexual activity 
input_df_15_24_filter[, sex_activity_last_4_weeks := ifelse(rec_sex_activity == "Active in the last 4 weeks", 1,
                                                           ifelse(!is.na(rec_sex_activity), 0, NA))]
input_df_15_24_filter[, no_sex_activity_last_4_weeks := ifelse(rec_sex_activity == "Not active in the last 4 weeks", 1,
                                                              ifelse(!is.na(rec_sex_activity), 0, NA))]
input_df_15_24_filter[, rec_sex_activity_missing := ifelse(rec_sex_activity == "rec_sex_activity_missing", 1,
                                                          ifelse(!is.na(rec_sex_activity), 0, NA))]

## current method : split into long-acting modern, short-acting modern, other/other traditional, and no method 
input_df_15_24_filter[, long_acting_method_mod := ifelse(current_method %in% c("iud", "implants",'female_sterilization'), 1, 0)]
input_df_15_24_filter[, short_acting_method_mod := ifelse(current_method %in% c("rhythm", "condom",'injections', 'withdrawal',
                                                                               "lactational_amenorrhea_method", "emergency_contraception",
                                                                               "foam_jelly_sponge", "calendar_methods", 'pill','other_modern_method'), 1, 0)]
input_df_15_24_filter[, no_method := ifelse(current_method %in% c("none"), 1, 0)]
input_df_15_24_filter[, other_method_trad := ifelse(current_method %in% c("other", 'other_traditional_method'), 1, 0)]


## take out columns we made dummy variables and region and religion
input_df_15_24_filter <- input_df_15_24_filter[, -c("religion", "region", "current_method", "rec_sex_activity","hv025")]

for (cur_country in unique(input_df_15_24_filter$country)){ ## probably want to do this by age group
  cur_df <- input_df_15_24_filter[country == cur_country]
  ## filter out na's 
  input_df_15_24_no_na <- na.omit(cur_df) 
  ## isolate  outcomes
  births_preg <- input_df_15_24_no_na[,c("any_birth_preg", "country", "baseline", 'age')]
  ## remove outcomes from input data
  input_df_15_24_no_na <- input_df_15_24_no_na[, -c("births_3_yr", "births_5_yr", "any_birth_preg")]
  
  ## save dataframes to use in ob decomp
  if (endline == "MICS"){
    write.csv(input_df_15_24_no_na, file.path(out.dir, paste0('ob_input_prepped_df_mics_', cur_country, '.csv')), row.names=F)
    write.csv(births_preg, file.path(out.dir, paste0('outcome_prepped_df_mics_', cur_country, '.csv')), row.names=F)
  } else {
    write.csv(input_df_15_24_no_na, file.path(out.dir, paste0('ob_input_prepped_df_dhs_', cur_country, '.csv')), row.names=F)
    write.csv(births_preg, file.path(out.dir, paste0('outcome_prepped_df_dhs_', cur_country, '.csv')), row.names=F)
  }
  
  ## standardize predictors 
  predictors <- names(input_df_15_24_no_na)[names(input_df_15_24_no_na) %ni% c("country", "baseline")]
  
  ## first separate into age groups
  input_df_15_19_no_na <- copy (input_df_15_24_no_na)
  input_df_15_19_no_na <- input_df_15_19_no_na[age %in% c(seq(15,19))]
  
  input_df_15_19_no_na <- input_df_15_19_no_na %>%
    mutate_at(predictors, ~(scale(.) %>% as.vector))
  
  input_df_15_24_no_na <- input_df_15_24_no_na %>%
    mutate_at(predictors, ~(scale(.) %>% as.vector))
  
  ## save dataframes to use in lasso regression
  if (endline == "MICS"){
    write.csv(input_df_15_19_no_na, file.path(out.dir, paste0('lasso_input_prepped_df_15_19_mics_', cur_country, '.csv')), row.names=F)
    write.csv(input_df_15_24_no_na, file.path(out.dir, paste0('lasso_input_prepped_df_15_24_mics_', cur_country, '.csv')), row.names=F)
  } else{
    write.csv(input_df_15_19_no_na, file.path(out.dir, paste0('lasso_input_prepped_df_15_19_dhs_', cur_country, '.csv')), row.names=F)
    write.csv(input_df_15_24_no_na, file.path(out.dir, paste0('lasso_input_prepped_df_15_24_dhs_', cur_country, '.csv')), row.names=F)
  }

}
  





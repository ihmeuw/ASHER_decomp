## Created by: Corinne Bintz
## Creation date: 1/23/2024
##  set up data for asher decomp lasso regressions: for pooled OB
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
in.date <- '2024-02-13'
in.dir <- file.path('/share/scratch/projects/hssa/asher/phase1/data/', in.date)

out.dir <- file.path('/share/scratch/projects/hssa/asher/phase1/data', Sys.Date())
dir.create(out.dir, recursive = T)

## set endline to either MICS (to use Malawi MICS 2019-2020) or DHS (to use Malawi DHS 2015-2016)
endline <- 'DHS'

## read in data
if (endline == "MICS"){
  ind_dt <- data.table(fread(file.path(in.dir, '/merged_baseline_endline_mics.csv')))
} else{
  ind_dt <- data.table(fread(file.path(in.dir, '/merged_baseline_endline_dhs.csv')))
}


## read in list of variable names
variable_list <- data.table(read_xlsx('/share/scratch/projects/hssa/asher/variable_availability.xlsx'))
if (endline == "MICS"){
  variable_list <- variable_list[DHS_phase1 == 'Y' & MICS_phase1 == 'Y']
} else{
  variable_list <- variable_list[DHS_phase1 == 'Y']
}
variables <- c(variable_list$Variable, "country", 'year', 'any_birth_preg', 'strata_unique', 'psu_unique', 'pweight')

## subset input data just to these variables
input_df <- ind_dt[,names(ind_dt)%in% variables,with=FALSE]

input_df[, country := str_sub(country, 0,2)]

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

## remove outcomes from data to be input data as well as data that is largely missing and will need to be re-processed
## also remove categorical education variables because we will use continuous ed variable 
input_df_15_24_filter <- input_df_15_24[, -c( "mean_yrs_schooling_head", "highest_ed_level", "sex_partners")]
nrow(input_df_15_24_filter)

## filter out na's 
input_df_15_24_no_na <- na.omit(input_df_15_24_filter) # 49937 rows
nrow(input_df_15_24_no_na)

## isolate  outcomes
births_preg <- input_df_15_24_no_na[,c("any_birth_preg", "country", "baseline", 'age')]

## remove unnecssary outcomes from input data
#input_df_15_24_no_na <- input_df_15_24_no_na[, -c("births_3_yr", "births_5_yr", "any_birth_preg")]
input_df_15_24_no_na <- input_df_15_24_no_na[, -c("births_3_yr", "births_5_yr")]

## recode categorical into dummy 
for (col in colnames(input_df_15_24_no_na)){
  if ( class(input_df_15_24_no_na[,get(col)]) == "character"){
    print(col)
  }
}

## hv025
input_df_15_24_no_na[, rural := ifelse(hv025 == "rural", 1,
                                       ifelse(hv025 == "urban", 0, NA))]

## recent sexual activity 
input_df_15_24_no_na[, sex_activity_last_4_weeks := ifelse(rec_sex_activity == "Active in the last 4 weeks", 1,
                                                           ifelse(!is.na(rec_sex_activity), 0, NA))]
input_df_15_24_no_na[, no_sex_activity_last_4_weeks := ifelse(rec_sex_activity == "Not active in the last 4 weeks", 1,
                                                              ifelse(!is.na(rec_sex_activity), 0, NA))]

# current method : split into long-acting modern, short-acting modern, other/other traditional, and no method: for lasso regression
input_df_15_24_no_na[, long_acting_method_mod := ifelse(current_method %in% c("iud", "implants",'female_sterilization','male_sterilization'), 1, 0)]
input_df_15_24_no_na[, short_acting_method_mod := ifelse(current_method %in% c("rhythm", "condom",'injections', 
                                                                           "lactational_amenorrhea_method", "emergency_contraception",
                                                                           "foam_jelly_sponge", "calendar_methods", 'pill','other_modern_method','diaphragm'), 1, 0)]
input_df_15_24_no_na[, no_method := ifelse(current_method %in% c("none"), 1, 0)]
input_df_15_24_no_na[, other_method_trad := ifelse(current_method %in% c("other", 'withdrawal','other_traditional_method'), 1, 0)]

## for stata, leave as categorical 
input_df_15_24_no_na[, current_method_catg := ifelse(current_method %in% c("iud", "implants",'female_sterilization', 'male_sterilization'), 'long_acting_method_mod', 
                                                     ifelse(current_method %in% c("rhythm", "condom",'injections', 'withdrawal',
                                                                                  "lactational_amenorrhea_method", "emergency_contraception",
                                                                                  "foam_jelly_sponge", "calendar_methods", 'pill','other_modern_method','diaphragm'), 'short_acting_method_mod',
                                                            ifelse(current_method %in% c("none"), 'no_method',
                                                                   ifelse(current_method %in% c("other", 'other_traditional_method'), 'other_method_trad', 0))))]

## take out columns we made dummy or special categorical variables and region and religion
input_df_15_24_no_na <- input_df_15_24_no_na[, -c("religion", "region", "rec_sex_activity","hv025", "current_method")]

## move strata that only have one PSU to a new strata: per country
lonely_strata <- input_df_15_24_no_na[, .(count_psu = uniqueN(psu_unique)), by = c("country", "year", "strata_unique")]
lonely_strata[, strata_unique_reassign := ifelse(count_psu ==1, paste(str_sub(strata_unique, 0,8), 'lonely'), strata_unique)]

## merge new strata onto data frame
input_df_15_24_no_na <- merge(input_df_15_24_no_na, lonely_strata[, -c("count_psu")], by = c("country", "year", "strata_unique"))

## remove old strata column
input_df_15_24_no_na <- input_df_15_24_no_na[, -c("strata_unique")]

## rename reassigned strata
setnames(input_df_15_24_no_na, "strata_unique_reassign", "strata_unique")

## save dataframes to use in ob decomp

if (endline == "MICS"){
  write.csv(input_df_15_24_no_na, file.path(out.dir, 'ob_input_prepped_df_mics.csv'), row.names=F)
  write.csv(births_preg, file.path(out.dir, 'outcome_prepped_df_mics.csv'), row.names=F)
} else {
  write.csv(input_df_15_24_no_na, file.path(out.dir, 'ob_input_prepped_df_dhs.csv'), row.names=F)
  write.csv(births_preg, file.path(out.dir, 'outcome_prepped_df_dhs.csv'), row.names=F)
}

## standardize predictors 
predictors <- names(input_df_15_24_no_na)[names(input_df_15_24_no_na) %ni% c("country", "baseline", 'pweight', 'strata_unique', 'psu_unique', 'any_birth_preg', 'current_method_catg')]

## first separate into age groups
input_df_15_24_no_na <- input_df_15_24_no_na[, -c("pweight", "psu_unique", "strata_unique", "any_birth_preg", "current_method_catg")]
input_df_15_19_no_na <- copy (input_df_15_24_no_na)
input_df_15_19_no_na <- input_df_15_19_no_na[age %in% c(seq(15,19))]

input_df_15_19_no_na <- input_df_15_19_no_na %>%
  mutate_at(predictors, ~(scale(.) %>% as.vector))

input_df_15_24_no_na <- input_df_15_24_no_na %>%
  mutate_at(predictors, ~(scale(.) %>% as.vector))

## save dataframes to use in lasso regression
if (endline == "MICS"){
  write.csv(input_df_15_19_no_na, file.path(out.dir, 'lasso_input_prepped_df_15_19_mics.csv'), row.names=F)
  write.csv(input_df_15_24_no_na, file.path(out.dir, 'lasso_input_prepped_df_15_24_mics.csv'), row.names=F)
} else{
  write.csv(input_df_15_19_no_na, file.path(out.dir, 'lasso_input_prepped_df_15_19_dhs.csv'), row.names=F)
  write.csv(input_df_15_24_no_na, file.path(out.dir, 'lasso_input_prepped_df_15_24_dhs.csv'), row.names=F)
}


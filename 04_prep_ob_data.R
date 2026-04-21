## HEADER --------
## Created by: NAME 
## Set up data for ASHER Oaxaca-Blinder analysis 

# SET-UP --------------------------------------------------------------------


# create %ni%
'%ni%' <- Negate('%in%')

# load packages
pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey,tidyr,glmnet)

# load shared functions
invisible(sapply(list.files("FILEPATH", full.names = T), source)) # IHME-specific line. comment out if running outside IHME

# create function to convert CMC date to year and month
get_cmc_year <- function(x) 1900 + as.integer((as.integer(x) - 1) / 12)
get_cmc_month <- function(x) as.integer(x) - 12 * (get_cmc_year(x) - 1900)

# directories
#out.dir <- file.path('FILEPATH', Sys.Date())
out.dir <- file.path('FILEPATH', "2025-10-08")

dir.create(out.dir, recursive = T)

in.dir <- 'FILEPATH'
files <- list.files(in.dir)
# we don't want to include india
files <- files[!grepl("in_", files)]
in.dir_cal <- 'FILEPATH'

# PREP DATA FUNCTION ---------------------------------------------------------
endline <- "MICS"
prep_data <- function(endline){
  
  ## LOAD DATA --------
  if (endline == "DHS"){
    ind_dt <-rbindlist(lapply(file.path(in.dir, files[!grepl("mics",files)]), fread), fill = TRUE)
  } else{
    files <- files[!grepl("mw", files)]
    ind_dt <-rbindlist(lapply(file.path(in.dir, files), fread), fill = TRUE)
    
    ## read in malawi data
    files <- list.files(in.dir)
    files <- files[grepl("mw_baseline_endline_mics.csv", files)]
    mw_dt <-rbindlist(lapply(file.path(in.dir, files), fread), fill = TRUE)
    mw_dt[, wealth_quintiles := ifelse(is.na(wealth_quintiles), windex5, wealth_quintiles)]
    mw_dt <- mw_dt[, -c("windex5")]
    
    ## add on malawi
    # fix attend_school for MICS
    # only asked to individuals 3-24 who have ever attended school
    ind_dt <- rbind(ind_dt, mw_dt, fill = T)
    if ("ever_attend_school" %in% names(ind_dt)) ind_dt[ever_attend_school == 0, attend_school := 0]
    
  }
  ## rename wealth quintiles to wealth index
  setnames(ind_dt, "wealth_quintiles", "wealth_index")
  
  # identify if a woman had a termination in the last 2 years
  ind_dt[, term_2_years := ifelse(term_recall<24, 1, 0)] # termination within 2 years of interview
  ind_dt[is.na(term_2_years), term_2_years := 0]
  
  # PREP CALENDAR DATA --------
  
  # load calendar data for endline surveys
  surveys <- unique(ind_dt$survey)
  cal_files <- list.files(in.dir_cal)
  cal_files <- cal_files[grepl("calendar", cal_files) & !grepl("IND", cal_files)] # exclude india 
  cal_files <- cal_files[grepl(paste(surveys, collapse = "|"), cal_files)]
  cal_dt <- rbindlist(lapply(file.path(in.dir_cal, cal_files), fread), fill = TRUE)

  # merge on columns from input data needed for calendar prep
  cal_dt <- merge(cal_dt, ind_dt[, c("id_unique", "survey", "curr_preg", "cmc_first_child", "any_births_2_yr", "number_births", 
                                     "cmc_preg_term", "ever_term", "term_2_years")], 
                  by = c("id_unique", "survey"), all.x = T)
  
  # determine cmc date of first termination
  cal_dt[ever_term == 1, cmc_first_term := min(month_cmc[event_recode == "termination"], na.rm = T), by = c("id_unique", "survey")]
  cal_dt[any_term_before_cal == 1, cmc_first_term := calendar_starts - 10]   # this number is fake, just ensuring it does not fall within our outcome time frames
  
  # identify women with any birth/pregnancy history
  cal_dt[, ever_preg := max(birth_preg, na.rm = T), by = c("id_unique", "survey")]
  
  # identify if women had a pregnancy in last 2 years 
  # even if a woman started her pregnancy more than 24 months ago, and it ended within the last 24 months, we want to count it
  cal_dt[, any_preg_2_yr := ifelse(curr_preg == 1 | any_births_2_yr == 1 | term_2_years == 1, 1, 0), by = c("id_unique", "survey")]

  # assign each grouping of consecutive months a woman is pregnant an id
  cal_dt[, birth_preg_id := rleid(id_unique, survey, birth_preg)]
  
  # identify most recent preg id
  cal_dt[ever_preg == 1, most_recent_birth_preg_id := min(birth_preg_id[birth_preg == 1], na.rm = T), by = c("id_unique", "survey")]

  # get first month of most recent pregnancy
  cal_dt[ever_preg == 1, cmc_most_recent_preg_start := min(month_cmc[birth_preg_id == most_recent_birth_preg_id], na.rm = T), by = c("id_unique", "survey")]

  # get first month of a woman's first pregnancy
  
  ## women currently pregnant, with 0 terms and 0 births
  cal_dt[curr_preg == 1 & ever_term == 0 & number_births == 0, cmc_first_preg_start := min(month_cmc[birth_preg == 1]), by = c("id_unique", "survey")]
  ## women with 0 terms, first birth in last 2 years
  cal_dt[ever_term == 0 & cmc_first_child > cmc_interview_date - 24, cmc_first_preg_start := min(month_cmc[birth_preg == 1]), by = c("id_unique", "survey")]
  ## women with 0 births, first term in last 2 years
  cal_dt[ever_term == 1 & number_births == 0 & (cmc_first_term > cmc_interview_date - 24), cmc_first_preg_start := min(month_cmc[birth_preg == 1]), by = c("id_unique", "survey")]
  ## women with first birth and first term in last 2 years
  cal_dt[(cmc_first_term > cmc_interview_date - 24) & (cmc_first_child > cmc_interview_date - 24), cmc_first_preg_start := min(month_cmc[birth_preg == 1]), by = c("id_unique", "survey")]
  
  # calculate if ever used contraception (restrict to before most recent or first pregnancy, if applicable)
  
  # for outcome A, restrict to before most recent pregnancy in last 2 years
  cal_dt[any_preg_2_yr == 1, mod_contra_outcome_a := max(mod_contra[month_cmc < cmc_most_recent_preg_start], na.rm = T), by = c("id_unique", "survey")]
  cal_dt[curr_preg == 0 & number_births == 0 & ever_term == 0, mod_contra_outcome_a := max(mod_contra, na.rm = T), by = c("id_unique", "survey")]
  
  cal_dt[any_preg_2_yr == 1, methods_outcome_a := paste0(unique(event_recode[month_cmc < cmc_most_recent_preg_start & any_contra == 1]), collapse = ", "), by = c("id_unique", "survey")]
  cal_dt[curr_preg == 0 & number_births == 0 & ever_term == 0, methods_outcome_a := paste0(unique(event_recode[any_contra == 1]), collapse = ", "), by = c("id_unique", "survey")]
  
  # for outcome B, restrict to before first pregnancy in last 2 years
  cal_dt[(curr_preg == 1 & ever_term == 0 & number_births == 0) | (cmc_first_child > cmc_interview_date - 24) | (cmc_first_term > cmc_interview_date - 24), mod_contra_outcome_b := max(mod_contra[month_cmc < cmc_first_preg_start], na.rm = T), by = c("id_unique", "survey")]
  cal_dt[curr_preg == 0 & number_births == 0 & ever_term == 0, mod_contra_outcome_b := max(mod_contra, na.rm = T), by = c("id_unique", "survey")]
  
  cal_dt[(curr_preg == 1 & ever_term == 0 & number_births == 0) | (cmc_first_child > cmc_interview_date - 24) | (cmc_first_term > cmc_interview_date - 24), methods_outcome_b := paste0(unique(event_recode[month_cmc < cmc_first_preg_start & any_contra == 1]), collapse = ", "), by = c("id_unique", "survey")]
  cal_dt[curr_preg == 0 & number_births == 0 & ever_term == 0, methods_outcome_b := paste0(unique(event_recode[any_contra == 1]), collapse = ", "), by = c("id_unique", "survey")]
  
  # for outcome C, restrict to before most recent pregnancy in last 2 years (same as for outcome a)
  cal_dt[any_preg_2_yr == 1, mod_contra_outcome_c := max(mod_contra[month_cmc < cmc_most_recent_preg_start], na.rm = T), by = c("id_unique", "survey")]
  cal_dt[curr_preg == 0 & number_births == 0 & ever_term == 0, mod_contra_outcome_c := max(mod_contra, na.rm = T), by = c("id_unique", "survey")]
  
  cal_dt[any_preg_2_yr == 1, methods_outcome_c := paste0(unique(event_recode[month_cmc < cmc_most_recent_preg_start & any_contra == 1]), collapse = ", "), by = c("id_unique", "survey")]
  cal_dt[curr_preg == 0 & number_births == 0 & ever_term == 0, methods_outcome_c := paste0(unique(event_recode[any_contra == 1]), collapse = ", "), by = c("id_unique", "survey")]
  
  # keep unique rows per woman, drop unnecessary calendar columns
  cal_dt_unique <- unique(cal_dt[, .(id_unique, survey, cmc_interview_date, any_preg_2_yr, any_term_before_cal, cmc_first_term, cmc_first_preg_start, cmc_most_recent_preg_start, 
                                     mod_contra_outcome_a, methods_outcome_a, mod_contra_outcome_b, methods_outcome_b, mod_contra_outcome_c, methods_outcome_c)])
  
  # merge onto data  
  ind_dt <- merge(ind_dt, cal_dt_unique, by = c("id_unique", "survey", "cmc_interview_date"), all.x = T)

  ## CREATE OUTCOMES ----
  
  ## construct outcome to use with dhs endline: in the last 2 years or currently preg, include terminations 
  ind_dt[any_births_2_yr == 1 | curr_preg == 1 | term_2_years == 1, any_birth_preg_2_yr_dhs := 1]
  ind_dt[number_births == 0 & curr_preg == 0 & ever_term == 0, any_birth_preg_2_yr_dhs := 0]
  
  ## construct outcome to use with mics endline: in the last 2 years or currently preg, exclude terminations 
  ind_dt[any_births_2_yr == 1 | curr_preg == 1, any_birth_preg_2_yr_mics := 1]
  ind_dt[number_births == 0 & curr_preg == 0, any_birth_preg_2_yr_mics := 0]
  
  ## CONSTRUCT NEW OUTCOMES --------
  
  ## NEW outcomes, per 9/27/24 discussion with NAME
  
  ## outcome A: (original formulation)
  ## 1 = any pregnancy in last 2 years, or currently pregnant, including terminations (for DHS)
  ## 0 = never pregnant 
  
  ## outcome B:
  ## 1 = first pregnancy in last 2 years, or currently pregnant, including terminations (for DHS)
  ## 0 = never pregnant 
  
  ## outcome C:
  ## 1 = any pregnancy in last 2 years, or currently pregnant, including terminations (for DHS)
  ## 0 = no pregnancy in last 2 years
  
  ## construct outcome A
  ind_dt[, outcome_a_dhs := any_birth_preg_2_yr_dhs]
  ind_dt[, outcome_a_mics := any_birth_preg_2_yr_mics]
  
  ## construct outcome B
  ind_dt[(curr_preg == 1 & ever_term == 0 & ever_birth == 0) | (ever_term == 0 & (cmc_first_child > cmc_interview_date - 24)) | 
           (ever_birth == 0 & (cmc_first_term > cmc_interview_date - 24)) | (cmc_first_term > cmc_interview_date - 24) & (cmc_first_child > cmc_interview_date - 24), outcome_b_dhs := 1]
  ind_dt[ever_preg == 0, outcome_b_dhs := 0]
  
  ind_dt[(curr_preg == 1 & ever_birth == 0) | (cmc_first_child > cmc_interview_date - 24), outcome_b_mics := 1]   # excludes terminations
  ind_dt[number_births == 0 & curr_preg == 0, outcome_b_mics := 0]  
  
  # construct outcome C
  ind_dt[any_births_2_yr == 1 | curr_preg == 1 | term_2_years == 1, outcome_c_dhs := 1]
  ind_dt[any_births_2_yr == 0 & curr_preg == 0 & term_2_years == 0, outcome_c_dhs := 0]
  
  ind_dt[any_births_2_yr == 1 | curr_preg == 1, outcome_c_mics := 1]   # excludes terminations
  ind_dt[any_births_2_yr == 0 & curr_preg == 0, outcome_c_mics := 0]  
  
  # construct indicator for if married after pregnancy 
  ind_dt[, married_after_preg := ifelse(cmc_first_cohabit > cmc_first_child, 1, 0)]
  # if no births or never married, married_after_preg should be 0
  ind_dt[number_births == 0 | former_cohabit== 0, married_after_preg := 0]
  
  # CLEAN DATA --------
  
  ## adjust years
  ind_dt[country == "np", year := get_cmc_year(cmc_interview_date - ((56*12)+8))]
  ind_dt[, year := min(year), by = "survey"]
  
  ## read in list of variable names
  variable_list <- data.table(read_xlsx('FILEPATH'))
  variable_list <- variable_list[DHS_phase1 == 'Y']
  if (endline == "DHS"){
    variables <- c(variable_list$Variable, "country", 'year', 'cluster', 'pweight', 'survey', 'id_unique', 'cmc_interview_date',
                   'curr_preg', 'number_births', 'any_births_2_yr', 'term_2_years', 'ever_birth', 'ever_term', 'cmc_first_child', 'cmc_first_term',
                   'any_birth_preg_2_yr_dhs', 'outcome_a_dhs', 'outcome_b_dhs', 'outcome_c_dhs', 
                   'any_contra', 'mod_contra', 'trad_contra', 'current_method',
                   'mod_contra_outcome_a', 'methods_outcome_a', 'mod_contra_outcome_b', 'methods_outcome_b', 'mod_contra_outcome_c', 'methods_outcome_c',
                   'mean_yrs_schooling_head',"attend_school", "urban", 'married_after_preg')
  } else{
    variables <- c(variable_list$Variable, "country", 'year','any_birth_preg_2_yr_mics', 'cluster', 'pweight', 'survey','id_unique', 
                   'any_contra', 'mod_contra', 'trad_contra', 'current_method', 'outcome_a_mics', 'outcome_b_mics', 'outcome_c_mics',
                   'mod_contra_outcome_a', 'methods_outcome_a', 'mod_contra_outcome_b', 'methods_outcome_b', 'mod_contra_outcome_c', 'methods_outcome_c',
                   'mean_yrs_schooling_head',"attend_school", "urban", 'married_after_preg')
  }
  
  ## subset input data just to these variables
  input_df <- ind_dt[,names(ind_dt)%in% variables,with=FALSE]
  
  ## label baseline so we can group data for oaxaca-blinder
  input_df[, latest := max(year), by = "country"]
  input_df[, baseline := ifelse(year == latest, 0,1)]
  input_df[, endline := ifelse(year == latest, 1,0)]
  
  input_df <- input_df[, -c("latest")] # don't need this column anymore
  
  ## filter to just women aged 15-24
  input_df_15_24 <- input_df[age %in% c(seq(15,24))] #54901 rows
  
  ## subpop: had sex
  # age_1st_sex_imp
  input_df_15_24[had_intercourse == 0 & is.na(age_1st_sex_imp), age_1st_sex_imp := 0]
  
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
  
  ## make wealth index categorical as it is specified in ob
  input_df_15_24[, wealth_dummies1 := ifelse(wealth_index ==1, 1, 0)]
  input_df_15_24[, wealth_dummies2 := ifelse(wealth_index ==2, 1, 0)]
  input_df_15_24[, wealth_dummies3 := ifelse(wealth_index ==3, 1, 0)]
  input_df_15_24[, wealth_dummies4 := ifelse(wealth_index ==4, 1, 0)]
  input_df_15_24[, wealth_dummies5 := ifelse(wealth_index ==5, 1, 0)]
  
  # unique psu for each nid
  input_df_15_24[, psu_unique := paste(country, year, cluster)]
  
  # re code had_intercourse to only capture sexual activity among unmarried women 
  # if a woman is married, we set had_intercourse_unmarried to 0
  # if a woman is unmarried, it remains how it was
  input_df_15_24[curr_cohabit == 1 & !is.na(had_intercourse), had_intercourse_unmarried := 0]
  input_df_15_24[curr_cohabit == 0 & !is.na(had_intercourse), had_intercourse_unmarried := had_intercourse]

  
  ## EXPORT DATA ---------
  # saving versions with endline only for regression and versions with both for OB 
  if (endline == "DHS"){
    # save outcome A data
    outcome_a_input_df <- input_df_15_24[, .(baseline,endline,cluster,country,year,pweight,age,curr_cohabit,age_1st_sex_imp,beating_just,educ_single_yrs,
                                             had_intercourse,had_intercourse_unmarried,wealth_index,wealth_dummies1,wealth_dummies2,wealth_dummies3,wealth_dummies4,wealth_dummies5,
                                             outcome_a_dhs,mod_contra_outcome_a,psu_unique, unmet_need)]
    outcome_a_input_df <- na.omit(outcome_a_input_df)
    
    write.csv(outcome_a_input_df, file.path(out.dir, paste0('ob_input_prepped_df_outcome_a_', tolower(endline), '.csv')), row.names=F)
    
    outcome_a_input_df_ob <- input_df_15_24[, .(baseline,endline,cluster,country,year,pweight,age,curr_cohabit,age_1st_sex_imp,beating_just,educ_single_yrs,
                                                     had_intercourse,had_intercourse_unmarried,wealth_index,wealth_dummies1,wealth_dummies2,wealth_dummies3,wealth_dummies4,wealth_dummies5,
                                                     outcome_a_dhs,psu_unique, unmet_need)]
    outcome_a_input_df_ob <- na.omit(outcome_a_input_df_ob)
    
    write.csv(outcome_a_input_df_ob, file.path(out.dir, paste0('ob_input_prepped_df_outcome_a_ob_', tolower(endline), '.csv')), row.names=F)
    
    
    # save outcome B data
    outcome_b_input_df <- input_df_15_24[, .(baseline,endline,cluster,country,year,pweight,age,curr_cohabit,age_1st_sex_imp,beating_just,educ_single_yrs,
                                             had_intercourse,had_intercourse_unmarried,wealth_index,wealth_dummies1,wealth_dummies2,wealth_dummies3,wealth_dummies4,wealth_dummies5,
                                             outcome_b_dhs,mod_contra_outcome_b,psu_unique, unmet_need)]
    outcome_b_input_df <- na.omit(outcome_b_input_df)
    
    write.csv(outcome_b_input_df, file.path(out.dir, paste0('ob_input_prepped_df_outcome_b_', tolower(endline), '.csv')), row.names=F)
    
    outcome_b_input_df_ob <- input_df_15_24[, .(baseline,endline,cluster,country,year,pweight,age,curr_cohabit,age_1st_sex_imp,beating_just,educ_single_yrs,
                                                     had_intercourse,had_intercourse_unmarried,wealth_index,wealth_dummies1,wealth_dummies2,wealth_dummies3,wealth_dummies4,wealth_dummies5,
                                                     outcome_b_dhs,psu_unique, unmet_need)]
    outcome_b_input_df_ob <- na.omit(outcome_b_input_df_ob)
    
    write.csv(outcome_b_input_df_ob, file.path(out.dir, paste0('ob_input_prepped_df_outcome_b_ob_', tolower(endline), '.csv')), row.names=F)
    
    # save outcome C data
    outcome_c_input_df <- input_df_15_24[, .(baseline,endline,cluster,country,year,pweight,age,curr_cohabit,age_1st_sex_imp,beating_just,educ_single_yrs,
                                             had_intercourse,had_intercourse_unmarried,wealth_index,wealth_dummies1,wealth_dummies2,wealth_dummies3,wealth_dummies4,wealth_dummies5,
                                             outcome_c_dhs,mod_contra_outcome_c,psu_unique, unmet_need)]
    outcome_c_input_df <- na.omit(outcome_c_input_df)
    
    write.csv(outcome_c_input_df, file.path(out.dir, paste0('ob_input_prepped_df_outcome_c_', tolower(endline), '.csv')), row.names=F)
    
    outcome_c_input_df_ob <- input_df_15_24[, .(baseline,endline,cluster,country,year,pweight,age,curr_cohabit,age_1st_sex_imp,beating_just,educ_single_yrs,
                                                     had_intercourse,had_intercourse_unmarried,wealth_index,wealth_dummies1,wealth_dummies2,wealth_dummies3,wealth_dummies4,wealth_dummies5,
                                                     outcome_c_dhs,psu_unique, unmet_need)]
    outcome_c_input_df_ob <- na.omit(outcome_c_input_df_ob)
    
    write.csv(outcome_c_input_df_ob, file.path(out.dir, paste0('ob_input_prepped_df_outcome_c_ob_', tolower(endline), '.csv')), row.names=F)
    
    # save sensitivity for RTR: test mean years schooling hh head, fp media exposure, and urbanicity 
    full_sensitivity_outcome_c_input_df <- input_df_15_24[, .(baseline,endline,cluster,country,year,pweight,age,curr_cohabit,age_1st_sex_imp,beating_just,educ_single_yrs,
                                                         had_intercourse,had_intercourse_unmarried,wealth_index,wealth_dummies1,wealth_dummies2,wealth_dummies3,wealth_dummies4,wealth_dummies5,
                                                         outcome_c_dhs,psu_unique, unmet_need, urban, mean_yrs_schooling_head)]
    full_sensitivity_outcome_c_input_df <- na.omit(full_sensitivity_outcome_c_input_df)
    write.csv(full_sensitivity_outcome_c_input_df, file.path(out.dir, paste0('ob_input_prepped_df_outcome_c_ob_full_sensitivity_', tolower(endline), '.csv')), row.names=F)
    
    # save sensitivity for RTR: dropping women who married after pregnancy
    marriage_preg_drop <- input_df_15_24[married_after_preg==0, .(baseline,endline,cluster,country,year,pweight,age,curr_cohabit,age_1st_sex_imp,beating_just,educ_single_yrs,
                                                              had_intercourse,had_intercourse_unmarried,wealth_index,wealth_dummies1,wealth_dummies2,wealth_dummies3,wealth_dummies4,wealth_dummies5,
                                                              outcome_c_dhs,psu_unique, unmet_need)]
    marriage_preg_drop <- na.omit(marriage_preg_drop)
    write.csv(marriage_preg_drop, file.path(out.dir, paste0('ob_input_prepped_df_outcome_c_marriage_preg_drop_', tolower(endline), '.csv')), row.names=F)
    
  } else{ # MICS
    # save outcome A data
    outcome_a_input_df <- input_df_15_24[, .(baseline,endline,cluster,country,year,pweight,age,curr_cohabit,age_1st_sex_imp,beating_just,educ_single_yrs,
                                             had_intercourse,had_intercourse_unmarried,wealth_index,wealth_dummies1,wealth_dummies2,wealth_dummies3,wealth_dummies4,wealth_dummies5,
                                             outcome_a_mics,mod_contra_outcome_a,psu_unique, unmet_need)]
    outcome_a_input_df <- na.omit(outcome_a_input_df)
    
    write.csv(outcome_a_input_df, file.path(out.dir, paste0('ob_input_prepped_df_outcome_a_', tolower(endline), '.csv')), row.names=F)
    
    outcome_a_input_df_ob <- input_df_15_24[, .(baseline,endline,cluster,country,year,pweight,age,curr_cohabit,age_1st_sex_imp,beating_just,educ_single_yrs,
                                             had_intercourse,had_intercourse_unmarried,wealth_index,wealth_dummies1,wealth_dummies2,wealth_dummies3,wealth_dummies4,wealth_dummies5,
                                             outcome_a_mics,psu_unique, unmet_need)]
    outcome_a_input_df_ob <- na.omit(outcome_a_input_df_ob)
    
    write.csv(outcome_a_input_df_ob, file.path(out.dir, paste0('ob_input_prepped_df_outcome_a_ob_', tolower(endline), '.csv')), row.names=F)
    
    
    # save outcome B data
    outcome_b_input_df <- input_df_15_24[, .(baseline,endline,cluster,country,year,pweight,age,curr_cohabit,age_1st_sex_imp,beating_just,educ_single_yrs,
                                             had_intercourse,had_intercourse_unmarried,wealth_index,wealth_dummies1,wealth_dummies2,wealth_dummies3,wealth_dummies4,wealth_dummies5,
                                             outcome_b_mics,mod_contra_outcome_b,psu_unique, unmet_need)]
    outcome_b_input_df <- na.omit(outcome_b_input_df)
    
    write.csv(outcome_b_input_df, file.path(out.dir, paste0('ob_input_prepped_df_outcome_b_', tolower(endline), '.csv')), row.names=F)
    
    outcome_b_input_df_ob <- input_df_15_24[, .(baseline,endline,cluster,country,year,pweight,age,curr_cohabit,age_1st_sex_imp,beating_just,educ_single_yrs,
                                             had_intercourse,had_intercourse_unmarried,wealth_index,wealth_dummies1,wealth_dummies2,wealth_dummies3,wealth_dummies4,wealth_dummies5,
                                             outcome_b_mics,psu_unique, unmet_need)]
    outcome_b_input_df_ob <- na.omit(outcome_b_input_df_ob)
    
    write.csv(outcome_b_input_df_ob, file.path(out.dir, paste0('ob_input_prepped_df_outcome_b_ob_', tolower(endline), '.csv')), row.names=F)
    
    
    # save outcome C data
    outcome_c_input_df <- input_df_15_24[, .(baseline,endline,cluster,country,year,pweight,age,curr_cohabit,age_1st_sex_imp,beating_just,educ_single_yrs,
                                             had_intercourse,had_intercourse_unmarried,wealth_index,wealth_dummies1,wealth_dummies2,wealth_dummies3,wealth_dummies4,wealth_dummies5,
                                             outcome_c_mics,mod_contra_outcome_c,psu_unique, unmet_need)]
    outcome_c_input_df <- na.omit(outcome_c_input_df)
    
    write.csv(outcome_c_input_df, file.path(out.dir, paste0('ob_input_prepped_df_outcome_c_', tolower(endline), '.csv')), row.names=F)
    
    outcome_c_input_df_ob <- input_df_15_24[, .(baseline,endline,cluster,country,year,pweight,age,curr_cohabit,age_1st_sex_imp,beating_just,educ_single_yrs,
                                             had_intercourse,had_intercourse_unmarried,wealth_index,wealth_dummies1,wealth_dummies2,wealth_dummies3,wealth_dummies4,wealth_dummies5,
                                             outcome_c_mics,psu_unique, unmet_need)]
    outcome_c_input_df_ob <- na.omit(outcome_c_input_df_ob)
    
    write.csv(outcome_c_input_df_ob, file.path(out.dir, paste0('ob_input_prepped_df_outcome_c_ob_', tolower(endline), '.csv')), row.names=F)
    
    # save sensitivity for RTR: test mean years schooling hh head, fp media exposure, and urbanicity 
    full_sensitivity_outcome_c_input_df <- input_df_15_24[, .(baseline,endline,cluster,country,year,pweight,age,curr_cohabit,age_1st_sex_imp,beating_just,educ_single_yrs,
                                                              had_intercourse,had_intercourse_unmarried,wealth_index,wealth_dummies1,wealth_dummies2,wealth_dummies3,wealth_dummies4,wealth_dummies5,
                                                              outcome_c_mics,psu_unique, unmet_need, urban, mean_yrs_schooling_head)]
    full_sensitivity_outcome_c_input_df <- na.omit(full_sensitivity_outcome_c_input_df)
    write.csv(full_sensitivity_outcome_c_input_df, file.path(out.dir, paste0('ob_input_prepped_df_outcome_c_ob_full_sensitivity_', tolower(endline), '.csv')), row.names=F)
    
    # save sensitivity for RTR: dropping women who married after pregnancy
    marriage_preg_drop <- input_df_15_24[married_after_preg==0, .(baseline,endline,cluster,country,year,pweight,age,curr_cohabit,age_1st_sex_imp,beating_just,educ_single_yrs,
                                                                  had_intercourse,had_intercourse_unmarried,wealth_index,wealth_dummies1,wealth_dummies2,wealth_dummies3,wealth_dummies4,wealth_dummies5,
                                                                  outcome_c_mics,psu_unique, unmet_need)]
    marriage_preg_drop <- na.omit(marriage_preg_drop)
    write.csv(marriage_preg_drop, file.path(out.dir, paste0('ob_input_prepped_df_outcome_c_marriage_preg_drop_', tolower(endline), '.csv')), row.names=F)
    
  }
  
  # save full data: note: has na's, in ob will need to choose data frame based on outcome so na's are removed appropriately 
  if (endline == "DHS") input_df_15_24 <- input_df_15_24[, -c("outcome_c_mics")] # wasn't included in main results and not sure if including will mess up sample size somewhere else 
  write.csv(input_df_15_24, file.path(out.dir, paste0('ob_input_prepped_df_', tolower(endline), '.csv')), row.names=F)
}

# RUN FUNCTION ---------------------------------------------------------------

# prep data with only DHS endline surveys: need DHS endline for contra use
prep_data("DHS")

# prep data allowing for MICS endline surveys: need to run thsi to prep data for malawi, but excludes terminations. so run this, and run dhs outcome. 
prep_data("MICS")

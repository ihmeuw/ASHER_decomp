#-------------------Header------------------------------------------------
# Project: IHME ASHER Decomposition
# Purpose: Merge and prep data for cohort survival model
# Date: 2/27/2024
# Notes:
#***************************************************************************

# SET-UP --------------------------------------------------------------------

# clear memory
rm(list=ls())

# Username is pulled automatically
username <- Sys.getenv("USER") 

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

# load packages, install if missing
pacman::p_load(data.table,tidyverse,dplyr,foreign,haven,zoo)

# in/out
in.dir <- "FILEPATH"
out.dir <- "FILEPATH"

# create function for the opposite of %in%
'%ni%' <- Negate('%in%')

# create function to convert CMC date to year and month
get_cmc_year <- function(x) 1900 + as.integer((as.integer(x) - 1) / 12)
get_cmc_month <- function(x) as.integer(x) - 12 * (get_cmc_year(x) - 1900)


# PREP DATA -------------------------------------------------------

# countries to merge/prep files for
countries <- c("mw", "np", "rw", "gh")

# loop through each country to merge
for (cur_country in countries) {
  
  print(cur_country)
  
  # list csv files in processed directory
  files <- list.files(in.dir, pattern = ".csv")
  
  # subset to files for country of interest
  files <- files[grepl(cur_country, tolower(files))]
  
  # subset to DHS files
  files <- files[grepl("DHS", files)]
  
  # list of surveys with calendar info
  surveys <- unique(str_extract(files[grepl("calendar", files)], "[A-Z]{3}_DHS[0-9]_[0-9]{4}(_[0-9]{4})*"))
  
  # loop through each survey to merge
  for (survey in surveys) {
    
    print(survey)
    
    ## wn: _wn_extract.csv
    ## hhm: _hhm_extract.csv
    ## calendar: _calendar_extract.csv
    ## gps: _gps_extract.csv
    ## pregnancy history: _preg_extract.csv
    
    wn_file <- files[grepl(survey, files) & grepl("_wn_extract", files)]
    hhm_file <- files[grepl(survey, files) & grepl("_hhm_extract", files)]
    calendar_file <- files[grepl(survey, files) & grepl("_calendar_extract", files)]
    gps_file <- files[grepl(survey, files) & grepl("_gps_extract", files)]
    preg_file <- files[grepl(survey, files) & grepl("_preg_extract", files)]
    
    ## read in wn level extracted data
    wn_dt <- fread(file.path(in.dir, wn_file))
    
    ## subset to relevant columns
    possible_cols <- c("survey","cluster","id_unique","cmc_interview_date","religion","ethnicity","admin_1","admin_2","wscore","windex5","curr_preg","cmc_first_child",
                       "age_1st_birth","ever_preg","ever_term","cmc_preg_term","cmc_first_cohabit","cmc_curr_cohabit","in_first_cohabit","curr_cohabit","former_cohabit","no_read",
                       "age_1st_sex_imp","never_had_intercourse","desire_child_teen","ideal_child","never_used_contra","fp_exp_media","decision_use_joint_respondent",
                       "knowledge_mod","contra_source_public","contra_source_priv","contra_source_other","fp_se","fp_dealse","fp_othermethod","beating_just","sex_cash",
                       "preg_wanted_curr")
    wn_dt <- wn_dt[, names(wn_dt)[names(wn_dt) %in% possible_cols], with = F]
    
    ## read in hhm level extracted data
    hhm_dt <- fread(file.path(in.dir, hhm_file))
    
    ## read in wn level calendar extracted data
    calendar_dt <- fread(file.path(in.dir, calendar_file))
    
    ## read in GPS extracted data, if available
    if (!is_empty(gps_file)) gps_dt <- fread(file.path(in.dir, gps_file))
    
    ## read in pregnancy extracted data, if available
    if (!is_empty(preg_file)) preg_dt <- fread(file.path(in.dir, preg_file))
    
    
    # MERGE FILES ---------------------------------------------------------
    
    ## merge wn and hhm files, all women should appear in the hhm file
    merged_dt_1 <- merge(wn_dt, hhm_dt, by = intersect(names(wn_dt), names(hhm_dt)))
    
    # quick check
    try(if(nrow(merged_dt_1) != nrow(wn_dt)) stop("Women getting dropped! Misalignment between wn and hhm files!"))
    
    ## merge merged_dt_1 (women) and calendar file, keeping all calendar observations
    merged_dt_2 <- merge(merged_dt_1, calendar_dt, by = intersect(names(merged_dt_1), names(calendar_dt)))
    
    # quick check
    try(if(nrow(merged_dt_2) != nrow(calendar_dt)) stop("Women getting dropped! Misalignment between calendar and wn/hmm files!"))
    
    
    # MERGE CONSTRUCTED WEALTH QUINTILES ------------------------------
    
    ## read in wealth quintile estimates for country of interest
    wq_estimates <- fread(file.path(in.dir,"wealth_quintile_construction/cohort",paste0(cur_country,"_wealth_quintile_estimates.csv")))
    
    ## subset to survey of interest
    wq_estimates <- wq_estimates[survey == get("survey", envir = 1)]
    
    ## merge onto merged_dt_3, keep all women
    merged_dt_3 <- merge(merged_dt_2, wq_estimates, by = c("survey", "hhid_unique"), all.x = T)
    
    
    # MERGE ON GPS COORDINATES ----------------------------------------
    
    ## merge gps file onto merged_dt_3, if gps file is available, keep all wn
    if (!is_empty(gps_file)) merged_dt_3 <- merge(merged_dt_3, gps_dt, by = intersect(names(merged_dt_3), names(gps_dt)), all.x = T)
    
    # MERGE ON PREGNANCY HISTORY --------------------------------------
    
    ## merge preg file onto merged_dt_3, if preg file is available, keep all wn
    if (!is_empty(preg_file)) {
      
      # rename preg_end_cmc to month_cmc
      setnames(preg_dt, "preg_end_cmc", "month_cmc")
      
      # merge
      merged_dt_3 <- merge(merged_dt_3, preg_dt, by = intersect(names(merged_dt_3), names(preg_dt)), all.x = T)
      
      # set preg_id to 0 for women pregnant at time of the survey
      merged_dt_3[month_cmc == cmc_interview_date & event_recode == "pregnancy", preg_id := 0]
      
      # identify women with pregnancy info
      merged_dt_3[event_recode %in% c("pregnancy","birth","termination"), has_preg_info := !is.na(max(preg_id,na.rm=T)), by = "id_unique"]
      
      # label all months pregnant with their pregnancy id 
      merged_dt_3[event_recode %in% c("pregnancy","birth","termination") & has_preg_info == T, preg_id_filled := na.locf(preg_id, fromLast = T), by = "id_unique"]
      
      # create combined preg_wanted variable from past and current pregnancy
      merged_dt_3[preg_id_filled == 0, preg_desire := preg_wanted_curr]
      merged_dt_3[preg_id_filled > 0, preg_desire := preg_wanted]
      merged_dt_3[preg_id_filled > 0 & has_preg_info == T, preg_desire := na.locf(preg_wanted, fromLast = T), by = c("id_unique", "preg_id_filled")]
      
      # break preg_desire into separate columns
      merged_dt_3[, preg_desire_then := ifelse(preg_desire == "then", 1, 0)]
      merged_dt_3[, preg_desire_later := ifelse(preg_desire == "later", 1, 0)]
      merged_dt_3[, preg_desire_not_at_all := ifelse(preg_desire %in% c("no more","not at all"), 1, 0)]
    }
    
    # PROCESS VARIABLES -----------------------------------------------
    
    # copy table
    dt <- copy(merged_dt_3)
    
    # check ordering of months
    # ensure table is orderer from latest to oldest month in descending order (i.e. birth, pregnancy, pregnancy)
    dt <- dt[order(id_unique, -month_cmc)]
    
    # interview age
    dt[, int_age := max(age), by = "id_unique"]
    
    # time varying variables
    
    ## currently partnered
    
    # if in first cohabitation, use date of first cohabitation
    dt[in_first_cohabit == 1, curr_cohabit_timevary := ifelse(month_cmc >= cmc_first_cohabit, 1, 0)]
    # if never cohabited, set to 0
    dt[curr_cohabit == 0 & former_cohabit == 0, curr_cohabit_timevary := 0]
    # if date of current cohabitation available, use
    if ("cmc_curr_cohabit" %in% names(dt)) dt[!is.na(cmc_curr_cohabit), curr_cohabit_timevary := ifelse(month_cmc >= cmc_curr_cohabit, 1, 0)]
    
    ## ever had intercourse
    
    # potentially use to set month of first sex to first cohabit
    dt[, age_first_cohabit := as.integer((cmc_first_cohabit - cmc_woman_dob)/12)]  
    # if never had intercourse, set to 0
    dt[never_had_intercourse == 1, ever_had_intercourse_timevary := 0]
    # if had intercourse, use age at first intercourse
    dt[!is.na(age_1st_sex_imp), ever_had_intercourse_timevary := ifelse(age >= age_1st_sex_imp, 1, 0)]
    
    # check for nonsensical data (age at first intercourse after a birth/pregnancy)
    message(paste0(length(dt[birth_preg == 1 & ever_had_intercourse_timevary == 0, unique(id_unique)]), " women out of ",
                   length(unique(dt$id_unique)), " with inconsistent sexual experience data!!"))
    
    # assume first had sex to be month before first pregnancy
    dt[event_recode == "pregnancy", min_cmc_preg := min(month_cmc), by = "id_unique"]
    dt[, min_cmc_preg := min_cmc_preg[!is.na(min_cmc_preg)][1L], by = "id_unique"]  # fill in rest of rows
    dt[month_cmc >= min_cmc_preg, ever_had_intercourse_timevary := 1]
    
    
    ## education in years
    
    # Nepal
    # primary school entrance age is 5, 5 yrs primary (ages 5-9), 3 lower second (ages 10-12), 4 upper second (ages 13-16)
    # primary = grades 1-5, lower secondary = grades 6-8, upper secondary = grades 9-12, higher = 13+
    # school year runs from April to April
    # age 6 = year 1 completed
    # age 10 = year 5 completed
    # age 13 = year 8 completed
    # age 17 = year 12 completed
    # age 18 = year 13 completed
    if (cur_country == "np") {
      end_school_yr <- 4
      diff_age_grade <- 5
    }
    
    # Rwanda
    # primary school entrance age is 7, 6 yrs primary (ages 7-12), 3 junior second (ages 13-15), 3 senior second (ages 16-18), 4 bachelors (19+)
    # primary = grades 1-6, junior secondary = 7-9, senior secondary = 10-12, higher = 13+
    # school year runs from February to November
    # age 8 = year 1 completed
    # age 16 = year 9 completed
    # age 19 = year 12 completed
    # age 20 = year 13 completed
    if (cur_country == "rw") {
      end_school_yr <- 11
      diff_age_grade <- 7
    }
    
    # Ghana
    # primary school entrance age is 6, 6 yrs primary (ages 6-11), 3 junior second (ages 12-14), 3 senior second (ages 15-17), 4 bachelors (17+)
    # primary = grades 1-6, junior secondary = 7-9, senior secondary = 10-12, higher = 13+
    # school year runs from August to May
    # age 7 = year 1 completed
    # age 12 = year 6 completed
    # age 15 = year 9 completed
    # age 18 = year 12 completed
    # age 19 = year 13 completed
    if (cur_country == "gh") {
      end_school_yr <- 5
      diff_age_grade <- 6
    }
    
    # Malawi
    # primary school entrance age is 6, 8 yrs primary (ages 6-13), 4 secondary (ages 14-17), 4 university (18+)
    # primary = grades 1-8, secondary = 9-12, higher = 13+
    # school year runs from Sept to July
    # age 7 = year 1 completed
    # age 14 = year 8 completed
    # age 18 = year 12 completed
    # age 19 = year 13 completed
    if (cur_country == "mw") {
      end_school_yr <- 7
      diff_age_grade <- 6
    }
    
    # if currently attending school, use years completed
    dt[attend_school == 1, educ_yrs_timevary := educ_single_yrs]
    
    # assume they have been consistently attending and subtract 1 year for each previous school year 
    dt[, school_year := if_else(month(month) >= end_school_yr, year(month), year(month) - 1)]
    dt[, school_diff := (max(school_year) - school_year), by = "id_unique"]
    dt[, educ_yrs_timevary := educ_single_yrs - school_diff]
    dt[attend_school == 1 & educ_yrs_timevary < 0, educ_yrs_timevary := 0]
    
    # if never attend school, set all to 0
    dt[educ_single_yrs == 0, educ_yrs_timevary := 0]
    
    # if not currently attending school, assume they completed each level at the recommended age (should be 5 years older than the level completed)
    # keep max educ_single_yrs unless at an age they likely were attending school in the past, and adjust educ_single_yrs accordingly
    dt[is.na(attend_school) | attend_school == 0, educ_yrs_timevary := ifelse(age - educ_single_yrs >= diff_age_grade, educ_single_yrs, educ_single_yrs - (diff_age_grade - (age - educ_single_yrs)))]
    dt[is.na(attend_school) | attend_school == 0, educ_yrs_timevary := min(educ_yrs_timevary), by = c("id_unique", "school_year")]  # align with school years
    

    ## currently attending school (only asked to 15-24)
    dt[, attend_school_timevary := attend_school]
    dt[educ_single_yrs != educ_yrs_timevary, attend_school_timevary := 1]
    dt[, first_school_yr := max(school_year - educ_yrs_timevary), by = "id_unique"]
    dt[school_year < first_school_yr, attend_school_timevary := 0]
    if ("ever_attend_school" %in% names(dt)) dt[ever_attend_school == 0, attend_school_timevary := 0]
    
    if (cur_country == "rw") dt[month(month) %in% c(12,1), attend_school_timevary := 0]   # school year runs Feb to Nov
    if (cur_country == "gh") dt[month(month) %in% c(6,7), attend_school_timevary := 0]    # school year runs Aug to May
    if (cur_country == "mw") dt[month(month) %in% c(8), attend_school_timevary := 0]      # school year runs Sept to July
    
    
    ## method information index (mii)
    # only asked of women using certain modern methods:
    # female sterilization, iud, pill, implant, injections, condoms, emergency, other modern and sometimes SDM (but we don't want)
    dt[, method_info_idx := ifelse(fp_se == 1 & fp_dealse == 1 & fp_othermethod == 1, 1, 0)]
    dt[, method_info_idx_timevary := ifelse(event_recode %in% c("female sterilization", "iud", "pill", "implants", "injection", 
                                                                "condom", "emergency contraception", "other modern method"), method_info_idx, 0)]
    
    ## decision to use contraception made by respondent or together with partner
    # only asked of women currently married/in-union using contraception EXECEPT Nepal endline which is all married/in-union women
    dt[curr_cohabit_timevary == 0, decision_use_joint_respondent := 1]
    if (survey == "NPL_DHS8_2022") dt[, decision_use_joint_respondent_timevary_np_2022 := decision_use_joint_respondent]   # all partnered women answered
    dt[, decision_use_joint_respondent_timevary := ifelse(any_contra == 1, decision_use_joint_respondent, 0)]              # only partnered women using contra answered
    
    
    ## public source of contraception
    dt[, contra_source_public_timevary := ifelse(mod_contra == 1, contra_source_public, 0)]
    
    
    # EXPORT PREPPED DATA ------------------------------------------
    
    write.csv(dt, file.path(out.dir, paste0(survey, "_cohort_prepped.csv")), row.names = F)
  }
}

  
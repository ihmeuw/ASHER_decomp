#-------------------Header------------------------------------------------
# Project: IHME ASHER Decomposition
# Purpose: Run cohort survival analysis
# Date: 2/28/2024
# Notes:
#***************************************************************************

# SET-UP ---------------------------------------------------------------

# clear memory
rm(list=ls())

# Username is pulled automatically
username <- Sys.getenv("USER") 

# runtime configuration
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
pacman::p_load(data.table,tidyverse,parallel,plyr,dplyr,haven,survey,tools,survival,ggfortify)

# in/out
in.dir <- "FILEPATH"
out.dir <- "FILEPATH"

# create function for the opposite of %in%
'%ni%' <- Negate('%in%')

# turn off scientific notation
options(scipen = 999)

# create function to convert CMC date to year and month
get_cmc_year <- function(x) 1900 + as.integer((as.integer(x) - 1) / 12)
get_cmc_month <- function(x) as.integer(x) - 12 * (get_cmc_year(x) - 1900)



# PREP DATA -------------------------------------------------------------

# list csv files in prepped directory
files <- list.files(in.dir, pattern = ".csv")

# OPTIONAL: subset to files for country of interest
# files <- files[grepl("np", tolower(files))]

# read in and combine data
dt <- rbindlist(lapply(file.path(in.dir, files), fread), fill = T)

# list of variables for analysis
analysis_vars <- c("survey","country","cluster","id_unique","pweight","admin_1","admin_1_shp","cmc_woman_dob","cmc_interview_date","month_cmc",
                   "event_recode","reason_recode","method_failure","desire_to_become_pregnant","other_fertility_related_reasons",
                   "side_effects_health_concerns","wanted_more_effective_method","other_method_related","other_dk",
                   "cmc_first_child","ever_preg","ever_term","cmc_preg_term","any_term_before_cal",
                   "age","urban","curr_cohabit_timevary","ever_had_intercourse_timevary","educ_yrs_timevary", "attend_school_timevary",
                   "method_info_idx_timevary","decision_use_joint_respondent_timevary","decision_use_joint_respondent_timevary_np_2022",
                   "wealth_quintiles","windex5","fp_exp_media","knowledge_mod","mod_contra","trad_contra","any_contra","birth_preg",
                   "contra_source_public_timevary","mean_yrs_schooling_head","ideal_child","age_1st_sex_imp","beating_just","sex_cash",
                   "preg_desire","preg_desire_then","preg_desire_later","preg_desire_not_at_all")
dt_analysis <- dt[, analysis_vars[analysis_vars %in% names(dt)], with = F]

# OPTIONAL: remove full dt to save memory space
remove(dt)

# create grouped education
# no education, currently attending, not attend - primary, some secondary, secondary
dt_analysis[attend_school_timevary == 0, ed_level := ifelse(educ_yrs_timevary >= 12, "Not attending: completed secondary",
                                                                   ifelse(educ_yrs_timevary < 12, "Not attending: less than secondary", NA))]
dt_analysis[attend_school_timevary == 0 & educ_yrs_timevary == 0, ed_level := "No education"]
dt_analysis[attend_school_timevary == 1, ed_level := "Currently attending"]

# set education ref group
dt_analysis$ed_level <- factor(dt_analysis$ed_level)
dt_analysis$ed_level <- relevel(dt_analysis$ed_level, ref = "Not attending: less than secondary")

# create binary out of ideal children
dt_analysis[, wants_children := ifelse(ideal_child > 0, 1, 0)]

# create time-varying method failure
dt_analysis[, method_fail_timevary := method_failure]
dt_analysis[is.na(method_fail_timevary), method_fail_timevary := 0]
dt_analysis[, method_fail_lag_1m := lead(method_fail_timevary), by = c("survey","id_unique")]

# if pregnant because of method failure, set contra in month of pregnancy to 1
dt_analysis[, mod_contra_inc_fail := mod_contra]
dt_analysis[method_fail_lag_1m == 1 & lead(mod_contra) == 1, mod_contra_inc_fail := 1, by = c("survey","id_unique")]

dt_analysis[, trad_contra_inc_fail := trad_contra]
dt_analysis[method_fail_lag_1m == 1 & lead(trad_contra) == 1, trad_contra_inc_fail := 1, by = c("survey","id_unique")]

dt_analysis[, any_contra_inc_fail := any_contra]
dt_analysis[method_fail_lag_1m == 1 & lead(any_contra) == 1, any_contra_inc_fail := 1, by = c("survey","id_unique")]

dt_analysis[any_contra_inc_fail == 1, event_recode_failed := lead(event_recode), by = c("survey","id_unique")]
dt_analysis[method_fail_lag_1m == 0, event_recode_failed := NA]

# create grouped contra use by method type 
dt_analysis[any_contra_inc_fail == 0, current_contra_type := "No method"]
dt_analysis[mod_contra_inc_fail == 1, current_contra_type := "Modern method"]
dt_analysis[trad_contra_inc_fail == 1, current_contra_type := "Traditional method"]

dt_analysis[event_recode %in% c("birth","no method used","pregnancy","termination"), current_contra_subtype := "No method"]
dt_analysis[event_recode %in% c("male sterilization","female sterilization","implants","iud"), current_contra_subtype := "Modern Long-acting"]
dt_analysis[event_recode %in% c("injections","pill","condom","diaphragm","foam/jelly","other modern method",
                                "female condom","emergency contraception"), current_contra_subtype := "Modern Short-acting"]
dt_analysis[event_recode %in% c("calendar methods","withdrawal","rhythm","lactational amenorrhea method",
                                "other traditional method"), current_contra_subtype := "Traditional"]

dt_analysis[event_recode_failed %in% c("male sterilization","female sterilization","implants","iud"), current_contra_subtype := "Modern Long-acting"]
dt_analysis[event_recode_failed %in% c("injections","pill","condom","diaphragm","foam/jelly","other modern method",
                                       "female condom","emergency contraception"), current_contra_subtype := "Modern Short-acting"]
dt_analysis[event_recode_failed %in% c("calendar methods","withdrawal","rhythm","lactational amenorrhea method",
                                       "other traditional method"), current_contra_subtype := "Traditional"]

# set reference group to be "no method"
dt_analysis$current_contra_type <- factor(dt_analysis$current_contra_type) 
dt_analysis$current_contra_type <- relevel(dt_analysis$current_contra_type, ref = "No method")

dt_analysis$current_contra_subtype <- factor(dt_analysis$current_contra_subtype) 
dt_analysis$current_contra_subtype <- relevel(dt_analysis$current_contra_subtype, ref = "No method")



# CREATE COHORT ----------------------------------------------------------

# age at start and end of calendar
dt_analysis[, age_start := min(age), by = c("survey", "id_unique")]
dt_analysis[, age_end := max(age), by = c("survey", "id_unique")]

# cmc date of first month captured by calendar
dt_analysis[, start_cal_cmc := min(month_cmc), by = c("survey", "id_unique")]

# identify month a woman turned 15
dt_analysis[, cmc_age_15 := cmc_woman_dob + (12*15)]

# drop women who have been pregnant before 15
dt_analysis <- dt_analysis[is.na(cmc_first_child) | (cmc_first_child - 8) >= cmc_age_15]

dt_analysis[event_recode %in% c("pregnancy","termination"), first_preg_cmc := min(month_cmc), by = c("survey", "id_unique")]
dt_analysis[, first_preg_cmc := min(first_preg_cmc, na.rm = T), by = c("survey", "id_unique")]

dt_analysis <- dt_analysis[is.na(first_preg_cmc) | first_preg_cmc >= cmc_age_15]

# drop women who were pregnant before start of calendar
dt_analysis <- dt_analysis[is.na(cmc_first_child) | (cmc_first_child - 8) >= start_cal_cmc]

# drop women who have had a termination before 15
dt_analysis <- dt_analysis[is.na(cmc_preg_term) | cmc_preg_term >= cmc_age_15]

# drop women who have had a termination before start of calendar (won't be able to capture first month pregnant)
dt_analysis <- dt_analysis[is.na(any_term_before_cal) | any_term_before_cal == 0]

# keep ages 15-19
dt_analysis <- dt_analysis[age %in% 15:19]

# cmc date of first observed month
dt_analysis[, start_month_cmc := min(month_cmc), by = c("survey", "id_unique")]

# number months starting from the first month a woman is 15
dt_analysis[, month := (month_cmc - cmc_age_15) + 1, by = c("survey", "id_unique")]

# identify events/failures, first month of very first pregnancy (could be pregnancy or termination if terminated in first month)
dt_analysis[first_preg_cmc == month_cmc, event_first_preg := 1]
dt_analysis[is.na(first_preg_cmc) | first_preg_cmc != month_cmc, event_first_preg := 0]

# censor (drop) months after a woman has had an event (first month of pregnancy)
dt_analysis <- dt_analysis[is.na(first_preg_cmc) | month_cmc <= first_preg_cmc]

# create start and stop times of observation
dt_analysis[, tstart := month - 1]  # exclusive
dt_analysis[, tstop := month]       # inclusive

# calendar year
dt_analysis[country == "np", year := get_cmc_year(month_cmc - ((56*12)+8))]
dt_analysis[country != "np", year := get_cmc_year(month_cmc)]

## Sub-cohort for two-stage analysis dependent on sexual debut

# create event of first sexual intercourse
dt_analysis[ever_had_intercourse_timevary == 1, first_sex_cmc := min(month_cmc, na.rm = T), by = c("survey", "id_unique")]
dt_analysis[first_sex_cmc == month_cmc, event_first_sex := 1]
dt_analysis[is.na(first_sex_cmc) | first_sex_cmc != month_cmc, event_first_sex := 0]

# keep in part 1 if never had sex, or had first sex during observation
dt_analysis_first_sex_p1 <- dt_analysis[is.na(first_sex_cmc) | month_cmc <= first_sex_cmc]
dt_analysis_first_sex_p1 <- dt_analysis_first_sex_p1[age_1st_sex_imp >= age_start]

# keep in part 2 if had first sex during observation
dt_analysis_first_sex_p2 <- dt_analysis[month_cmc >= first_sex_cmc]
dt_analysis_first_sex_p2 <- dt_analysis_first_sex_p2[age_1st_sex_imp >= age_start]



# SURVIVAL CURVES ------------------------------------------------------

# Ghana
fit <- survfit(Surv(tstart, tstop, event_first_preg) ~ 1, data = dt_analysis[country == "gh"], weights = pweight)
autoplot(fit, ylab = "No pregnancy", xlab = "Time (Months)", conf.int = F) + theme_bw() + theme(legend.title = element_blank())
ggsave(file.path(out.dir, "GHA_survival_curve.png"), width = 8, height = 5)

# Malawi
fit <- survfit(Surv(tstart, tstop, event_first_preg) ~ 1, data = dt_analysis[country == "mw"], weights = pweight)
autoplot(fit, ylab = "No pregnancy", xlab = "Time (Months)", conf.int = F) + theme_bw() + theme(legend.title = element_blank())
ggsave(file.path(out.dir, "MWI_survival_curve.png"), width = 8, height = 5)

# Nepal
fit <- survfit(Surv(tstart, tstop, event_first_preg) ~ 1, data = dt_analysis[country == "np"], weights = pweight)
autoplot(fit, ylab = "No pregnancy", xlab = "Time (Months)", conf.int = F) + theme_bw() + theme(legend.title = element_blank())
ggsave(file.path(out.dir, "NPL_survival_curve.png"), width = 8, height = 5)

# Rwanda
fit <- survfit(Surv(tstart, tstop, event_first_preg) ~ 1, data = dt_analysis[country == "rw"], weights = pweight)
autoplot(fit, ylab = "No pregnancy", xlab = "Time (Months)", conf.int = F) + theme_bw() + theme(legend.title = element_blank())
ggsave(file.path(out.dir, "RWA_survival_curve.png"), width = 8, height = 5)



# TEST MODELS ----------------------------------------------------------

# NOTES:
# If working with a single survey, use windex5 not wealth_quintiles
# wealth_quintiles was made via PCA with all DHS/MICS data, should be rerun with only included surveys to be accurate

test_models <- function(cur_country = "") {
  
  # empty table to store coefficients
  coefs <- data.table()
  
  # corresponding country ihme_loc_id
  if (cur_country == "gh") ihme_loc_id <- "GHA"
  if (cur_country == "mw") ihme_loc_id <- "MWI"
  if (cur_country == "np") ihme_loc_id <- "NPL"
  if (cur_country == "rw") ihme_loc_id <- "RWA"
  
  # save country cohort data
  write.csv(dt_analysis[country == cur_country], file.path(out.dir, paste0(ihme_loc_id, "_model_cohort.csv")), row.names = F)
  
  # adjust mod contra for method failure, categorical ed_level
  fit1_v1 <- coxph(formula = Surv(tstart, tstop, event_first_preg) ~ urban + as_factor(wealth_quintiles) + mean_yrs_schooling_head +
                     curr_cohabit_timevary + ever_had_intercourse_timevary + ed_level + mod_contra_inc_fail + 
                     beating_just,
                   data = dt_analysis[country == cur_country], weights = pweight)
  summary(fit1_v1)
  
  tmp <- summary(fit1_v1)$coefficients %>% as.data.table(keep.rownames = T)
  tmp[, model := "no contra lag, inc method failure, categorical education"]
  tmp[, country := cur_country]
  write.csv(tmp, file.path(out.dir, "model_results", paste0(ihme_loc_id, "_all_contra_inc_fail_ed_level.csv")), row.names = F)
  coefs <- rbind(coefs, tmp[, data := paste0(ihme_loc_id, "_ALL")], fill = T)
  
  # method type accounting for failure, categorical ed_level
  fit1_v1 <- coxph(formula = Surv(tstart, tstop, event_first_preg) ~ urban + as_factor(wealth_quintiles) + mean_yrs_schooling_head +
                     ever_had_intercourse_timevary + ed_level + curr_cohabit_timevary + as.factor(current_contra_type) + 
                     beating_just,
                   data = dt_analysis[country == cur_country], weights = pweight)
  summary(fit1_v1)
  
  tmp <- summary(fit1_v1)$coefficients %>% as.data.table(keep.rownames = T)
  tmp[, model := "contra type inc fail, categorical education"]
  tmp[, country := cur_country]
  write.csv(tmp, file.path(out.dir, "model_results", paste0(ihme_loc_id, "_all_contra_type_inc_fail_ed_level.csv")), row.names = F)
  coefs <- rbind(coefs, tmp[, data := paste0(ihme_loc_id, "_ALL")], fill = T)
  
  # method subtype accounting for failure, categorical ed_level
  fit1_v1 <- coxph(formula = Surv(tstart, tstop, event_first_preg) ~ urban + as_factor(wealth_quintiles) + mean_yrs_schooling_head +
                     curr_cohabit_timevary + ever_had_intercourse_timevary + ed_level + as.factor(current_contra_subtype) + beating_just,
                   data = dt_analysis[country == cur_country], weights = pweight)
  summary(fit1_v1)
  
  tmp <- summary(fit1_v1)$coefficients %>% as.data.table(keep.rownames = T)
  tmp[, model := "contra subtype inc fail, drop knowledge, categorical education"]
  tmp[, country := cur_country]
  write.csv(tmp, file.path(out.dir, "model_results", paste0(ihme_loc_id, "_all_contra_subtype_inc_fail_ed_level.csv")), row.names = F)
  coefs <- rbind(coefs, tmp[, data := paste0(ihme_loc_id, "_ALL")], fill = T)
  
  # part 1 mod contra inc failure, outcome is sexual debut, categorical ed_level
  fit1_v1 <- coxph(formula = Surv(tstart, tstop, event_first_sex) ~ urban + as_factor(wealth_quintiles) + mean_yrs_schooling_head +
                     curr_cohabit_timevary + ed_level + beating_just,
                   data = dt_analysis_first_sex_p1[country == cur_country], weights = pweight)
  summary(fit1_v1)
  
  tmp <- summary(fit1_v1)$coefficients %>% as.data.table(keep.rownames = T)
  tmp[, model := "Two-stage model: part 1 outcome is first sexual intercourse, categorical education"]
  tmp[, country := cur_country]
  write.csv(tmp, file.path(out.dir, "model_results", paste0(ihme_loc_id, "_all_part_1_outcome_first_sex_ed_level.csv")), row.names = F)
  coefs <- rbind(coefs, tmp[, data := paste0(ihme_loc_id, "_ALL")], fill = T)
  
  
  # part 2 mod contra inc failure, outcome is first pregnancy, among those who had event_first_sex, categorical ed_level
  fit1_v1 <- coxph(formula = Surv(tstart, tstop, event_first_preg) ~ urban + as_factor(wealth_quintiles) + mean_yrs_schooling_head +
                     curr_cohabit_timevary + ed_level + mod_contra_inc_fail + beating_just,
                   data = dt_analysis_first_sex_p2[country == cur_country], weights = pweight)
  summary(fit1_v1)
  
  tmp <- summary(fit1_v1)$coefficients %>% as.data.table(keep.rownames = T)
  tmp[, model := "Two-stage model: part 2 outcome is first pregnancy among those who have had sex, categorical education"]
  tmp[, country := cur_country]
  write.csv(tmp, file.path(out.dir, "model_results", paste0(ihme_loc_id, "_all_part_2_outcome_first_preg_ed_level.csv")), row.names = F)
  coefs <- rbind(coefs, tmp[, data := paste0(ihme_loc_id, "_ALL")], fill = T)
  
  # save results
  write.csv(x = coefs[grepl(ihme_loc_id, data)], file.path(out.dir, paste0(ihme_loc_id, "_models_coefs.csv")), row.names = F)
  
  # return table of coefficients
  return(coefs)
}

coefs_gha <- test_models(cur_country = "gh")
coefs_mwi <- test_models(cur_country = "mw")
coefs_npl <- test_models(cur_country = "np")
coefs_rwa <- test_models(cur_country = "rw")

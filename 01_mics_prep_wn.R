#-------------------Header------------------------------------------------
# Project: IHME ASHER Decomposition
# Purpose: Prepare variables from woman's file from MICS surveys
# Notes:
#***************************************************************************

# SET-UP -----------------------------------------------------------

# clear environment
rm(list=ls())
username <- Sys.info()[["user"]]

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

# load packages
pacman::p_load(magrittr,tidyverse,parallel,plyr,dplyr,haven,survey,tools,devtools)

# in/out
out.dir <- "FILEPATH"

# create function for the opposite of %in%
'%ni%' <- Negate('%in%')


# EXTRACT DATA FUNCTION -----------------------------------------------

extract_data <- function(survey, cur_country) {

  # read in data 
  dt <- data.table(read_dta(survey))
  
  
  ## SURVEY CHARACTERISTICS ----------------------------------------
  message("||---Survey characteristics")
  
  # convert variable names to lowercase
  names(dt) <- tolower(names(dt))
  
  # convert character columns to lowercase
  dt[] <- lapply(dt, function(x) if(is.character(x)) tolower(x) else x)
  
  # extract survey name and add to dt
  survey_name <- str_extract(survey, "[A-Z]{3}_MICS[0-9]_[0-9]{4}(_[0-9]{4})*")
  dt[, survey := survey_name]
  
  # identifiers
  dt[, cluster := ifelse(grepl("MICS2", survey), wiclno, hh1)] 
  dt[, hh_id := ifelse(grepl("MICS2", survey), wihhno, hh2)]
  dt[, id := ifelse(grepl("MICS2", survey), wilnno, ln)]
  dt[, area_unit := ifelse(grepl("MICS2", survey), hi6, hh6)]
  dt[, urban := ifelse(area_unit == 1, 1, 0)]
  dt[, country := cur_country]
  
  # interview date
  dt[, year := ifelse(grepl("MICS2", survey), hi3y, wm6y)]
  dt[, month := ifelse(grepl("MICS2", survey), hi3m, wm6m)]
  dt[, cmc_interview_date := ifelse(grepl("MICS2", survey), cmcdoi, 
                                            ifelse(grepl("MICS3", survey), cmcdoiw, wdoi))]
  
  # convert Nepali calendar dates to Gregorian
  if (grepl("NPL", survey)) {
    dt[, year := 1990 + as.integer(cmc_interview_date/12)]
    dt[, month := cmc_interview_date - 12*(year-1990)]
  }
  
  # woman's sample weight
  dt[, pweight := wmweight]
  dt[, wpweight := pweight*1000000]
  
  # unique hh id for each household
  dt[, hhid_unique := paste(hh_id, cluster)]
  
  # unique individual id for each woman
  dt[, id_unique := paste(hhid_unique, id)]
  
  
  ## HOUSEHOLD CHARACTERISTICS ------------------------------------
  message("||---Household characteristics")
  
  # ethnicity (assumed household head)
  # MISSING: RWA 2000, NPL 2014 and 2019, CMR 2000
  if (!grepl("MICS2|NPL_MICS[5-6]", survey)) {
    dt[, ethnicity := ifelse(grepl("CMR_MICS3|GHA_MICS3", survey), as.character(as_factor(hc1c)),
                                    ifelse(grepl("CMR_MICS5", survey), as.character(as_factor(ethnie)), 
                                           ifelse(grepl("MWI_MICS3", survey), as.character(as_factor(hc1b)), as.character(as_factor(ethnicity)))))]
    message(paste0("||-Unique ethnicity levels: ", paste(unique(dt$ethnicity), collapse=", ")))
  }

  # woman-specific ethnicity (if asked)
  if (grepl("GHA_MICS3", survey)) dt[, ethnicity_wm := as.character(as_factor(wm16))]
  if (grepl("GHA_MICS4", survey)) dt[, ethnicity_wm := as.character(as_factor(wb9))]
  
  
  ## OUTCOME-RELATED VARIABLES ------------------------------------
  message("||---Outcome-related variables")
  
  # currently pregnant
  # NOTE: CMR 2000 has a currently married gateway prior to asking if currently pregnant
  dt[, curr_preg := ifelse(grepl("RWA_MICS2", survey), wi5, 
                                   ifelse(grepl("CMR_MICS2", survey), cu2, cp1))]
  dt[, curr_preg := ifelse(curr_preg == 1, 1, ifelse(curr_preg %in% c(2,8), 0, NA))]   # recode to 0/1 
  if (grepl("CMR_MICS3", survey)) dt[cp0 == 2, curr_preg := 0]                         # catch ever sex gateway

  
  # ever given birth (does not include stillbirths)
  # MISSING: RWA 2000, CMR 2006
  if (!grepl("RWA_MICS2|CMR_MICS3", survey)) dt[, ever_birth := ifelse(cm1 == 1, 1, ifelse(cm1 == 2, 0, NA))] 
  
  # if any live births in the last 2 years
  # MISSING: RWA 2000, CMR 2000
  if (!grepl("MICS2", survey)) {
    dt[, any_births_2_yr := ifelse(grepl("CMR_MICS3", survey), tt0, 
                                       ifelse(grepl("MICS3", survey), cm12, 
                                              ifelse(grepl("MICS4|MICS5", survey), cm13, cm17)))]
    dt[, any_births_2_yr := ifelse(any_births_2_yr %in% c(1, "y", "o"), 1, ifelse(any_births_2_yr %in% c(0, "n", 2), 0, NA))]
    if ("ever_birth" %in% names(dt)) dt[is.na(any_births_2_yr) & ever_birth == 0, any_births_2_yr := 0]
  }
  
  # DOB of first child (CMC)
  # MISSING: RWA 2000, NPL 2010, CMR 2006, MWI 2006
  if ("wdobfc" %in% names(dt)) dt[, cmc_first_child := wdobfc]
  if (grepl("GHA_MICS3|CMR_MICS2", survey)) dt[cm2am %in% 1:12 & cm2ay < 9990 & cm2ay > 1990, cmc_first_child := 12*(cm2ay-1900)+cm2am]  # can impute CMC date with year and month
  
  # DOB of last child (CMC)
  # MISSING: RWA 2000, CMR 2006
  if ("wdoblc" %in% names(dt)) dt[, cmc_last_child := wdoblc]                                # GHA 2011, MICS5, MICS6
  if ("cmclb" %in% names(dt)) dt[, cmc_last_child := cmclb]                                  # MWI 2006
  if (grepl("GHA_MICS3|CMR_MICS2", survey)) dt[cm11m %in% 1:12 & cm11y < 9990 & cm11y > 1990, cmc_last_child := 12*(cm11y-1900)+cm11m]  # can impute CMC date with year and month
  if (grepl("NPL_MICS4", survey)) dt[cm12m %in% 1:12 & cm12y < 9990 & cm12y > 1990, cmc_last_child := 12*(cm12y-56-1900)+cm12m]            # can impute CMC date with year and month

  # number of children ever born
  # MISSING: RWA 2000, NPL 2010, CMR 2006
  if ("ceb" %in% names(dt)) dt[, number_births := ceb]

  # currently pregnant or any live birth
  # MISSING: RWA 2000, CMR 2006, plus married gateway for CMR 2000
  if (all(c("ever_birth", "curr_preg") %in% names(dt))) {
    dt[, any_birth_preg := ifelse(ever_birth ==1 | curr_preg ==1, 1,
                                          ifelse(!is.na(ever_birth) & !is.na(curr_preg),0,NA))]
  }

  
  ## INDIVIDUAL CHARACTERISTICS -----------------------------------
  message("||---Individual characteristics")
  
  # age in single year
  dt[, age := ifelse(grepl("MICS2", survey), hl4, 
                             ifelse(grepl("MICS3", survey), wm9, 
                                    ifelse(grepl("MICS4|MICS5", survey), wb2, wb4)))]
  
  # DOB of woman (CMC)
  dt[, cmc_woman_dob := wdob]
  
  # age at first birth
  if ("cmc_first_child" %in% names(dt)) dt[, age_1st_birth := (cmc_first_child - cmc_woman_dob)/12]

  # current marital status
  dt[, marital_status := mstatus]
  
  # currently married/in-union
  dt[, curr_cohabit := ifelse(mstatus == 1, 1, 0)]

  # formerly married/in-union
  if (grepl("RWA_MICS2", survey)) dt[, former_cohabit := ifelse(mstatus %in% 2:4, 1, 0)]
  if (grepl("CMR_MICS2|MICS[3-6]", survey)) dt[, former_cohabit := ifelse(mstatus == 2, 1, 0)]
  
  # ever married/in-union
  dt[, ever_cohabit := ifelse(curr_cohabit == 1 | former_cohabit == 1, 1, 0)]
  
  # if still in first marriage/union
  if (grepl("MICS3", survey)) dt[, in_first_cohabit := ifelse(ma5 == 1, 1, ifelse(ma5 == 2, 0, NA))]
  if (grepl("MICS[4-6]", survey)) dt[, in_first_cohabit := ifelse(ma7 == 1, 1, ifelse(ma7 == 2, 0, NA))]
  if (grepl("MICS[3-6]", survey)) dt[ever_cohabit == 0, in_first_cohabit := 0]
  
  # date and age at first marriage/union
  # MISSING FROM RWA 2000 AND CMR 2000
  if ("wdom" %in% names(dt)) dt[, cmc_first_cohabit := wdom]
  if (grepl("CMR_MICS3|GHA_MICS3", survey)) dt[ma6m %in% 1:12 & ma6y < 9990, cmc_first_cohabit := 12*(ma6y-1900)+ma6m]   # can impute CMC date with year and month
  
  if (grepl("MICS3", survey)) dt[, age_first_cohabit := agem]
  if (grepl("MICS[4-6]", survey)) dt[, age_first_cohabit := wagem]

  # education
  if ("melevel" %in% names(dt)) dt[, edu_level_categ_wn := as.character(as_factor(melevel))] 
  if ("welevel" %in% names(dt)) dt[, edu_level_categ_wn := as.character(as_factor(welevel))] 
  if (grepl("NPL_MICS6", survey)) dt[, edu_level_categ_wn := as.character(as_factor(welevel1))] 
  message(paste0("||-Unique education levels: ", paste(unique(dt$edu_level_categ_wn), collapse=", ")))
  
  # ever attended school
  if (grepl("MICS2", survey)) dt[, ever_attend_school := ifelse(ed15 ==1,1,ifelse(ed15 == 2,0,NA))]
  if (grepl("MICS3", survey)) dt[, ever_attend_school := ifelse(wm10 ==1,1,ifelse(wm10 == 2,0,NA))]
  if (grepl("MICS[4-5]", survey)) dt[, ever_attend_school := ifelse(wb3 ==1,1,ifelse(wb3 == 2,0,NA))]
  if (grepl("MICS6", survey)) dt[, ever_attend_school := ifelse(wb5 ==1,1,ifelse(wb5 == 2,0,NA))]
  
  # wife beating justified
  # only using dv1a-dv1e as these appear in all surveys with this module
  if ("dv1a" %in% names(dt)){
    dt[, beating_just := ifelse(dv1a == 1, 1, ifelse(dv1a %in% c(2,8), 0, NA))]
    dt[dv1b ==1, beating_just := 1]
    dt[dv1c ==1, beating_just := 1]
    dt[dv1d ==1, beating_just := 1]
    dt[dv1e ==1, beating_just := 1]
  }
  
  # FGM (female genital mutilation)
  # ONLY AVAILABLE IN GHA 2006, 2011, 2017-2018
  if ("fg3" %in% names(dt)) dt[, fgm := ifelse(fg3 == 1, 1, ifelse(fg3 == 2, 0, NA))]

  
  ## SEXUAL EXPERIENCE ------------------------------------------------
  message("||---Sexual experience")
  
  # MISSING FROM CMR 2000, RWA 2000, NPL 2010, NPL 2014
  # CMR 2006 only asked to 15-24
  if ("sb1" %in% names(dt)) {
    
    # age at first sex
    dt[, age_1st_sex_imp := sb1]
    
    # 0 means have never had sexual intercourse
    dt[, never_had_intercourse := ifelse(age_1st_sex_imp == 0, 1,
                                                 ifelse(age_1st_sex_imp > 0 & age_1st_sex_imp != 99,0,NA))]
    
    # CMR 2006 has an additional ever sex gateway before contraception and sexual experience sections
    if (grepl("CMR_MICS3", survey)) dt[cp0 == 2, never_had_intercourse := 1]
    
    # many report that it is "at the time of cohabitation", so impute age at first cohabitation here
    dt[age_1st_sex_imp == 95, age_1st_sex_imp := age_first_cohabit]

    # set 0 (never had intercourse) and 99 (no response) to NA
    dt[age_1st_sex_imp %in% c(0,99), age_1st_sex_imp := NA]
    
    # time since last sexual intercourse
    dt[, last_sex_unit := ifelse(grepl("MICS(3|6)", survey), sb2u, sb3u)]
    dt[, last_sex_timing := ifelse(grepl("MICS(3|6)", survey), sb2n, sb3n)]
    
    # calculate if sexually active in the last four weeks
    dt[last_sex_unit == 1, rec_sex_activity := ifelse(last_sex_timing <= 28, "Active in last 4 weeks", "Not active in last 4 weeks")] ## days 
    dt[last_sex_unit == 2, rec_sex_activity := ifelse(last_sex_timing <= 4, "Active in last 4 weeks", "Not active in last 4 weeks")]  ## weeks
    dt[last_sex_unit == 3, rec_sex_activity := ifelse(last_sex_timing <= 1, "Active in last 4 weeks", "Not active in last 4 weeks")]  ## months
    dt[last_sex_unit == 4, rec_sex_activity := "Not active in last 4 weeks"]                                                          ## years
    dt[never_had_intercourse == 1, rec_sex_activity := "Not active in last 4 weeks"]
    
    # average gap in years between median ages at first intercourse and first marriage
    dt[, gap_sex_mar := age_1st_sex_imp - age_first_cohabit]
  } 
  
  
  # FERTILITY PREFERENCES --------------------------------------------
  message("||---Fertility preferences")
  
  # MISSING FOR CMR 2000, 2006, RWA 2000, GHA 2006, MWI 2006
  if (grepl("MICS[4-6]", survey)) {
    
    # desire for last birth, if birth within last 2 years
    dt[, desire_child_then := ifelse(grepl("MICS[4-5]", survey), db1, db2)]     # wanted last birth then
    dt[, desire_child_later := ifelse(grepl("MICS[4-5]", survey), db2, db4)]    # wanted last birth later or not at all
    
    # desire for another child
    dt[, fert_pref_preg := ifelse(grepl("MICS[4-5]", survey), un4, un5)]       # respondents who are currently pregnant
    dt[, fert_pref_notpreg := ifelse(grepl("MICS[4-5]", survey), un6, un7)]    # respondents who are not currently pregnant
    dt[, fertility_pref := ifelse(!is.na(fert_pref_preg),
                                          fert_pref_preg,
                                          fert_pref_notpreg)]
    
    # desired waiting time for another child
    dt[, desire_unit := ifelse(grepl("MICS[4-5]", survey), un7u, un8u)]       # time unit
    dt[, desire_timing := ifelse(grepl("MICS[4-5]", survey), un7n, un8n)]     # time value
    
    # identify adolescents who desire a/another child within adolescence 
    dt[, desire_child_teen := ifelse(age==15 & (desire_unit==93 | (desire_unit==2 & desire_timing<5) | (desire_unit==1 & desire_timing<60)), 1,
                              ifelse(age==16 & (desire_unit==93 | (desire_unit==2 & desire_timing<4) | (desire_unit==1 & desire_timing<48)), 1,
                              ifelse(age==17 & (desire_unit==93 | (desire_unit==2 & desire_timing<3) | (desire_unit==1 & desire_timing<36)), 1,
                              ifelse(age==18 & (desire_unit==93 | (desire_unit==2 & desire_timing<2) | (desire_unit==1 & desire_timing<24)), 1,
                              ifelse(age==19 & (desire_unit==93 | (desire_unit==2 & desire_timing<1) | (desire_unit==1 & desire_timing<12)), 1,
                              ifelse(age %in% 15:19, 0, NA))))))]
    
    # construct desire spacing and limiting
    # desire spacing = wants more children, but in 2+ years, or are unsure about more children/timing
    # desire limiting = wants no more children
    dt[, desire_spacing := ifelse(fertility_pref == 8 | desire_timing %in% c(95,98)| (desire_unit == 1 & desire_timing >= 24) | (desire_unit == 2 & desire_timing >= 2), 1, 0)]
    dt[, desire_limiting := ifelse(fertility_pref == 2, 1, 0)]
    
    # desire soon = wants a child soon
    dt[, desire_soon := ifelse(desire_timing %in% c(93,993), 1, 0)]
    
    # desire spacing among only women who do want limiting
    dt[, desire_spacing_nested := desire_spacing]
    dt[desire_limiting == 1, desire_spacing_nested := as.numeric(NA)]
    
    # update denominator
    dt[is.na(desire_spacing_nested) & desire_limiting == 0, desire_spacing_nested := 0]
    
    # create desire_later
    dt[, desire_later := ifelse(desire_limiting == 1 | desire_spacing == 1, 1, 0)]
  }

  
  # CONTRACEPTIVE USE -------------------------------------------------
  message("||---Contraceptive use")
  
  # MISSING FROM CMR 2000 AND RWA 2000
  if (grepl("MICS[3-6]", survey)) {
    
    # currently using any contraception
    dt[, any_contra := ifelse(cp2 == 1, 1, ifelse(cp2 %in% c(2,8), 0, NA))]
    dt[curr_preg == 1 & is.na(any_contra), any_contra := 0]   # catch currently pregnant gateway
    
    # never used contraception
    if (grepl("MICS[5-6]", survey)) {
      dt[, never_used_contra := ifelse(grepl("MICS5", survey), cp2a, cp3)]
      dt[, never_used_contra := ifelse(never_used_contra == 1, 0, ifelse(never_used_contra == 2, 1, NA))]
      dt[any_contra == 1, never_used_contra := 0]
    }

    # current method in use
    if (grepl("MICS3", survey)) {
      if("cp3x" %in% names(dt)) dt[, current_method := ifelse(cp3x == "x", "other", NA)]
      if("cp3m" %in% names(dt)) dt[, current_method := ifelse(cp3m == "m", "withdrawal", current_method)]
      if("cp3l" %in% names(dt)) dt[, current_method := ifelse(cp3l == "l", "rhythm", current_method)]
      if("cp3k" %in% names(dt)) dt[, current_method := ifelse(cp3k == "k", "lactational_amenorrhea_method", current_method)]
      if("cp3j" %in% names(dt)) dt[, current_method := ifelse(cp3j == "j", "foam_jelly_sponge", current_method)]
      if("cp3i" %in% names(dt)) dt[, current_method := ifelse(cp3i == "i", "diaphragm", current_method)]
      if("cp3h" %in% names(dt)) dt[, current_method := ifelse(cp3h == "h", "female_condom", current_method)]
      if("cp3g" %in% names(dt)) dt[, current_method := ifelse(cp3g == "g", "male_condom", current_method)]
      if("cp3f" %in% names(dt)) dt[, current_method := ifelse(cp3f == "f", "implants", current_method)]
      if("cp3e" %in% names(dt)) dt[, current_method := ifelse(cp3e == "e", "injections", current_method)]
      if("cp3d" %in% names(dt)) dt[, current_method := ifelse(cp3d == "d", "iud", current_method)]
      if("cp3c" %in% names(dt)) dt[, current_method := ifelse(cp3c == "c", "pill", current_method)]
      if("cp3b" %in% names(dt)) dt[, current_method := ifelse(cp3b == "b", "male_sterilization", current_method)]
      if("cp3a" %in% names(dt)) dt[, current_method := ifelse(cp3a == "a", "female_sterilization", current_method)]
    } else if (grepl("MICS[4-5]", survey)) {
      if("cp3x" %in% names(dt)) dt[, current_method := ifelse(cp3x == "x", "other", NA)]
      if("cp3m" %in% names(dt)) dt[, current_method := ifelse(cp3m == "m", "withdrawal", current_method)]
      if("cp3l" %in% names(dt)) dt[, current_method := ifelse(cp3l == "l", "rhythm", current_method)]
      if("cp3k" %in% names(dt)) dt[, current_method := ifelse(cp3k == "k", "lactational_amenorrhea_method", current_method)]
      if("cp3j" %in% names(dt)) dt[, current_method := ifelse(cp3j == "j", "foam_jelly_sponge", current_method)]
      if("cp3i" %in% names(dt)) dt[, current_method := ifelse(cp3i == "i", "diaphragm", current_method)]
      if("cp3h" %in% names(dt)) dt[, current_method := ifelse(cp3h == "h", "female_condom", current_method)]
      if("cp3g" %in% names(dt)) dt[, current_method := ifelse(cp3g == "g", "male_condom", current_method)]
      if("cp3f" %in% names(dt)) dt[, current_method := ifelse(cp3f == "f", "pill", current_method)]
      if("cp3e" %in% names(dt)) dt[, current_method := ifelse(cp3e == "e", "implants", current_method)]
      if("cp3d" %in% names(dt)) dt[, current_method := ifelse(cp3d == "d", "injections", current_method)]
      if("cp3c" %in% names(dt)) dt[, current_method := ifelse(cp3c == "c", "iud", current_method)]
      if("cp3b" %in% names(dt)) dt[, current_method := ifelse(cp3b == "b", "male_sterilization", current_method)]
      if("cp3a" %in% names(dt)) dt[, current_method := ifelse(cp3a == "a", "female_sterilization", current_method)]
    } else {
      if("cp4x" %in% names(dt)) dt[, current_method := ifelse(cp4x == "x", "other", NA)]
      if("cp4m" %in% names(dt)) dt[, current_method := ifelse(cp4m == "m", "withdrawal", current_method)]
      if("cp4l" %in% names(dt)) dt[, current_method := ifelse(cp4l == "l", "rhythm", current_method)]
      if("cp4k" %in% names(dt)) dt[, current_method := ifelse(cp4k == "k", "lactational_amenorrhea_method", current_method)]
      if("cp4j" %in% names(dt)) dt[, current_method := ifelse(cp4j == "j", "foam_jelly_sponge", current_method)]
      if("cp4i" %in% names(dt)) dt[, current_method := ifelse(cp4i == "i", "diaphragm", current_method)]
      if("cp4h" %in% names(dt)) dt[, current_method := ifelse(cp4h == "h", "female_condom", current_method)]
      if("cp4g" %in% names(dt)) dt[, current_method := ifelse(cp4g == "g", "male_condom", current_method)]
      if("cp4f" %in% names(dt)) dt[, current_method := ifelse(cp4f == "f", "pill", current_method)]
      if("cp4e" %in% names(dt)) dt[, current_method := ifelse(cp4e == "e", "implants", current_method)]
      if("cp4d" %in% names(dt)) dt[, current_method := ifelse(cp4d == "d", "injections", current_method)]
      if("cp4c" %in% names(dt)) dt[, current_method := ifelse(cp4c == "c", "iud", current_method)]
      if("cp4b" %in% names(dt)) dt[, current_method := ifelse(cp4b == "b", "male_sterilization", current_method)]
      if("cp4a" %in% names(dt)) dt[, current_method := ifelse(cp4a == "a", "female_sterilization", current_method)]
    }
    
    # if not using any contra, mark current_method as none
    dt[any_contra == 0, current_method := "none"]
    
    # combine condom categories
    dt[current_method %in% c("male_condom","female_condom"), current_method := "condom"]
    
    # identify modern and traditional contraceptive users
    dt[current_method %in% c("female_sterilization", "male_sterilization", "iud", "injections", "implants", "pill", "contraceptive_patch", "contraceptive_ring", "female_condom",
                             "male_condom", "diaphragm", "foam_jelly_sponge", "emergency_contraception", "other_modern_method"), mod_contra := 1]
    dt[current_method %in% c("lactational_amenorrhea_method", "rhythm", "calendar_methods", "withdrawal", "other_traditional_method"), trad_contra := 1]
    
    # unless specifically marked as missing/no response, assume women were asked and should therefore be in denominator
    dt[(!is.na(current_method) | !is.na(any_contra)) & is.na(mod_contra), mod_contra := 0]
    dt[(!is.na(current_method) | !is.na(any_contra)) & is.na(trad_contra), trad_contra := 0]
    
    # create mcpr
    dt[, mcpr := mod_contra]
  }


  # NEED FOR CONTRACEPTION ------------------------------------------
  message("||---Need for contraception")
  
  # Women who have have a need for contraceptives are women who:
  # 1. have had sex in the last 30 days or are married/in union
  # 2. said that they do not want a child in the next 2 years
  # 3. have not hit menopause, had a hysterectomy, or are otherwise known to be
  #    infecund (including having never menstruated or not having menstruated
  #    in at least 6 months if not postpartum amenorrheic, or having been postpartum
  #    amenorrheic for 5 or more years)
  # 4. For pregnant women and women who are postpartum amenorrheic from a birth in
  #    the last 2 years, need is determined separately based on whether they wanted
  #    to space or limit their current/most recent pregnancy
  
  # first, assume no need for contraception
  dt[, need_contra := 0]
  
  # 1. have had sex in the last 30 days or are married/in union
  # 2. said that they do not want a child in the next 2 years
  # set need_contra to 1 if they have had sex in the last 30 days or are married/in union and do not want a child in the next 2 years
  if ("desire_later" %in% names(dt)) {
    dt[curr_cohabit == 1 & desire_later == 1, need_contra := 1]
    if ("rec_sex_activity" %in% names(dt)) {
      dt[curr_cohabit == 0 & rec_sex_activity == "Active in last 4 weeks" & desire_later == 1, need_contra := 1]
    }
  }
  
  # 3. have not hit menopause, had a hysterectomy, or are otherwise known to be
  #    infecund (including having never menstruated or not having menstruated
  #    in at least 6 months if not postpartum amenorrheic, or having been postpartum
  #    amenorrheic for 5 or more years)
  
  # determine infecundity and set need_contra to 0 if infecund
  if (grepl("MICS[4-6]", survey)) {
    
    # set infecund to 0 to begin with
    dt[, infecund := 0]
    
    # extract timing of last menstruation, convert to months
    dt[, last_menses_unit := ifelse(grepl("MICS[4-5]", survey), un13u, un14u)]
    dt[, last_menses_timing := ifelse(grepl("MICS[4-5]", survey), un13n, un14n)]
    dt[last_menses_unit == 1, last_menses_months := last_menses_timing/30]   # days
    dt[last_menses_unit == 2, last_menses_months := last_menses_timing/4.3]   # weeks
    dt[last_menses_unit == 3, last_menses_months := last_menses_timing]    # months
    dt[last_menses_unit == 4, last_menses_months := last_menses_timing*12]    # years
    
    # set infecund to 1 if answers to last menstruation:
    # i) has not menstruated for at least six months
    dt[last_menses_months >= 6, infecund := 1]   
    # ii) has never menstruated
    if (grepl("MICS[4-5]", survey)) dt[last_menses_timing == 96, infecund := 1]
    if (grepl("MICS6", survey))  dt[last_menses_timing == 95, infecund := 1]
    # iii) had last menstruation occurring before last birth AND last birth 5+ years ago
    if (grepl("MICS[4-5]", survey)) dt[last_menses_timing ==95 & (cmc_interview_date - cmc_last_child >= 60), infecund := 1]
    if (grepl("MICS6", survey))  dt[last_menses_timing == 94 & (cmc_interview_date - cmc_last_child >= 60), infecund := 1]
    # iv) is in menopause/has had hysterectomy  
    if (grepl("MICS[4-5]", survey)) dt[last_menses_timing == 94, infecund := 1]
    if (grepl("MICS6", survey))  dt[last_menses_timing == 93, infecund := 1]
    
    # set infecund to 1 if answers to why not able to get physically pregnant:
    # i) has had hysterectomy
    if (grepl("MICS[4-5]", survey)) dt[un11d == "d", infecund := 1]
    if (grepl("MICS6", survey)) dt[un12d == "d", infecund := 1]
    # ii) has never menstruated
    if (grepl("MICS[4-5]", survey)) dt[un11c == "c", infecund := 1]
    if (grepl("MICS6", survey)) dt[un12c == "c", infecund := 1]
    # iii) is menopausal
    if (grepl("MICS[4-5]", survey)) dt[un11b == "b", infecund := 1]
    if (grepl("MICS6", survey)) dt[un12b == "b", infecund := 1]
    # iv) has been trying to get pregnant for at least 2 years without result
    if (grepl("MICS[4-5]", survey)) dt[un11e == "e", infecund := 1]
    if (grepl("MICS6", survey)) dt[un12e == "e", infecund := 1]
    
    # set infecund to 1 if answers to desire for future birth:
    # i) cannot get pregnant
    dt[fert_pref_notpreg == 3, infecund := 1]
    
    # set infecund to 1 if has not had a birth in the preceding 5 years, has never used contraception (or is not using if never not avail),
    # and is currently married and was continuously married during the preceding 5 years (can only determine if in first marriage/union)
    if ("never_used_contra" %in% names(dt)) {
      dt[curr_cohabit == 1 &                                             # currently married/in-union
           never_used_contra == 1 &                                      # never used contraception
           in_first_cohabit == 1 &                                       # in first marriage/union
           cmc_interview_date - cmc_last_child >= 60 &                   # no births in last 5 years
           cmc_interview_date - cmc_first_cohabit >= 60, infecund := 1]  # been married continuously in last 5 years
    } else {
      dt[curr_cohabit == 1 &                                             # currently married/in-union
           any_contra == 0 &                                             # currently not using contraception
           in_first_cohabit == 1 &                                       # in first marriage/union
           cmc_interview_date - cmc_last_child >= 60 &                   # no births in last 5 years
           cmc_interview_date - cmc_first_cohabit >= 60, infecund := 1]  # been married continuously in last 5 years
    }
 
    # also pregnant women are not infecund even though their last menses may have been >6 months ago
    dt[curr_preg == 1, infecund := 0]
    
    # if infecund, set need_contra to 0
    dt[infecund == 1, need_contra := 0]
  }
  
  # 4. For pregnant women and women who are postpartum amenorrheic from a birth in
  #    the last 2 years, need is determined separately based on whether they wanted
  #    to space or limit their current/most recent pregnancy
  
  # determine need among pregnant women
  if (grepl("MICS[4-6]", survey)) {
  
    # pregnant women are assumed to not need contraception as they are not at risk for pregnancy
    dt[curr_preg == 1, need_contra := 0]
  
    # extract desire for current pregnancy
    dt[, preg_not_wanted := ifelse(un2 == 2, 1, ifelse(un2 == 1 ,0, NA))]
    
    # set need_contra to 1 if pregnant women wanted to space/limit their pregnancy
    dt[curr_preg == 1 & preg_not_wanted == 1, need_contra := 1]
  }
  
  # determine need among postpartum amenorrheic women
  if (grepl("MICS[4-6]", survey)) {
    
    # menstrual period returned since birth of child
    if (grepl("MICS[4-5]", survey)) dt[, menses_not_returned := ifelse(mn23 == 2, 1, ifelse(mn23 == 1, 0, NA))]
    if (grepl("MICS6", survey)) dt[, menses_not_returned := ifelse(mn35 == 2, 1, ifelse(mn35 == 1, 0, NA))]
    # last menstruation before last birth in last 5 years
    if (grepl("MICS[4-5]", survey)) dt[(cmc_interview_date - cmc_last_child < 60) & last_menses_timing == 95, menses_not_returned := 1]
    if (grepl("MICS6", survey)) dt[(cmc_interview_date - cmc_last_child < 60) & last_menses_timing == 94, menses_not_returned := 1]
    
    # a woman is postpartum amenorrheic if she had a live birth in last two years and is not currently pregnant, and her menstrual
    # period has not returned since the birth of the last child.
    dt[any_births_2_yr == 1 & curr_preg == 0 & menses_not_returned == 1, ppa := 1]
    
    # set need_contra to 1 if ppa women wanted to space/limit their most recent pregnancy
    dt[ppa == 1 & desire_child_then == 2, need_contra := 1]
  }
  
  # regardless of answers to any other questions, if a woman is currently using any contraceptive method then
  # she is considered to have a need for contraception
  if ("any_contra" %in% names(dt)) dt[any_contra == 1, need_contra := 1]
  
  # restrict need_contra to those observations where we know women were actually asked about contraception,
  # making it consistent with the denominator of modern contraceptive usage (since otherwise it's impossible
  # to know whether that need was met or not)
  if ("mod_contra" %in% names(dt)) dt[is.na(mod_contra), need_contra := NA]
  
  # if surveys are missing sexual experience, fertility preferences, or contraceptive use, we are not 
  # able to correctly calculate need, set entirely to NA
  # ONLY MICS4-6 WITH SEXUAL EXPERIENCE DATA WILL REMAIN
  if (!all(c("rec_sex_activity", "desire_later", "any_contra") %in% names(dt))) dt[, need_contra := NA]
  
  
  
  # DEMAND SATISFIED/UNMET NEED -------------------------------------
  message("||---Demand satisfied/unmet need")
  
  if (grepl("MICS[3-6]", survey)) {
    
    # unmet need = women with a need for contraception who are NOT using ANY contraception
    dt[, unmet_need := ifelse(any_contra == 0 & need_contra == 1, 1, ifelse(!is.na(need_contra), 0, NA))]
    
    # unmet need mod = women with a need for contraception who are NOT using MODERN contraception
    dt[, unmet_need_mod := ifelse(mod_contra == 0 & need_contra == 1, 1, ifelse(!is.na(need_contra), 0, NA))]
    
    # demand satisfied = women with a need for contraception who are using MODERN contraception
    dt[, demand_satisfied := ifelse(need_contra == 1, mod_contra, NA)]
  }
  
  
  # EXPORT CLEANED DATA --------------------------------------------
  message("||---Export cleaned data")
  
  # filter to variables of interest
  vars_interest <- c("survey","country","cluster","hh_id","id","area_unit","urban","year","month","cmc_interview_date",
                     "wpweight","pweight","hhid_unique","id_unique","ethnicity","ethnicity_wm","curr_preg","ever_birth",
                     "any_births_2_yr","cmc_first_child","cmc_last_child","number_births","any_birth_preg","age",
                     "cmc_woman_dob","age_1st_birth","marital_status","curr_cohabit","former_cohabit","ever_cohabit","in_first_cohabit",
                     "cmc_first_cohabit","age_first_cohabit","edu_level_categ_wn","ever_attend_school",
                     "beating_just", "fgm","age_1st_sex_imp","never_had_intercourse","last_sex_unit","last_sex_timing",
                     "rec_sex_activity","gap_sex_mar","desire_child_then","desire_child_later","fert_pref_preg", 
                     "fert_pref_notpreg","fertility_pref","desire_unit","desire_timing","desire_child_teen","desire_spacing",
                     "desire_limiting","desire_soon","desire_spacing_nested","desire_later","any_contra","never_used_contra",
                     "current_method","mod_contra","trad_contra","mcpr","need_contra","infecund","last_menses_unit", 
                     "last_menses_timing","last_menses_months","preg_not_wantd","menses_not_returned","ppa","unmet_need","unmet_need_mod",
                     "demand_satisfied")

  cur_vars <- names(dt)[names(dt) %in% vars_interest]
  dt_filter <- dt[, ..cur_vars]
  
  # drop observations which did not actually complete the interview
  dt_filter <- dt_filter[wpweight != 0]
  
  # save the cleaned data
  file_name <- gsub("\\.DTA", "", basename(survey))
  output_file_path <- file.path(out.dir, paste0(file_name, "_wn_extract.csv"))
  write.csv(dt_filter, output_file_path, row.names = FALSE)
  
  message(paste0("Successfully saved ", file_name))
}


# RUN EXTRACTIONS ---------------------------------------------------

# Cameroon
extract_data("/FILEPATH/CMR_MICS2_2000_WN.DTA", "cm")
extract_data("/FILEPATH/CMR_MICS3_2006_WN.DTA", "cm")
extract_data("/FILEPATH/CMR_MICS5_2014_WN.DTA", "cm")

# Nepal
extract_data("/FILEPATH/NPL_MICS4_2010_WN.DTA", "np")
extract_data("/FILEPATH/NPL_MICS5_2014_WN.DTA", "np")
extract_data("/FILEPATH/NPL_MICS6_2019_WN.DTA", "np")

# Malawi
extract_data("/FILEPATH/MWI_MICS3_2006_WN.DTA", "mw")
extract_data("/FILEPATH/MWI_MICS5_2013_2014_WN.DTA", "mw")
extract_data("/FILEPATH/MWI_MICS6_2019_2020_WN.DTA", "mw")

# Ghana
extract_data("/FILEPATH/GHA_MICS3_2006_WN.DTA", "gh")
extract_data("/FILEPATH/GHA_MICS4_2011_WN.DTA", "gh")
extract_data("/FILEPATH/GHA_MICS6_2017_2018_WN.DTA", "gh")

# Rwanda
extract_data("/FILEPATH/RWA_MICS2_2000_WN.DTA", "rw")

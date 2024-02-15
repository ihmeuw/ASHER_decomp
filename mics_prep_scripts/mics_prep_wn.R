#### Prepare MICS dataset for ASHER decomp ####
#### Editing the script to pull in the MICS survey in the J and L Drive#####
#### - Paul #####
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
pacman::p_load(magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey, tools)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source)) # IHME-specific line. comment out if running outside IHME
out.dir <- '/share/scratch/projects/hssa/asher/processed/'

extract_data <- function(survey, cur_country) {
dt <- data.table(read_dta(file.path(l, survey)))
#### SURVEY CHARACTERISTICS ####
# no ID variable in the dataset; need to compute ourselves
names(dt) <- tolower(names(dt))

# Convert only character columns to lowercase
dt[] <- lapply(dt, function(x) if(is.character(x)) tolower(x) else x)
dt[, cluster := hh1]
dt[, hh_id := hh2]
dt[, id := wm3]
dt[, area_unit := hh6]
dt[, country := cur_country]

#This variable is not in CMR 2014 survey
dt[, wpweight := wmweight]
dt[, pweight := wpweight/1000000]

#This variable is not in CMR 2000, CMR 2006, GHA 2006 survey
dt[, psu := psu]
dt[, strata := stratum]
dt[, region := hh7]
dt[, year := wm6y]

#unique cluster id for each nid
dt[, cluster_unique := paste( cluster)]

# unique psu for each nid
dt[, psu_unique := paste(psu)]

# unique hh id for each nid
dt[, hhid_unique := paste( hh_id, cluster)]

# unique individual id for each nid
dt[, id_unique := as.integer(id)]


#### HOUSEHOLD CHARACTERISTICS ####

dt[, urban := ifelse(hh6 == 1, 1, 0)]

# "ethine" in CMR surveys
dt[, ethnicity := ethnicity]

#### outcome-related variables ####
#### pregnancy history ####
dt[, curr_preg := cp1] # Whether the respondent is currently pregnant
dt[, ever_preg := ifelse(curr_preg == 1, 1, 0)]
dt[, ever_birth := cm1] # Whether the respondent has ever given birth (does not include stillbirths)
dt[, births_2_yr := cm17] # one or more live births in the last 2 years
dt[, cmc_first_child := wdobfc] # DOB of first child (CMC)
dt[, number_births := ceb] # Number of children ever born

#### INDIVIDUAL CHARACTERISTICS ####

# Age
dt[, age := wb4]
dt[, cmc_women_dob := wdob]
dt[, cmc_interview_date := wdoi]

#Marriage
dt[, cmc_first_cohabit := wdom ]
dt[, age_first_cohabit := wagem] # age of first marriage/union

#Literacy
dt[, no_read := ifelse(wb14 == 0, 1, 0) ]
dt[wb14==3, no_read := NA]
dt[wb14 == 4, no_read:= NA]

# Education
dt[, highest_ed_level := welevel]

# Attended school during current school year 
dt[, attend_school := ifelse(wb9 ==1,1,
                             ifelse(wb9 == 2, 0, 0))]
dt[, attend_school := ifelse(is.na(attend_school), 0, attend_school)]

#AGE at first sex
# 0 means have not had sex
dt[, age_1st_sex_imp := sb1] # many report that it is at the time of cohabitation, so need to impute those responses here


dt[, never_had_intercourse := ifelse(age_1st_sex_imp ==0,1,
                                     ifelse(age_1st_sex_imp >0 &age_1st_sex_imp != 99,0,NA ))]

## need to fix time of cohabitation
dt[age_1st_sex_imp == 95, age_1st_sex_imp:= ma11]

## fix 99's as no response
dt[age_1st_sex_imp == 99, age_1st_sex_imp:=NA]

## make 0 (never had intercourse) NA
dt[age_1st_sex_imp == 0,age_1st_sex_imp:= NA]

dt[, no_sex := ifelse(age_1st_sex_imp == 0, 1, 0)]

# Fertility preferences --> need to create a combined fert_pref variable for pregnant and non-pregnant respondents
dt[, desire_child_then := db2] # wanted last child then
dt[, desire_child_later := db4] # wanted last child later or not at all
dt[, fert_pref_preg := un5] # respondents who are currently pregnant
dt[, fert_pref_notpreg := un7] # respondents who are currently not pregnant
dt[, fertility_pref := ifelse(!is.na(dt$fert_pref_preg),
                         dt$fert_pref_preg,
                         dt$fert_pref_notpreg)] 

## create desire child teen
dt[, desire_unit:=un8u] ## could be un7u in some years: CHECK 
dt[, desire_value:=un8n] ## could be un7n in some years: CHECK 

## for MWI 2019-20: we have un8u/un8n and desire soon/now is 93

dt[, desire_child_teen:=ifelse(age==19 & fertility_pref==1 & (desire_unit== 93 | (un8u == 2 & un8n == 0) | (un8n == 1 &  un8n <12) ), 1,
                               ifelse(age==15 & fertility_pref==1 & (desire_unit == 93 | (un8u == 2 & un8n <5) | (un8n == 1 &  un8n <60)), 1,
                                      ifelse(age==16 & fertility_pref==1 & (desire_unit == 93 | (un8u == 2 & un8n <4) | (un8n == 1 &  un8n <48)), 1,
                                             ifelse(age==17 & fertility_pref==1 & (desire_unit == 93 | (un8u == 2 & un8n <3) | (un8n == 1 &  un8n <36)), 1,
                                                    ifelse(age==18 & fertility_pref==1 & (desire_unit == 93 | (un8u == 2 & un8n <2) | (un8n == 1 &  un8n <24)),1,0)))))]

# contraception
dt[, any_contra:= cp2]
dt[, contra_method:=ifelse((cp4a ==1 | cp4b ==1 | cp4c ==1 | cp4d ==1 | cp4e ==1 | cp4f ==1 | cp4g ==1 | cp4h ==1 | cp4i ==1 | cp4j ==1), 1, 0)]


# FGM
if("fg3" %in% names(dt)){
  dt[, fgm:=fg3]
}

#### INDIVIDUAL CHARACTERISTICS GV LIST ####
dt[, marital_status := mstatus]
dt[, ever_married:=ma5]
dt[, fp_exp_newsp:=ifelse(mt1 == 0, 0, 1)]
dt[, fp_exp_radio:=ifelse(mt2 == 0, 0, 1)]
dt[, fp_exp_tv:=ifelse(mt3 == 0, 0, 1)]

dt[, fp_exp_media:=ifelse(rowSums(!is.na(.SD), na.rm=TRUE) == 0, NA,
                          ifelse(rowSums(.SD == 1, na.rm=TRUE) >= 1, 1, 0)),
   .SDcols = c("fp_exp_radio", "fp_exp_tv", "fp_exp_newsp")]

# unmet need to be computed from Unmet Need section

####################################################################################################
##### FERTILITY PREFERENCES #######################################################################
####################################################################################################
#test <-as.data.table(read_dta('/Volumes/snfs/DATA/UNICEF_MICS/MWI/2013_2014/MWI_MICS5_2013_2014_WN_Y2015M08D14.DTA'))
#dt <- copy(test)
#names(dt) <- tolower(names(dt)) # some country-years have capital letter while others do not
#dt[] <- lapply(dt, tolower) ##make all values lower case for easier string comparison


dt[, desire_child := un7] # changed from un6; do we also check un5 for pregnant women?
dt[, desire_unit := un8u] # changed from un7
dt[, desire_timing := un8n] # changed from un7

dt[, desire_limiting := as.numeric(NA)]
dt[, desire_spacing := as.numeric(NA)]
dt[, desire_limiting := ifelse(un7 == 2, 1, desire_limiting)]
dt[, desire_timing := un8n]
dt[desire_timing >= 99, desire_timing := NA]
dt[(desire_child == 8 | desire_timing >= 998 | (desire_unit == 1 & desire_timing >= 24) | (desire_unit == 2 & desire_timing >= 2)), desire_spacing := 1]

# desire spacing var that is nested of women who do want limiting
dt[, desire_spacing_nested := desire_spacing]
dt[desire_limiting == 1, desire_spacing_nested := as.numeric(NA)]

dt[is.na(desire_limiting), desire_limiting := 0]
dt[is.na(desire_spacing), desire_spacing := 0]
dt[is.na(desire_spacing_nested) & desire_limiting == 0, desire_spacing_nested := 0]

# update desire_later
dt[desire_limiting == 1, desire_later := 1]
dt[desire_spacing == 1, desire_later := 1]

# create desire soon
if ("un7n" %in% names(dt)){
  dt[, desire_soon := ifelse(un7n ==993, 1,0)]
}

####################################################################################################
##### mCPR #######################################################################
####################################################################################################
if("cp4x" %in% names(dt)){
  dt[cp4x == "x", current_method := "other"]
}
if("cp4m" %in% names(dt)){
  dt[cp4m == "m", current_method := "withdrawal"]
}
if("cp4l" %in% names(dt)){
  dt[cp4l == "l", current_method := "rhythm"]
}
if("cp4k" %in% names(dt)){
  dt[cp4k == "k", current_method := "lactational_amenorrhea_method"]
}
if("cp4j" %in% names(dt)){
  dt[cp4j == "j", current_method := "foam_jelly_sponge"]
}
if("cp4i" %in% names(dt)){
  dt[cp4i == "i", current_method := "diaphragm"]
}
if("cp4g" %in% names(dt)){
  dt[cp4g == "g", current_method := "condom"] # male condom
}
if("cp4h" %in% names(dt)){
  dt[cp4h == "h", current_method := "condom"] #female condom
}
if("cp4f" %in% names(dt)){
  dt[cp4f == "f", current_method := "pill"]
}
if("cp4e" %in% names(dt)){
  dt[cp4e == "e", current_method := "implants"]
}
if("cp4d" %in% names(dt)){
  dt[cp4d == "d", current_method := "injections"]
}
if("cp4c" %in% names(dt)){
  dt[cp4c == "c", current_method := "iud"]
}
if("cp4b" %in% names(dt)){
  dt[cp4b == "b", current_method := "male_sterilization"]
}
if("cp4a" %in% names(dt)){
  dt[cp4a == "a", current_method := "female_sterilization" ]
}

dt[cp2 ==2, current_method := "none"]

## identify modern and traditional contraceptive users
dt[current_method %in% c("female_sterilization", "male_sterilization", "iud", "injections", "implants", "pill", "contraceptive_patch", "contraceptive_ring", "condom",
                         "diaphragm", "foam_jelly_sponge", "emergency_contraception", "other_modern_method"), mod_contra := 1]

dt[current_method %in% c("lactational_amenorrhea_method", "rhythm", "calendar_methods", "withdrawal", "other_traditional_method"), trad_contra := 1]

## unless specifically marked as missing/no response, assume women were asked and should therefore be in denominator
dt[(!is.na(current_method) | !is.na(cp3))& is.na(mod_contra), mod_contra := 0]
dt[(!is.na(current_method) | !is.na(cp3))& is.na(trad_contra), trad_contra := 0]

## If women are using any method, mark that
dt[!is.na(mod_contra), any_contra := 0]
dt[trad_contra == 1 | mod_contra == 1, any_contra := 1]

dt[, mcpr := mod_contra]

####################################################################################################
##### NEED FOR CONTRACEPTION #######################################################################
####################################################################################################

## Note: for MICS, sex in the last month not in data

## Women who have have a need for contraceptives are women who:
## 1. have had sex in the last 30 days or are married/in union
## 2. said that they do not want a child in the next 2 years
## 3. have not hit menopause, had a hysterectomy, or are otherwise known to be
##    infecund (including having never menstruated or not having menstruated
##    in at least 6 months if not postpartum amenorrheic, or having been postpartum
##    amenorrheic for 5 or more years)
## 4. have not been continuously married/living with a man for 5 years without
##    having a child despite answering that they have never used any method of
##    contraception.
## 5. For pregnant women and women who are postpartum amenorrheic from a birth in
##    the last 2 years, need is determined separately based on whether they wanted
##    to space or limit their current/most recent pregnancy


## first, assume no need for contraception
dt[, need_contra := 0]

## current cohabit status
dt[, curr_cohabit := ifelse(mstatus ==1, 1,
                            ifelse(mstatus %in% c(2,3),0,NA))]

if ("desire_later" %in% names(dt)) {
  ## Survey has information on women's desire for a child in the next 2 years
  dt[curr_cohabit == 1 & desire_later == 1, need_contra := 1]
} else if ("desire_soon" %in% names(dt)) {
  ## Survey only has info regarding desire for a child right now. Assume that anyone who is married/sexually active
  ## and did not answer that they want a child right now has a need
  dt[curr_cohabit == 1 & (is.na(desire_soon) | desire_soon == 0), need_contra := 1]

} else {
  ## No info regarding desire for children, assume that everyone who's married/sexually active has a need
  if ("curr_cohabit" %in% names(dt)) dt[curr_cohabit == 1,need_contra := 1]
}
## Now we exclude women from having a need for contraception if they are pregnant, postpartum amenorrheic, have expressed that
## they are infertile, or can be assumed to be infertile based on their recent menstruation or lack of children
## after 5 years of marriage with no contraceptive usage

# Indicated lack of menstruation (in any way)
## NOTE: the response numbers for this variable, start of last menstrual period (number), either un13n or un14n, will vary by country-year
## we want to capture anything that indiciates lack of menstruation
## often, "in menopause/had hysterectomy" and "never mensturated"

if("un14n" %in% names(dt)){
  dt[, no_menses := ifelse(un14n %in% c(93,95),1, 0)] ## example here was for MWI 2013-2014 (AS note: updated to 93 and 95 for "in menopause/had hysterectomy" and "never mensturated" resp.)
}

if("un13n" %in% names(dt)){
  dt[, no_menses := ifelse(un13n %in% c(94,95),1, 0)] ## AS note: this is not in Malawi MICS. Use un14n above
}

if ("no_menses" %in% names(dt)) dt[no_menses == 1, infecund := 1]

if("un7" %in% names(dt)){
  dt[, desire_children_infertile := ifelse(un7 ==3, 1,
                                           ifelse(un7 ==1, 0, NA))]
}

if("un6" %in% names(dt)){
  dt[, desire_children_infertile := ifelse(un6 ==3, 1,
                                           ifelse(un6 ==1, 0, NA))]
}

# Indicated inability to have a child
if ("desire_children_infertile" %in% names(dt)) dt[desire_children_infertile == 1, infecund := 1]

## Indicated lack of menstruation or inability to have a child as reason why she was not using a contraceptive method

if ("un12b" %in% names(dt)){ # menopausal
  dt[un12b == 'b', infecund := 1]
}
if ("un12c" %in% names(dt)){ # never menstruated
  dt[un12c == 'c', infecund := 1]
}

if ("un12d" %in% names(dt)){ # hysterectomy
  dt[un12d == 'd', infecund := 1]
}

if ("un12e" %in% names(dt)){ #trying to get pregnant for 2 years without result
  dt[un12e == 'e', infecund := 1]
}

## extract never used contra
if ("cp3" %in% names(dt)){
  dt[, never_used_contra := ifelse(cp3 ==2, 1,
                                   ifelse(cp3 == 1, 0, NA))]
}
if ("cp2" %in% names(dt)){
  dt[, never_used_contra := ifelse(cp2 ==2, 1,
                                   ifelse(cp2 == 1, 0, NA))]
}

# extract in first cohabit
dt[, in_first_cohabit := ifelse(ma5 ==1,1,
                                ifelse(ma5 == 2, 0, NA))]

# extract last_birth_year ## AS note: not sure how to correct this
if("cm12y" %in% names(dt)){
  dt[, last_birth_year := cm12y]
}
if("bh4y_last" %in% names(dt)){
  dt[, last_birth_year := bh4y_last]
}

# extract interview_year
dt[, year := wm6y]

## Exclude others from need based on assumed infertility after 5 years without a child. Relevant variables
## coded differently depending on the survey series)
if (all(c("curr_cohabit","never_used_contra") %in% names(dt))) {
  if (all(c("in_first_cohabit","first_cohabit_year","last_birth_year","interview_year") %in% names(dt))) {
    dt[curr_cohabit == 1 & ##currently married/in union
         never_used_contra == 1 & ##never used contraception
         in_first_cohabit == 1 & ##still in first marriage/union
         interview_year < 9990 & ##make sure interview year is known
         interview_year - first_cohabit_year > 5 & ##been together for 5 years
         (interview_year - last_birth_year > 5 | ##5 years since last birth or never given birth
            is.na(last_birth_year)),
       infecund := 1]
  }

}

# extract pregnant
dt[, pregnant := cp1]

## pregnant women are not infecund even though their last menses may have been >6 months ago
dt[pregnant == 1, infecund := 0]

## any woman that is infecund does NOT have a need for contraception; skip if survey will be crosswalked for missing fecundity
dt[infecund == 1, need_contra := 0]

####################################################################################################
##### PREGNANT AND POST-PARTUM AMENORRHEIC WOMEN ###################################################
####################################################################################################

## Pregnant and postpartum amenorrheic women from a birth in the last 2 years can still contribute
## to unmet demand for contraception if they indicate that they wanted to space or limit their
## current/most recent pregnancy

## pregnant women are assumed to not need contraception as they are not at risk for pregnancy
dt[pregnant == 1, need_contra := 0]

# extract preg_not_wanted
dt[, preg_not_wanted := ifelse(un2 == 2, 1,
                               ifelse(un2 ==1 ,0, NA))]

## Pregnant women who had a need
if ("preg_not_wanted" %in% names(dt)) dt[pregnant == 1 & preg_not_wanted == 1, need_contra := 1]

# extract ppa
dt[, preg_not_wanted := ifelse(un2 == 2, 1,
                               ifelse(un2 ==1 ,0, NA))]
# extract birth_in_last_two_years
if("cm12y" %in% names(dt)){
  dt[, birth_in_last_two_years := ifelse(cm13 == 1, 1,
                                         ifelse(cm12y ==0 ,0, NA))]
}
if("cm13" %in% names(dt)){
  dt[, birth_in_last_two_years := ifelse(cm13 == 1, 1,
                                         ifelse(cm13 ==0 ,0, NA))]
}
if("cm17" %in% names(dt)){
  dt[, birth_in_last_two_years := ifelse(cm17 == 1, 1,
                                         ifelse(cm17 ==0 ,0, NA))]
}

## Determine which postpartum amenorrheic women gave birth within the last 2 years
if (all(c("ppa","birth_in_last_two_years") %in% names(dt))) dt[ppa == 1 & birth_in_last_two_years == 1, ppa24 := 1]

## postpartum amenorrheic are asssumed to not be using/need contraception as their periods have not returned
if ("un12f" %in% names(dt)) {
  dt[un12f == 1 & is.na(mod_contra), mod_contra := 0]
  dt[un12f == 1 & is.na(trad_contra), trad_contra := 0]
  dt[un12f == 1 & is.na(any_contra), any_contra := 0]
  dt[un12f == 1, need_contra := 0]
}

## Designate which postpartum amenorrheic women had a need, and adjust the all-woman contraception denominator to match (in case
## postpartum women were not asked about contraception, similar to pregnant women)
if (all(c("un12f","ppa_not_wanted") %in% names(dt))) dt[un12f == 1 & ppa_not_wanted == 1, need_contra := 1]


####################################################################################################
##### NEED FOR CONTRACEPTION MET WITH MODERN METHODS ###############################################
####################################################################################################

## Restrict need_contra to those observations where we know women were actually asked about contraception,
## making it consistent with the denominator of modern contraceptive usage (since otherwise it's impossible
## to know whether that need was met or not)
dt[is.na(mod_contra), need_contra := NA]

## create variable for proportion of users where definition of need actually captured their need before we add those who
## are assumed to have a need because of contraception use (i.e. proportion of women with need where need was determined
## by definition, not by contraceptive use). BUT need to subset to non-sterilized women/couples because in such cases sterilized
## people retain "need" (through use) even as context/demand changes (need_contra may have been marked as 0 from infecundity)
dt[,need_prop := ifelse(any_contra == 1 & current_method != "male_sterilization" & current_method != "female_sterilization", need_contra, NA)]

## regardless of answers to any other questions, if a woman is currently using any contraceptive method then
## she is considered to have a need for contraception
dt[any_contra == 1, need_contra := 1]

## non-users that have an (unmet) need
dt[,unmet_need := ifelse(any_contra == 0, need_contra, NA)] 

## users that have an unmet need: using contraception but not using modern
dt[any_contra == 1 & !is.na(mod_contra), unmet_need := ifelse(mod_contra ==0, 1, 0)]


# Beating justified
# beating justified in any case
if ("dv1a" %in% names(dt)){
  dt[, beating_just := ifelse(dv1a == 1, 1, 0)]
  dt[dv1b ==1, beating_just:= 1]
  dt[dv1c ==1, beating_just:= 1]
  dt[dv1d ==1, beating_just:=1]
  dt[dv1e ==1, beating_just:=1]
}

# Average gap in years between median ages at first intercourse and first marriage
dt[, age_1st_mar:=ma11]
dt[age_1st_mar!=age_1st_sex_imp, gap_sex_mar:=age_1st_sex_imp-age_1st_mar]

## calculate if active in the last four weeks
dt[sb2u == 1, rec_sex_activity := ifelse(sb2n <=28, "Active in last 4 weeks", "Not active in last 4 weeks")] ## unit is days 
dt[sb2u == 2, rec_sex_activity := ifelse(sb2n <=4, "Active in last 4 weeks", "Not active in last 4 weeks")] ## unit is weeks
dt[sb2u == 3, rec_sex_activity := ifelse(sb2n <=1, "Active in last 4 weeks", "Not active in last 4 weeks")] ## unit is months
dt[sb2u == 4, rec_sex_activity := "Not active in last 4 weeks"] ## unit is years

## if never had intercourse and missing recent sexual activity, put in not active in the last 4 weeks
dt[never_had_intercourse == 1, rec_sex_activity := "Not active in last 4 weeks"] 

## construct outcome: currently pregnant or any live birth
dt[, any_birth_preg := ifelse(ever_birth ==1 | curr_preg ==1, 1,
                              ifelse(!is.na(ever_birth) & !is.na(curr_preg),0,NA)) ]

## filter just to variables of interest
vars_interest <- c( "psu","cluster","hh_id","id", "area_unit","wpweight",
                    "id","area_unit","wpweight","pweight","strata","region","year","cluster_unique","psu_unique",
                    "hhid_unique","id_unique","urban","curr_preg","ever_birth","births_2_yr",
                    "cmc_first_child","number_births","age","cmc_women_dob","cmc_interview_date","cmc_first_cohabit",
                    "age_first_cohabit","no_read","highest_ed_level","age_1st_sex_imp","no_sex","desire_child_then",
                    "desire_child_later","fert_pref_preg","fert_pref_notpreg","any_contra","contra_method","fgm",
                    "marital_status","ever_married","newsp","radio","tv", "desire_child","desire_unit" ,"desire_timing" ,
                    "desire_limiting","desire_spacing","desire_spacing_nested" ,"desire_later","current_method","mod_contra",
                    "trad_contra","mcpr","need_contra","curr_cohabit","no_menses","infecund","desire_children_infertile", 
                    "never_used_contra","in_first_cohabit","last_birth_year","pregnant","preg_not_wanted","birth_in_last_two_years",
                    "need_prop" , "unmet_prop","beating_just","age_1st_mar" ,"gap_sex_mar","rec_sex_activity","ethnicity" ,"ever_preg" ,"fertility_pref",
                    'mcpr','attend_school', 'country', 'desire_child_teen','never_had_intercourse','fp_exp_media','unmet_need', 'any_birth_preg')
cur_vars <- names(dt)[names(dt) %in% vars_interest]
dt_filter <- dt[, ..cur_vars]

## Save the cleaned data
file_name <- file_path_sans_ext(basename(survey))
output_file_path <- file.path(out.dir, paste0(file_name, "_wn_extract.csv"))
write.csv(dt_filter, output_file_path, row.names = FALSE)

}

#extract_data("IDENT/PROJECT_FOLDERS/UNICEF_MICS/GHA/2017_2018/GHA_MICS6_2017_2018_WN_Y2020M04D10.DTA", "gh")
extract_data("IDENT/PROJECT_FOLDERS/UNICEF_MICS/MWI/2019_2020/MWI_MICS6_2019_2020_WN_Y2022M01D31.DTA", "mw")

# 



# ##This section is for comparing with dhs survey.
# ##It can be left commented out -Paul
# check_dhs_wn <- fread('/share/scratch/projects/hssa/asher/processed/CMR_DHS7_2018_2019_WN_CMIR71FL_Y2020M06D10_wn_processsed.csv')
# mics_exclusive <- setdiff(names(dt), names(check_dhs_wn))
# dhs_exclusive <- setdiff(names(check_dhs_wn), names(dt_filter))
# mics_exclusive
# dhs_exclusive
# 
# 
# check_dhs_hh <- fread('/share/scratch/projects/hssa/asher/processed/CMR_DHS7_2018_2019_HH_CMHR71FL_Y2020M06D10_wn_hh_processsed.csv')
# dhs_exclusive <- setdiff(names(check_dhs_hh), names(dt_filter))
# 

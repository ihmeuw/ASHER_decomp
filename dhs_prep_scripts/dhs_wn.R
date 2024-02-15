#### Prepare datasets for ASHER decomp ####
#### Ira Martopullo ####
#### 10/12/2023 ####
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

#r <- "/Volumes/"
pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey,tibble)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source)) # IHME-specific line. comment out if running outside IHME

asher.dir<-paste0(r, "team/hs/pub/users/iramarto/asher/") # change to wherever files are stored

out.dir <- '/share/scratch/projects/hssa/asher/processed/'

files<-list.files(paste0(asher.dir, "dhs_data_dta/"))

'%ni%' <- Negate('%in%')

# create function to convert CMC date to year and month
get_cmc_year <- function(x) 1900 + as.integer((as.integer(x) - 1) / 12)
get_cmc_month <- function(x) as.integer(x) - 12 * (get_cmc_year(x) - 1900)



#df<-as.data.table(read_dta(paste0(asher.dir, "dhs_data_dta/", "CMR_DHS7_2018_2019_WN_CMIR71FL_Y2020M06D10.DTA")))
#### Individual dataset ####

## "GHA_DHS7_2014_WN_GHIR72FL_Y2018M12D10.DTA"  did not go through
## MWI_DHS4_2004_2005_WN_MWIR4EFL_Y2018M12D10.DTA did not go through
## "RWA_DHS5_2005_WN_RWIR53FL_Y2019M03D18.DTA" did not go through
wn<-files[files %like%"_WN_"]
# wn<-wn[!wn %like%"GHA_DHS7"]
# wn<-wn[!wn %like%"MWI_DHS4"]
# wn<-wn[!wn %like%"RWA_DHS5"]

## subset first last years
wn_first_last <- c("CMR_DHS4_2004_WN_CMIR44FL_Y2018M05D29.DTA", "CMR_DHS7_2018_2019_WN_CMIR71FL_Y2020M06D10.DTA",
                   "GHA_DHS4_2003_WN_GHIR4BFL_Y2018M10D15.DTA","GHA_DHS8_2022_2023_WN_GHIR8AFL_Y2024M01D22.DTA",
                   "MWI_DHS4_2000_WN_MWIR41FL_Y2019M01D07.DTA", "MWI_DHS7_2015_2016_WN_MWIR7AFL_Y2019M12D11.DTA",
                   "NPL_DHS5_2006_WN_NPIR51FL_Y2018M11D05.DTA","NPL_DHS8_2022_WN_NPIR81FL_Y2023M06D23.DTA",
                   "RWA_DHS4_2000_WN_RWIR41FL_Y2019M03D18.DTA", "RWA_DHS8_2019_2020_WN_RWIR81FL_Y2021M10D05.DTA")

# sub-setting for the in-between years
# For Paul: CMR DHS5 (2011), GHA DHS4 (1998),
# GHA DHS5 (2008), MWI DHS4 (2004-2005), MWI DHS6 (2010), RWA DHS5 (2005),
# RWA DHS6 (2010-2011), RWA DHS7 (2014-2015),
#
# For Ruby (note: jts comment out the surveys you aren't working on -Paul)
# Nepal DHS5 (2006), Nepal DHS6 (2011), Nepal DHS7 (2016-2017)
wn_iby<-wn[wn %like% "CMR_DHS5" | wn %like% "CMR_DHS7" | wn %like% "GHA_DHS4_1998" | wn %like% "GHA_DHS5"
       | wn %like% "GHA_DHS7" | wn %like% "MWI_DHS4" | wn %like% "MWI_DHS6" | wn %like% "MWI_DHS7"
       | wn %like% "RWA_DHS5" | wn %like% "RWA_DHS6" | wn %like% "RWA_DHS7"]
#wn_npl<-wn[wn %like% "NPL_DHS5" | wn %like% "NPL_DHS6" | wn %like% "NPL_DHS7"]
for(f in wn_first_last){
    df<-as.data.table(read_dta(paste0(asher.dir, "dhs_data_dta/", f)))
    #### SURVEY CHARACTERISTICS ####
    names(df) <- tolower(names(df)) # some country-years have capital letter while others do not
    message(unique(df$v000)) # keep track of code running
    df[, country :=v000]
    df[, cluster := v001]
    df[, hh_id := v002]
    df[, id := v003]
    df[, area_unit := v004]
    df[, wpweight := v005] # women's individual sample weight (6 decimals)
    df[, pweight := wpweight/1000000]
    df[, cmc_interview_date := v008]
    df[, psu := v021]
    df[, strata := v022]
    df[, region := v024]
    # d$sdist  --> district already coded
    if(unique(df$country)%like%"NP"){
      df[, year:=v007-57]
    } else {
      df[, year := v007]
    }

    #unique cluster id for each nid
    df[, cluster_unique := paste(country, year, cluster)]
    # unique psu for each nid
    df[, psu_unique := paste(country, year, psu)]
    # unique hh id for each nid
    df[, hhid_unique := paste(country, year, hh_id)]
    # unique individual id for each nid
    df[, id_unique := paste(country, year, id)]

    id_vars<-c("country", "cluster", "hh_id", "id", "area_unit", "wpweight", "pweight", "cmc_interview_date",
               "psu", "strata", "region", "year", "cluster_unique", "psu_unique", "hhid_unique", "id_unique")
    #### outcome-related variables ####

    #### pregnancy history ####
    df[, curr_preg := v213] # Whether the respondent is currently pregnant
    df[, ever_term := v228] # Whether the respondent ever had a pregnancy that terminated in a miscarriage, abortion, or still birth, i.e., did not result in a live birth
    df[, cmc_preg_term := v231] # Century month code of the last pregnancy termination.
    df[cmc_preg_term==9998, cmc_preg_term:=NA]
    df[, term_recall := cmc_interview_date - cmc_preg_term] # difference between interview date and pregnancy termination date
    df[, term_3_years := ifelse(term_recall<37, 1, 0)] # termination within 3 years of interview
    df[, number_births := v224] #Number of entries in the birth history
    df[, births_3_yr := v238] #Total number of births in the last three years is defined as all births in the months 0 to 35 prior to the month of interview, where month 0 is the month of interview
    df[, births_5_yr := v208] #Total number of births in the last five years
    df[, births_last_year := v209] # Total number of births in the past year

    #### Ever pregnant: 15-19 year olds  #####
    # Definition: Teenage pregnancy is percent of women 15-19 who are mothers or pregnant with their first child.

    df[, ever_preg := ifelse(curr_preg == 1, 1, 0)]
    df[ever_term == 1, ever_preg:=1]
    df[number_births >0, ever_preg:=1]
    summary(df$ever_preg) # why?
    
    ## construct outcome: currently pregnant or any live birth
    df[, any_birth_preg := ifelse(number_births >0 | curr_preg ==1, 1,
                                  ifelse(!is.na(number_births) & !is.na(curr_preg),0,NA)) ]

    #### Pregnancy last 3 years: 15-19   ####
    # DHS standard is 3 years
    df[, preg_36mos:=ifelse((births_3_yr>0 | term_3_years==1), 1, 0)]
    summary(df$preg_36mos)
    summary(df$births_3_yr)

    #check that ever_preg > preg_36mos > births_3_yr
    test1<-df$ever_preg>=df$preg_36mos

    outcome_vars<-c("curr_preg", "ever_term", "cmc_preg_term", "term_recall", "term_3_years", "number_births", "births_3_yr",
                    "births_5_yr", "births_last_year", "ever_preg", "preg_36mos")
    #### INDIVIDUAL CHARACTERISTICS ####

    # Age
    df[, age := v012]
    df[, age_group := v013]
    df[, cmc_women_dob := v011]

    #Marriage
    df[, cmc_first_cohabit := v509 ]
    df[, age_first_cohabit := v511]

    # Education
    df[, highest_ed_level := v106]
    df[, highest_ed_level := as_factor(highest_ed_level)]
    df[, highest_ed_level := as.character(highest_ed_level)]

    df[, educ_single_yrs := v133]
    df[, educ_single_yrs := ifelse(educ_single_yrs >90, NA, educ_single_yrs)] ## greater than 90 probably a DK or missing
    

    if ("v155" %in% names(df)){
      df[, no_read := ifelse(v155 == 0, 1, 0) ]
      df[v155==3, no_read := NA]
      df[v155 == 4, no_read:= NA]
    } else {
      unique_countries <- unique(df$v000)
      missing_values <- paste(unique_countries, collapse=", ")
      print(paste("education info is missing for: ", missing_values))
    }




    #Ovulation cycle knowledge
    df[, ov_cycle_correct := ifelse(v217==3, 1, 0)] # "Middle of the cycle"

    #AGE at first sex
    # many report that it is at the time of cohabitation, so DHS imputes by using that response here
    # 0 means have not had sex
    
    df[, age_1st_sex_imp := v531]
    df[age_1st_sex_imp>90, age_1st_sex_imp := age_first_cohabit] #  >90 is inconsistent or DK: impute with age of first cohabit
    df[, age_1st_sex_imp := ifelse(age_1st_sex_imp == 0, NA, age_1st_sex_imp)] # make NA if 0 to avoid negative values in gap sex mar
    df[, no_sex := ifelse(age_1st_sex_imp == 0, 1, 0)]

    # Fertility preferences
    df[, fert_pref :=v602]
    df[, desire_child := v605] # --> need to make desire spacing and limiting from this, although skeptical that we will use
    df[, ideal_child := v613]
    df[ideal_child > 50, ideal_child:=NA]
    df[, ideal_child_nr := ifelse(ideal_child > 50, 1, 0)] # non-(numeric)response: CB comment: does not include totally missing responses
    df[, ideal_child_gp := v614]  # grouped
    df[, desire_limiting := as.numeric(NA)]

    df[, desire_limiting := ifelse(v602 == 3, 1, desire_limiting)]
    df[, desire_spacing := as.numeric(NA)]
    df[ (v602 == 2 | (v603 >= 202 & v603 <= 299) | v603 == 993 | v603 == 996 | v603 == 998), desire_spacing := 1]
    # desire spacing var that is nested of women who do want limiting
    df[, desire_spacing_nested := desire_spacing]
    df[desire_limiting == 1, desire_spacing_nested := as.numeric(NA)]

    df[is.na(desire_limiting), desire_limiting := 0]
    df[is.na(desire_spacing), desire_spacing := 0]
    df[is.na(desire_spacing_nested) & desire_limiting == 0, desire_spacing_nested := 0]

    #mCPR
    df[, mcpr := ifelse(v313 == 3,1,0)] # --> current use by method type - modern, traditional, no method

    if ("v626a" %in% names(df)){
      #unmet need: code from DHS: https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap07_FP/FP_Need.R
      df[, fp_unmet_space := ifelse(v626a == 1, 1, 0)]
      df[, fp_unmet_limit := ifelse(v626a == 2, 1, 0)]
      df[, unmet_need := ifelse(v626a == 1 | v626a == 2, 1, 0)] # unmet need for spacing or limiting
      df[, fp_met_space := ifelse(v626a == 3, 1, 0)]
      df[, fp_met_limit := ifelse(v626a == 4, 1, 0)]
      df[, fp_met_tot := ifelse(v626a == 3 | v626a == 4, 1, 0)]
      df[, fp_demand_space := ifelse(v626a == 1 | v626a == 3, 1, 0)]
      df[, fp_demand_limit := ifelse(v626a == 2 | v626a == 4, 1, 0)]
      df[, fp_demand_tot := ifelse(unmet_need | fp_met_tot, 1, 0)]
      df[, fp_demsat_mod := ifelse(fp_demand_tot, ifelse(fp_met_tot & v313 == 3, 1, NA), NA)]
      df[, fp_demsat_any := ifelse(fp_demand_tot, ifelse(fp_met_tot, 1, NA), NA)]
    } else if ("v626" %in% names(df)){
      #unmet need: code from DHS: https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap07_FP/FP_Need.R
      df[, fp_unmet_space := ifelse(v626 == 1, 1, 0)]
      df[, fp_unmet_limit := ifelse(v626 == 2, 1, 0)]
      df[, unmet_need := ifelse(v626 == 1 | v626 == 2, 1, 0)] # unmet need for spacing or limiting
      df[, fp_met_space := ifelse(v626 == 3, 1, 0)]
      df[, fp_met_limit := ifelse(v626 == 4, 1, 0)]
      df[, fp_met_tot := ifelse(v626 == 3 | v626 == 4, 1, 0)]
      df[, fp_demand_space := ifelse(v626 == 1 | v626 == 3, 1, 0)]
      df[, fp_demand_limit := ifelse(v626 == 2 | v626 == 4, 1, 0)]
      df[, fp_demand_tot := ifelse(unmet_need | fp_met_tot, 1, 0)]
      df[, fp_demsat_mod := ifelse(fp_demand_tot, ifelse(fp_met_tot & v313 == 3, 1, NA), NA)]
      df[, fp_demsat_any := ifelse(fp_demand_tot, ifelse(fp_met_tot, 1, NA), NA)]
    } else{
      print("missing unmet need var")
    }


    # Currently working or working for cash or in kind at time of first birth
    #may want to separate later for women with a birth and those without
    df[, work_paid_curr1stb := ifelse(v714 == 1, 1, 0) ]

    # beating justified in any case
    if ("v744a" %in% names(df)){
    df[, beating_just := ifelse(v744a == 1, 1, 0)]
    df[v744b ==1, beating_just:= 1]
    df[v744c ==1, beating_just:= 1]
    df[v744d ==1, beating_just:=1]
    df[v744e ==1, beating_just:=1]
    summary(df$beating_just)

    individual_vars<-c("age", "age_group", "cmc_women_dob", "cmc_first_cohabit", "age_first_cohabit", "highest_ed_level",
                       "educ_single_yrs", "no_read", "ov_cycle_correct", "age_1st_sex_imp", "no_sex", "fert_pref",
                       "desire_child", "ideal_child", "ideal_child_nr", "ideal_child_gp", "mcpr", "unmet_need",
                       "work_paid_curr1stb", "beating_just", 'any_birth_preg')
    } else {
      unique_countries <- unique(df$v000)
      missing_values <- paste(unique_countries, collapse=", ")
      print(paste("beat_just info is missing for: ", missing_values))
    }
    ####CF indicators from SOW ####

    #### HOUSEHOLD CHARACTERISTICS ####

    df[, urban := ifelse(v025 == 1, 1, 0)]

    #religion  & ethnicity
    ## --> will need country specific re-coding
    df[, religion := v130]

      ## this works if religion is a haven labeled variable
      class(df$religion)
      df[, religion := as_factor(religion)]
      df[, religion := as.character(religion)]


      ## also do for region
      df[, region := as_factor(region)]
      df[, region := as.character(region)]
      df[, region := tolower(region)]
      

    df[, ethnicity := v131]

    hh_vars<-c("urban", "religion", "ethnicity")

    #### INDIVIDUAL CHARACTERISTICS GV LIST ####
    df[, curr_cohabit := ifelse(v502 ==1,1,
                                ifelse(v502 %in% c(0,2),0,NA))]
    df[, marital_status:= v502]
    df[, ever_married:=v502]
    df[, fp_exp_radio:=v384a]
    df[, fp_exp_tv:=v384b]
    df[, fp_exp_newsp:=v384c]
    df[, fp_exp_media:=ifelse(rowSums(!is.na(.SD), na.rm=TRUE) == 0, NA,
                              ifelse(rowSums(.SD == 1, na.rm=TRUE) >= 1, 1, 0)),
       .SDcols = c("fp_exp_radio", "fp_exp_tv", "fp_exp_newsp")]
    if ("v632" %in% names(df)){
    df[, decision_contra:=v632] # decision maker for using contraception
      df[!is.na(decision_contra), decision_use_respondent := ifelse(decision_contra == 1, 1, 0)]
      df[!is.na(decision_contra), decision_use_partner := ifelse(decision_contra == 2, 1, 0)]
      df[!is.na(decision_contra), decision_use_joint := ifelse(decision_contra == 3, 1, 0)]
      df[!is.na(decision_contra), decision_use_other := ifelse(decision_contra %in% c(6, 8, 9), 1, 0)]

      # check all the most important responses we would expect were included
      if (all(c(1, 2, 3) %ni% unique(df$decision_contra))) message(unique(df$v000), ": Double check decision_contra coding for DHS")
    } else {
      unique_countries <- unique(df$v000)
      missing_values <- paste(unique_countries, collapse=", ")
      print(paste("decision_contra info is missing for: ", missing_values))
    }
    if ("v780" %in% names(df)){
      df[, condom_ed:=v780] # Should children be taught about condoms (to avoid AIDS)

    } else {
      unique_countries <- unique(df$v000)
      missing_values <- paste(unique_countries, collapse=", ")
      print(paste("condom_ed info is missing for: ", missing_values))
    }
    # Average gap in years between median ages at first intercourse and first marriage
    df[, age_1st_mar:=v511]
    df[age_1st_mar!=age_1st_sex_imp, gap_sex_mar:=age_1st_sex_imp-age_1st_mar]

    if ("v536" %in% names(df)){
      df[, rec_sex_activity:=v536]
      df[, rec_sex_activity := as_factor(rec_sex_activity)]
      df[, rec_sex_activity := as.character(rec_sex_activity)]

    } else {
      unique_countries <- unique(df$v000)
      missing_values <- paste(unique_countries, collapse=", ")
      print(paste("recent sexual activity info is missing for: ", missing_values))
    }
    df[, age_1st_birth:=v212] # Age of the respondent at first birth

    # contraception
    df[, contra_method:=v312]
    ## add labels to contra method so we can group later:
    df[, contra_method := as_factor(contra_method)]
    df[, contra_method := as.character(contra_method)]

    df[, any_contra:= ifelse(contra_method>0, 1, 0)]
    df[, contra_type:=v313]
    df[, trad_contra_dhs :=ifelse(contra_type==2, 1, 0)] # DHS grouping
    df[, folk_contra_dhs :=ifelse(contra_type==1, 1, 0)] # DHS grouping

    # knowledge of methods
    modern_method<-c(1:7, 13, 14, 15)
    # check country specific methods if there are more modern ones and add to sum
    ## write function to check if any of the responses are 8 which is not asked and will incorrectly increase the number of methods known 
    check_not_asked <- function(var){
     
      df[, eval(var) := ifelse (get(var) >1, NA, get(var))]
      if(all(is.na(unique(df[get(var)])))){
        print(var)
        print("not asked")
      }
    }
    
    # Create the vector with formatted labels
    vector <- paste0("v304_", sprintf("%02d", modern_method))
    
    for (val in vector){
      check_not_asked(val)
    }
    

    df[ ,knowledge_mod := rowSums(.SD, na.rm=T), .SDcols = vector]
    
    
    # fertility preferences
    # v602 if they want to have another child
    # v604 preferred waiting time
    # compute desire for other child within adolescence using age

    df[, fertility_pref:=v602]
    df[, time_next_birth:=v604]
    df[, desire_child_teen:=ifelse((age==19 & fertility_pref==1 & time_next_birth==0), 1,
                                   ifelse((age==15 & fertility_pref==1 & time_next_birth<5), 1,
                                          ifelse((age==16 & fertility_pref==1 & time_next_birth<4), 1,
                                                 ifelse((age==17 & fertility_pref==1 & time_next_birth<3), 1,
                                                        ifelse((age==18 & fertility_pref==1 & time_next_birth<2), 1, 0)))))]



    #last source of contraception for current users
    df[, fp_source := v326]

    #Quality of counseling
    if ("v3a02" %in% names(df)){
      df[, fp_se := v3a02] # --> told about side effects
    } else {
      unique_countries <- unique(df$v000)
      missing_values <- paste(unique_countries, collapse=", ")
      print(paste("quality of counseling info is missing for: ", missing_values))
    }
    if ("v3a04" %in% names(df)){
    df[, fp_dealse := v3a04] # --> told about how to deal with side effects
    } else {
      unique_countries <- unique(df$v000)
      missing_values <- paste(unique_countries, collapse=", ")
      print(paste("side effect advisory info is missing for: ", missing_values))
    }
    if ("v3a05" %in% names(df)){
    df[, fp_othermethod := v3a05] # --> told about other methods
    } else{
      unique_countries <- unique(df$v000)
      missing_values <- paste(unique_countries, collapse=", ")
      print(paste("other methods advisory info is missing for: ", missing_values))
    }

    #Contact with HCW/HCP
    df[, fp_hcw12m := v393] # --> visited by FP worker in last 12 months
    df[, fp_facility12m := v394] # --> visited health facility in last 12 months

    gv_vars<-c("curr_cohabit","marital_status", "ever_married", "fp_exp_newsp", "fp_exp_tv", "fp_exp_radio", "fp_exp_media", "decision_contra", 'decision_use_respondent',
               "decision_use_partner", "decision_use_joint", "decision_use_other", "condom_ed", "age_1st_mar", "rec_sex_activity",
    	"age_1st_birth", "contra_method", "any_contra", "contra_type","trad_contra_dhs", "folk_contra_dhs", "knowledge_mod", "fertility_pref", "time_next_birth", "desire_child_teen", "fp_source", "fp_se",
    	"fp_dealse", "fp_othermethod", "fp_hcw12m", "fp_facility12m", 'gap_sex_mar')
    #### not in DHS IV ###
    if(any(unique(df$year) > 2004)){
    if ("v791a" %in% names(df)){
    df[, sex_cash:=v791a] # Had sex in return for gifts, cash or anything else in the past 12 months
    } else {
      unique_countries <- unique(df$v000)
      missing_values <- paste(unique_countries, collapse=", ")
      print(paste("sex_cash is missing for: ", missing_values))
    }

    if ("v836" %in% names(df)){
      df[, sex_partners:=v836] # Total lifetime number of sexual partners
    } else{
      unique_countries <- unique(df$v000)
      missing_values <- paste(unique_countries, collapse=", ")
      print(paste("sexual partner number info is missing for: ", missing_values))
    }
    if ("v850a" %in% names(df)){
      df[, refuse_sex:=v850a] # Can respondent refuse sex
    } else{
      unique_countries <- unique(df$v000)
      missing_values <- paste(unique_countries, collapse=", ")
      print(paste("sex refusal response info is missing for: ", missing_values))
    }
      if ("v850b" %in% names(df)){
        df[, ask_condom:=v850b] # Can ask partner to use condom.
      } else{
        unique_countries <- unique(df$v000)
        missing_values <- paste(unique_countries, collapse=", ")
        print(paste("condom conscent response info is missing for: ", missing_values))
      }

    if ('d108' %in% names(df)){
    df[, sexual_violence:=d108] # experienced any sexual violence (women in union)
}
    # presence during sex activity section
    #d$child_pres <- d$v815a  don't think we will use
    if ("v815c" %in% names(df)){
    df[, woman_pres := v815c]
    }
    if ("v815b" %in% names(df)){
    df[, man_pres :=v815b]
    }
    if ("v815b" %in% names(df) & "v815c" %in% names(df)){
      df[, adult_pres := ifelse(woman_pres == 1, 1,0)]
      df[man_pres ==1, adult_pres:=1]
    }


    # fgm
    # non of the FGM variables are included in the IR files
    # df[, fgm:=g102] # Respondent circumcised

    }
    new_dhs_vars<-c("sex_cash", "sex_partners", "refuse_sex", "ask_condom", "sexual_violence", "woman_pres", "man_pres", "adult_pres")

    #### not in DHS VIII ###
    if(any(unique(df$year)<2018 & "v769" %in% names(df))){
      df[, get_condom:=v769] # could get a condom if they wanted to
    } else {
      unique_countries <- unique(df$v000)
      missing_values <- paste(unique_countries, collapse=", ")
      print(paste("get_condom info is missing for: ", missing_values))
    }

    if(unique(df$country)%like%"GH" & any(unique(df$year)>2013) & all(unique(df$year) %ni% c(2022,2023))){
      ## country specific
      #ghana - these are not asked of all so may not be usable
      #or only usable in a sensitivity anlaysis
      df[, s106d := as_factor(s106d)]
      df[, current_school := ifelse(s106c == 1, 1, 0)] # for age 3-24 years;

      df[!is.na(current_school), non_hardship_stop_school := 0 ]
      df[s106d == "completed desired level", non_hardship_stop_school := 1 ]
      df[s106d == "no desire to continue", non_hardship_stop_school := 1 ]
      df[s106d == "waiting for approved admission", non_hardship_stop_school := 1 ]
      summary(df$non_hardship_stop_school)
      df[, s470_1 := as_factor(s470_1)]
      df[, s473_1 := as_factor(s473_1)]
      df[s470_1 == "yes" , work_paid_curr1stb:=1 ] #s470_1 only in GHA
      df[s470_1 == "yes" & s473_1  == "not paid", work_paid_curr1stb:= 0 ] #s473_1 only in GHA
      summary(df$work_paid_curr1stb)

      df[, s1044 := as_factor(s1044)]
      df[, s1045 := as_factor(s1045)]
      df[, easy_to_get_care  := ifelse(s1044 == "very easy", 1, 0 )] # check unique values
      df[s1044 == "easy", easy_to_get_care:=1]
      df[s1044 == "fairly easy", easy_to_get_care:=1]
      df[, hf_convenient := ifelse(s1045 == "fairly convenient", 1, 0)]
      df[s1045 == "convenient", hf_convenient:=1]
      df[s1045 == "very convenient", hf_convenient:=1]
      df[, prog_preg_wom := ifelse(s1025 == 1, 1,0)]  # program   helping  pregnant   women   accessing   health   services
    if ("s821" %in% names(df) & "s820" %in% names(df)){
      df[, circumcised:=s821]
      df[, fgm:=s820] # practiced in the community
    } else {
      unique_countries <- unique(df$v000)
      missing_values <- paste(unique_countries, collapse=", ")
      print(paste("circumcized and fgm info is missing for: ", missing_values))
    }
    }
    #gha_vars<-c("current_school", "non_hardship_stop_school", "work_paid_curr1stb", "easy_to_get_care", "hf_convenient", "prog_preg_wom", "circumcised", "fgm")

    f_save<-str_replace(f, ".dta", "")
    f_save<-str_replace(f, ".DTA", "")
    f_save <- paste0(f_save, "_wn_processsed.csv")

    keep_vars<-c(id_vars,  outcome_vars, individual_vars, gv_vars,  new_dhs_vars, hh_vars, "get_condom")

    ## filter just to vars of interest to save space
    cur_vars <- names(df)[names(df) %in% keep_vars]
    df <- data.table(df)
    df <- df[, ..cur_vars]

    write.csv(df, file.path(paste0(out.dir, f_save)), row.names = F)

}


##### household dataset ####
full_hh_df<-data.table()
hh<-files[files%like%"_HH_"]

## subset first last years
hh_first_last <- c("CMR_DHS4_2004_HH_CMHR44FL_Y2018M05D29.DTA", "CMR_DHS7_2018_2019_HH_CMHR71FL_Y2020M06D10.DTA",
                   "GHA_DHS4_2003_HH_GHHR4BFL_Y2018M10D15.DTA",'GHA_DHS8_2022_2023_HH_GHHR8AFL_Y2024M01D22.DTA',
                   "MWI_DHS4_2000_HH_MWHR41FL_Y2019M01D07.DTA", "MWI_DHS7_2015_2016_HH_MWHR7AFL_Y2019M12D11.DTA",
                   "NPL_DHS5_2006_HH_NPHR51FL_Y2018M11D05.DTA","NPL_DHS8_2022_HH_NPHR81FL_Y2023M06D23.DTA",
                   "RWA_DHS4_2000_HH_RWHR41FL_Y2019M03D18.DTA", "RWA_DHS8_2019_2020_HH_RWHR81FL_Y2021M10D05.DTA")


  for(f in hh_first_last){
    df<-as.data.table(read_dta(paste0(asher.dir, "dhs_data_dta/", f)))
    names(df) <- tolower(names(df)) # some country-years have capital letter while others do not
    message(unique(df$hv000)) # keep track of code running
    df[, country :=hv000]
    if(unique(df$country)%like%"NP"){
      df[, year:=hv007-57]
    } else {
      df[, year := hv007]
    }
    df[, hv025:= hv025]
    df[, hv025 := as_factor(hv025)]
    df[, hv025 := as.character(hv025)]
    df[, hv025 := tolower(hv025)]
    
    df[, cluster := hv001]
    df[, hh_id:=hv002]
    df[, hhid_unique := paste(country, year, hh_id)]
    df[, id:=hv003]
    df[, hhpweight := hv005] #household sample weight (6 decimals)
    df[, caseid:=paste(hhid, id, sep = " " )] # to merge with women's module
    save_vars <- c( "country", "year", "cluster", 'hh_id', "hhid_unique", "id", "caseid", 'hhpweight', 'hv025')

    # rename variables of interest

    # household member in school
    cols <- grep("hv110_", names(df), value = TRUE)
    for (col in cols) {
      new_col <- gsub("hv110_", "current_school_", col)  # Replace "xx" with "yy" in the column name
      df[, (new_col) := get(col)]
      save_vars <- c(save_vars, new_col)
    }

    cols <- grep("hv109_", names(df), value = TRUE)
    for (col in cols) {
      new_col <- gsub("hv109_", "education_attainment_", col)  # Replace "xx" with "yy" in the column name
      df[, (new_col) := get(col)]
      save_vars <- c(save_vars, new_col)
    }

    cols <- grep("education_attainment_", names(df), value = TRUE)
    for (col in cols) {
      new_col <- gsub( "education_attainment_", "primary_school_", col)  # Replace "xx" with "yy" in the column name
      df[, (new_col) := ifelse(is.na(get(col)), NA,
                               ifelse(get(col)==8, 0,
                                      ifelse(get(col)>=2, 1, 0)))]
      save_vars <- c(save_vars, new_col)
    }

    df[, household_head_line :=  as.character(hv218)]
    
    df[, mean_yrs_schooling_head := ifelse(household_head_line == "1", get("hv124_01"),
                                           ifelse(household_head_line == "2", get("hv124_02"),
                                                  ifelse(household_head_line == "3", get("hv124_03"),
                                                         ifelse(household_head_line == "4", get("hv124_04"),
                                                                ifelse(household_head_line == "5", get("hv124_05"),
                                                                       ifelse(household_head_line == "6", get("hv124_06"),
                                                                              ifelse(household_head_line == "3", get("hv124_03"),
                                                                                     ifelse(household_head_line == "8", get("hv124_08"),NA))))))))]
    save_vars <- c(save_vars, "mean_yrs_schooling_head")

    f_save<-str_replace(f, ".dta", "")
    f_save<-str_replace(f, ".DTA", "")
    f_save <- paste0(f_save, "_wn_hh_processsed.csv")

    ## filter just to vars of interest before saving to save space

    cur_vars <- names(df)[names(df) %in% save_vars]
    df <- df[, ..cur_vars]


    write.csv(df, file.path(paste0(out.dir, f_save)), row.names = F)
  }


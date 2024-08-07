#-------------------Header------------------------------------------------
# Project: IHME ASHER Decomposition
# Purpose: Prepare variables from woman's file from DHS surveys
# Date: 10/12/2023
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

# create function to convert CMC date to year and month
get_cmc_year <- function(x) 1900 + as.integer((as.integer(x) - 1) / 12)
get_cmc_month <- function(x) as.integer(x) - 12 * (get_cmc_year(x) - 1900)


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
  survey_name <- str_extract(survey, "[A-Z]{3}_DHS[0-9]_[0-9]{4}(_[0-9]{4})*")
  dt[, survey := survey_name]
  
  # identifiers
  dt[, cluster := v001]
  dt[, hh_id := v002]
  dt[, id := v003] 
  dt[, area_unit := v004]
  dt[, urban := ifelse(v025 == 1, 1, 0)]
  dt[, country := cur_country]
  
  # interview date
  dt[, cmc_interview_date := v008]
  dt[, year := get_cmc_year(cmc_interview_date)]
  dt[, month := get_cmc_month(cmc_interview_date)]
  
  # woman's sample weight
  dt[, wpweight := v005] 
  dt[, pweight := wpweight/1000000]
 
  # unique hh id for each household
  dt[, hhid_unique := paste(hh_id, cluster)]
  
  # unique individual id for each woman
  dt[, id_unique := paste(hhid_unique, id)]

  
  ## HOUSEHOLD CHARACTERISTICS ------------------------------------
  message("||---Household characteristics")

  # religion
  dt[, religion := as.character(as_factor(v130))]
  
  # ethnicity
  dt[, ethnicity := as.character(as_factor(v131))]
  
  # admin 1
  dt[, admin_1 := as.character(as_factor(v024))]
  
  # admin 2
  if ("sdist" %in% names(dt)) dt[, admin_2 := as.character(as_factor(sdist))]
  
  
  ## OUTCOME-RELATED VARIABLES ------------------------------------
  message("||---Outcome-related variables")
  
  # currently pregnant
  dt[, curr_preg := v213]
  
  # ever given birth (does not include still births)
  dt[, ever_birth := ifelse(v224 > 0, 1, 0)]
  
  # if any births in last 2 years
  dt[, any_births_2_yr := ifelse(v222 < 24, 1, 0)]
  dt[ever_birth == 0, any_births_2_yr := 0]
  
  # DOB of first child (CMC)
  dt[, cmc_first_child := v211]
  
  # number of births in last year
  dt[, births_last_year := v209]
  
  # number of births in last 3 years
  dt[, births_3_yr := v238]
  
  # number of births in last 5 years
  dt[, births_5_yr := v208] 
  
  # ever had a terminated pregnancy (did not result in a live birth)
  dt[, ever_term := v228]
  dt[, ever_term := ifelse(ever_term == 1, 1, ifelse(ever_term == 0, 0, NA))]
  
  # date of last pregnany termination (cmc)
  dt[, cmc_preg_term := v231] # Century month code of the last pregnancy termination.
  dt[cmc_preg_term %in% c(9998,9999), cmc_preg_term := NA]
  
  # recall period: difference between interview date and pregnancy termination date
  dt[, term_recall := cmc_interview_date - cmc_preg_term]
  
  # termination within 3 years of interview
  dt[, term_3_years := ifelse(term_recall < 36, 1, 0)]
  dt[ever_term == 0, term_3_years := 0]
  
  # number of children ever born
  dt[, number_births := v224]
 
  # currently pregnant or any live births
  dt[, any_birth_preg := ifelse(curr_preg == 1 | ever_birth == 1, 1, 0)]
  
  # ever pregnant (including still births)
  dt[, ever_preg := ifelse(curr_preg == 1 | ever_term == 1 | ever_birth == 1, 1, 0)]
  
  # ever pregnant in last 3 years
  dt[, preg_3_yrs := ifelse(births_3_yr > 0, 1, 0)]
  dt[term_3_years == 1, preg_3_yrs := 1]
  
  
  ## INDIVIDUAL CHARACTERISTICS -----------------------------------
  message("||---Individual characteristics")
  
  # age in single year
  dt[, age := v012]
  
  # subset to women 15-49
  dt <- dt[age %in% 15:49]
  
  # DOB of woman (CMC)
  dt[, cmc_woman_dob := v011]
  
  # Age of the respondent at first birth
  dt[, age_1st_birth := v212] 
  
  # current marital status 
  dt[, marital_status:= v502]
  
  # currently married/in-union
  dt[, curr_cohabit := ifelse(v502 == 1, 1, ifelse(v502 %in% c(0,2), 0, NA))]
  
  # formerly married/in-union
  dt[, former_cohabit := ifelse(v502 == 2, 1, ifelse(v502 %in% c(0,1), 0, NA))]
  
  # ever married/in-union
  dt[, ever_cohabit := ifelse(curr_cohabit == 1 | former_cohabit == 1, 1, ifelse(!is.na(curr_cohabit), 0, NA))]
  
  # if still in first marriage/union
  dt[, in_first_cohabit := ifelse(v503 == 1, 1, ifelse(v503 == 2, 0, NA))]
  dt[ever_cohabit == 0, in_first_cohabit := 0]
  
  # date and age at first marriage/union
  dt[, cmc_first_cohabit := v509]
  dt[, age_first_cohabit := v511]
  
  # date of current cohabitation
  if ("v509a" %in% names(dt)) dt[, cmc_curr_cohabit := v509a]
  
  # literacy
  if ("v155" %in% names(dt)){
    dt[, no_read := ifelse(v155 == 0, 1, 0)]
    dt[v155 %in% c(3,4), no_read := NA]
  } 
  
  
  ## SEXUAL EXPERIENCE ------------------------------------------------
  message("||---Sexual experience")
  
  # age at first sex
  dt[, age_1st_sex_imp := v531]
  
  # 0 means have never had sexual intercourse
  dt[, never_had_intercourse := ifelse(age_1st_sex_imp == 0, 1,
                                       ifelse(age_1st_sex_imp > 0 & age_1st_sex_imp < 99, 0, NA))]
  
  # >90 is inconsistent and dk: impute with age of first cohabit
  dt[age_1st_sex_imp == 97, age_1st_sex_imp := age_first_cohabit]
  
  # set 0 (never had intercourse) to NA
  dt[, age_1st_sex_imp := ifelse(age_1st_sex_imp == 0, NA, age_1st_sex_imp)]
  
  # if sexually active in last four week
  if ("v536" %in% names(dt)){
    dt[, rec_sex_activity := tolower(as.character(as_factor(v536)))]
    dt[, rec_sex_activity := ifelse(grepl("^never|^not", rec_sex_activity), "Not active in last 4 weeks", "Active in last 4 weeks")]
  } else {
    message(paste0("Recent sexual activity info is missing for: ", survey_name))
  }
  
  # average gap in years between median ages at first intercourse and first marriage
  dt[, gap_sex_mar := age_1st_sex_imp - age_first_cohabit]
  
  
  # FERTILITY PREFERENCES --------------------------------------------
  message("||---Fertility preferences")
  
  # desire for last birth, if birth within last 2 years 
  dt[v222 < 24, desire_child_then := ifelse(v367 == 1, 1, 0)]            # wanted last birth then
  dt[v222 < 24, desire_child_later := ifelse(v367 %in% c(2,3), 1, 0)]    # wanted last birth later or not at all
  
  # desire for a/another child
  dt[, fertility_pref := v602]
  
  # desire waiting time for another child
  dt[, desire_timing := v604]
  
  # identify adolescents who desire a/another child within adolescence 
  dt[, desire_child_teen:=ifelse((age==19 & fertility_pref==1 & desire_timing==0), 1,
                                 ifelse((age==15 & fertility_pref==1 & desire_timing<5), 1,
                                        ifelse((age==16 & fertility_pref==1 & desire_timing<4), 1,
                                               ifelse((age==17 & fertility_pref==1 & desire_timing<3), 1,
                                                      ifelse((age==18 & fertility_pref==1 & desire_timing<2), 1,
                                                             ifelse(age %in% 15:19, 0, NA))))))]
  
  # construct desire spacing and limiting
  # desire spacing = wants more children, but in 2+ years, or are unsure about more children/timing
  # desire limiting = wants no more children
  dt[, desire_spacing := ifelse(v605 %in% c(2,3,4), 1, 0)]
  dt[, desire_limiting := ifelse(v605 == 5, 1, 0)]
  
  # desire soon = wants a child soon
  dt[, desire_soon := ifelse(v603 == 994, 1, 0)]
  dt[is.na(v603), desire_soon := 0]
  
  # create desire_later
  dt[, desire_later := ifelse(desire_limiting == 1 | desire_spacing == 1, 1, 0)]
  
  # desire spacing among only women who do want limiting
  dt[, desire_spacing_nested := desire_spacing]
  dt[desire_limiting == 1, desire_spacing_nested := as.numeric(NA)]
  
  # update denominator
  dt[is.na(desire_spacing_nested) & desire_limiting == 0, desire_spacing_nested := 0]
  
  # ideal number of children
  dt[, ideal_child := v613]
  dt[ideal_child > 50, ideal_child := NA] # remove non-numeric responses
  dt[, ideal_child_nr := ifelse(ideal_child > 50, 1, 0)] # non-(numeric)response: CB comment: does not include totally missing responses

  
  
  # CONTRACEPTIVE USE -------------------------------------------------
  message("||---Contraceptive use")
  
  # currently using any contraception
  dt[, any_contra := ifelse(v312 == 0, 0, 1)]
  
  # never used contraception (captured via ever use so need to flip 0/1)
  if (any(!is.na(dt$v302))) {
    dt[, never_used_contra := v302]
  } else {
    dt[, never_used_contra := v302a]
  }
  dt[, never_used_contra := ifelse(never_used_contra %in% c(1,2,3), 0, ifelse(never_used_contra == 0, 1, NA))]
  
  # current method in use
  dt[, current_contra := tolower(as.character(as_factor(v312)))]
  
  # identify specific methods using search strings
  # modern methods 
  male_sterilization <- paste(c("^male ster", "^male_steril", "vasect", "operacion masc", "m ster", "est.*masc", " male ster", "sterilisation masc", " male ster",
                                "esterilizacion macul", "lisation pour les hommes", "^male ester", "^male-ster", "sterilisation \\(male\\)", " male ester",
                                "^males steril", "m\\.steril", "^ms$", "#male ster", "#male_steril", "#male ester", "#male-ster", "#males steril"), collapse = "|")
  female_sterilization <- paste(c("female ster", "ligation", "ligature", "ligadura", "legation", "fem ster", "esterilizada", "operacion fem", 
                                  "est.*fem", "steril.*fem", "fem.*steril", "female-ster", "f\\.steril", "^fs$"), collapse = "|")
  sterilization <- paste(c("sterili", "ester", "-ster", "fem ster", "m ster", "vasect", "ale steril", "operacion", "ligation", "ligature", "ligadura", 
                           'ester\\.', "est\\.", "sterliz", "female ster", "laquea", "legation"), collapse = "|") # make sure to not capture "sterilet" which is actually French for IUD
  pill <- paste(c("pill", "pilul", "pastill", "pildor", "oral", "phill"), collapse = "|")
  iud <- paste(c("iud", "diu", "dui", "spiral", "sterilet", "st\\?rilet", "coil", "iucd", "i\\.u\\.d.", "loop", "copper", "strilet"), collapse = "|")
  injections <- paste(c("inyec", "injec", "depo", "sayana", "piqure", "injeta"), collapse = "|")
  implants <- paste(c("implant", "norplant", "zadelle", "transplant", "inplant", "norolant"), collapse = "|")
  condom <- paste(c("condom", "condon", "preservati", "camisinha", "codom"), collapse = "|")
  emergency <- paste(c("morning-after", "morning after", "emergenc", "du lendemain", "dia siguiente", "contraception after sex", "^ec$"), collapse = "|")
  patch <- paste(c("patch", "parche"), collapse = "|")
  ring <- paste(c("contraceptive ring", "vaginal ring", "/ring", "anillo"), collapse = "|")
  diaphragm <- paste(c("diaphr", "diafrag", "cervical cap", "cones"), collapse = "|")
  foam_gel_sponge <- paste(c("tablet", "foam", "jelly", "jalea", "mousse", "espuma", "creme", "cream", "crema", "gel", "gelee", "spermicid", "eponge", "intravag",
                             "esponja", "esonja", "sponge", "vaginale", "comprimidos vaginais", "vaginal method", "suppository", "vaginals", "metodos vaginal"), collapse = "|")
  other_mod <- paste(c("modern", "other mod", "other_mod", "fem sci", "sci fem", "scien fem", "other female", "oth fsci", "menstrual regulation", "campo de latex"), collapse = "|") 
  
  # traditional methods
  lam <- paste(c("lactat", "lactan", "amenor", "lam", "mela", "breastf", "allaite", "mama", "prolonged bf", "lact\\. amen\\.",
                 "prolonged brst", "lact\\.amen\\."), collapse = "|")
  withdrawal <- paste(c("withdr", "retiro", "retrait", "coitus interr", "coito interr", "interruption"), collapse = "|")
  rhythm <- paste(c("rhyth", "rythm", "ritmo", "periodic abst", "abstinence period", "ncia peri", "safe period", "periodical abst", 
                    "rhtyhm", "sporadic abstinence", "periodiqu", "contnence peridque", "uterus inversion", "cont per"), collapse = "|")
  calendar_beads_sdays <- paste(c("calen", "beads", "sdm", "standard days", "billing", "mucus", "moco cervical", "ovulat", "fixed days", "string", "collar", 
                                  "collier", "dangerous days", "fertility wheel", "calandrier"), collapse = "|")
  other_trad <- paste(c("other", "otro", "autre", "other trad", "other_trad", "tradition", "herb", "hierba", "douch", "yuyo", "symptothermal", "temperature",
                        "gris", "medicinal plant", "plantes medici", "charm", "quinine", "natural", "lavado vaginal", "trad meth", "persona", "outro",
                        "candle", "washing", "meth tradi", "massage", "exercise", "akar kayu", "give suck", "stout", "mjf", "trad\\. intra",
                        "pijat", "jamu", "isolation", "folkloric", "coran", "marabout", "temperatr", "cerv mucs"), collapse = "|")
  
  # abstinence 
  # we currently do not count abstinence (not to be confused with periodic abstinence/rhythm) as a contraceptive method
  # still want to identify in order to easily include or exclude from current_use 
  abstinence <- paste(c("prolonged abstinence", "abstinence", "abstinencia", "abstinence prolong", "not sexually active",
                        "abstention"), collapse = "|")
  
  # non-use
  nonuse <- paste(c("none", "not using", "no method", "not current user", "not currently using", "no contraceptive", "no usa",
                    "no esta usa", "no estan usa", "nonuser", "non-user", "not expose", "non expose", "n expose", "not user",
                    "no current use", "nutilise", "did not use"), collapse = "|")
  
  # no response or unknown if a woman is using a method or what method 
  unknown <- paste(c("refused", "^dna", "don't know", "dont know", "don t know", "not stated", "no respon", "non repons", "not eligible", 
                     "inconsistent", "-99", "^na$", "refusal", "missing", "manquant", "does not know", "^$", "^ns$", "s/inf", "^dk$", 
                     "unsure", "sin informacion"), collapse = "|")
  
  # search current_contra for the most effective method a women is currently using; the order of these is very important!!! 
  # want to identify most effective methods last in order to overwrite less effective methods 
  # abstinence and rhythm will identify the same strings, ensure rhythm is always after abstinence to differentiate 
  dt[grepl(unknown, tolower(current_contra)), current_method := "no_response"]
  dt[grepl(nonuse, tolower(current_contra)), current_method := "none"]
  dt[grepl(abstinence, tolower(current_contra)), current_method := "abstinence"]
  dt[grepl(other_trad, tolower(current_contra)), current_method := "other_traditional_method"]
  dt[grepl(other_mod, tolower(current_contra)), current_method := "other_modern_method"]
  dt[grepl(withdrawal, tolower(current_contra)), current_method := "withdrawal"]
  dt[grepl(calendar_beads_sdays, tolower(current_contra)), current_method := "calendar_methods"]
  dt[grepl(rhythm, tolower(current_contra)), current_method := "rhythm"]
  dt[grepl(lam, tolower(current_contra)), current_method := "lactational_amenorrhea_method"]
  dt[grepl(emergency, tolower(current_contra)), current_method := "emergency_contraception"]
  dt[grepl(foam_gel_sponge, tolower(current_contra)), current_method := "foam_jelly_sponge"]
  dt[grepl(diaphragm, tolower(current_contra)), current_method := "diaphragm"]
  dt[grepl(condom, tolower(current_contra)), current_method := "condom"]
  dt[grepl(ring, tolower(current_contra)), current_method := "contraceptive_ring"]
  dt[grepl(patch, tolower(current_contra)), current_method := "contraceptive_patch"]
  dt[grepl(pill, tolower(current_contra)), current_method := "pill"]
  dt[grepl(implants, tolower(current_contra)), current_method := "implants"]
  dt[grepl(injections, tolower(current_contra)), current_method := "injections"]
  dt[grepl(iud, tolower(current_contra)), current_method := "iud"]
  dt[grepl(male_sterilization, tolower(current_contra)), current_method := "male_sterilization"] 
  dt[grepl(female_sterilization, tolower(current_contra)), current_method := "female_sterilization"] 
  
  ## check to ensure all current_contra responses were processed, otherwise need to revise/add search strings 
  if (any(is.na(dt$current_method))) stop(paste0(survey, ": current_contra response ", dt[is.na(current_method), unique(current_contra)], " not getting captured by search strings!!!"))
  
  ## identify modern and traditional contraceptive users
  dt[current_method %in% c("female_sterilization", "male_sterilization", "iud", "injections", "implants", "pill", "contraceptive_patch", "contraceptive_ring", "condom",
                           "diaphragm", "foam_jelly_sponge", "emergency_contraception", "other_modern_method"), mod_contra := 1]
  
  dt[current_method %in% c("lactational_amenorrhea_method", "rhythm", "calendar_methods", "withdrawal", "other_traditional_method"), trad_contra := 1]
  
  # unless specifically marked as missing/no response, assume women were asked and should therefore be in denominator
  dt[(!is.na(current_method) | !is.na(any_contra)) & is.na(mod_contra), mod_contra := 0]
  dt[(!is.na(current_method) | !is.na(any_contra)) & is.na(trad_contra), trad_contra := 0]
  
  # create mcpr
  dt[, mcpr := mod_contra]
  
  
  
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
  
  # set infecund to 0 to begin with
  dt[, infecund := 0]
  
  # extract timing of last menstruation, convert to months
  dt[, last_menses_timing := v215]
  dt[last_menses_timing >= 100 & last_menses_timing <= 190, last_menses_months := (last_menses_timing-100)/30]  # days
  dt[last_menses_timing >= 200 & last_menses_timing <= 290, last_menses_months := (last_menses_timing-200)/4.3]  # weeks
  dt[last_menses_timing >= 300 & last_menses_timing <= 390, last_menses_months := (last_menses_timing-300)]  # months
  dt[last_menses_timing >= 400 & last_menses_timing <= 490, last_menses_months := (last_menses_timing-400)*12]  # years
  
  # set infecund to 1 if answers to last menstruation:
  # i) has not menstruated for at least six months
  dt[last_menses_months >= 6, infecund := 1]   
  # ii) has never menstruated
  dt[last_menses_timing == 996, infecund := 1]   
  # iii) had last menstruation occurring before last birth AND last birth 5+ years ago
  dt[last_menses_timing == 995 & births_5_yr == 0, infecund := 1]   
  # iv) is in menopause/has had hysterectomy  
  dt[last_menses_timing == 994, infecund := 1]     
  
  # set infecund to 1 if answers to why not using contraception:
  # i) menopausal/hysterectomy
  if ("v3a08d" %in% names(dt)) {
    dt[v3a08d == 1, infecund := 1]
  } else {
    dt[v375a == 23, infecund := 1]
  }
  
  # set infecund to 1 if answers to desire for future birth:
  # i) cannot get pregnant
  dt[fertility_pref == 5, infecund := 1]
  
  # set infecund to 1 if has not had a birth in the preceding 5 years, has never used contraception,
  # and is currently married and was continuously married during the preceding 5 years (can only determine if in first marriage/union)
  dt[curr_cohabit == 1 &                                             # currently married/in-union
       never_used_contra == 1 &                                      # never used contraception
       in_first_cohabit == 1 &                                       # in first marriage/union
       births_5_yr == 0 &                                            # no births in last 5 years
       cmc_interview_date - cmc_first_cohabit >= 60, infecund := 1]  # been married continuously in last 5 years
  
  # also pregnant women are not infecund even though their last menses may have been >6 months ago
  dt[curr_preg == 1, infecund := 0]
  
  # if infecund, set need_contra to 0
  dt[infecund == 1, need_contra := 0]
  
  
  # 4. For pregnant women and women who are postpartum amenorrheic from a birth in
  #    the last 2 years, need is determined separately based on whether they wanted
  #    to space or limit their current/most recent pregnancy
  
  # pregnant women are assumed to not need contraception as they are not at risk for pregnancy
  dt[curr_preg == 1, need_contra := 0]
  
  # extract desire for current pregnancy
  dt[, preg_wanted_curr := as.character(as_factor(v225))]
  dt[, preg_not_wanted := ifelse(v225 %in% c(2,3), 1, ifelse(v225 == 1 ,0, NA))]
  
  # set need_contra to 1 if pregnant women wanted to space/limit their pregnancy
  dt[curr_preg == 1 & preg_not_wanted == 1, need_contra := 1]
  
  
  # determine need among postpartum amenorrheic women

  # menstrual period not returned since birth of child in last 2 years
  dt[, menses_not_returned := ifelse(m6_1 == 96, 1, ifelse(!is.na(m6_1), 0, NA))]                                                    # duration of ammenorhea last birth
  dt[menses_not_returned != 1, menses_not_returned := ifelse(births_5_yr > 0 & last_menses_months > v222, 1, menses_not_returned)]   # last menstruation before last birth in last 5 years
  dt[menses_not_returned != 1, menses_not_returned := ifelse(births_5_yr > 0 & last_menses_timing == 995, 1, menses_not_returned)]   # last menstruation before last birth in last 5 years
  
  # a woman is postpartum amenorrheic if she had a live birth in last two years and is not currently pregnant, and her menstrual
  # period has not returned since the birth of the last child.
  dt[v222 < 24 & curr_preg == 0 & menses_not_returned == 1, ppa := 1]
  
  # set need_contra to 0 for postpartum ammenhoreic women to start
  dt[ppa == 1, need_contra := 0]
  
  # postpartum ammenorheic women are not infecund
  dt[ppa == 1, infecund := 0]
  
  # set need_contra to 1 if ppa women wanted to space/limit their most recent pregnancy
  dt[ppa == 1 & desire_child_later == 1, need_contra := 1]
  
  # regardless of answers to any other questions, if a woman is currently using any contraceptive method then
  # she is considered to have a need for contraception
  if ("any_contra" %in% names(dt)) dt[any_contra == 1, need_contra := 1]
  
  # restrict need_contra to those observations where we know women were actually asked about contraception,
  # making it consistent with the denominator of modern contraceptive usage (since otherwise it's impossible
  # to know whether that need was met or not)
  if ("mod_contra" %in% names(dt)) dt[is.na(mod_contra), need_contra := NA]
  
  # if surveys are missing sexual experience, fertility preferences, or contraceptive use, we are not 
  # able to correctly calculate need, set entirely to NA

  if (!all(c("rec_sex_activity", "desire_later", "any_contra") %in% names(dt))) dt[, need_contra := NA]

  
  # DEMAND SATISFIED/UNMET NEED -------------------------------------
  message("||---Demand satisfied/unmet need")
  
  # extract DHS-calculated unmet need for validation
  if ("v626a" %in% names(dt)){
    dt[, unmet_need_dhs := v626a]
  } else if ("v626" %in% names(dt)){
    dt[, unmet_need_dhs := v626]
  }
  
  # unmet need = women with a need for contraception who are NOT using ANY contraception
  dt[, unmet_need := ifelse(any_contra == 0 & need_contra == 1, 1, ifelse(!is.na(need_contra), 0, NA))]
  
  # unmet need mod = women with a need for contraception who are NOT using MODERN contraception
  dt[, unmet_need_mod := ifelse(mod_contra == 0 & need_contra == 1, 1, ifelse(!is.na(need_contra), 0, NA))]
  
  # demand satisfied = women with a need for contraception who are using MODERN contraception
  dt[, demand_satisfied := ifelse(need_contra == 1, mod_contra, NA)]
  
  
  # ADDITIONAL VARIABLES -------------------------------------------
  message("||---Additional variables")
  
  # ovulation cycle knowledge
  dt[, ov_cycle_correct := ifelse(v217 == 3, 1, 0)] # "Middle of the cycle"
  
  # currently working 
  dt[, employed := ifelse(v714 == 1, 1, 0)]
  
  # beating justified in any case
  if ("v744a" %in% names(dt)){
    dt[, beating_just := ifelse(v744a == 1, 1, 0)]
    dt[v744b == 1, beating_just := 1]
    dt[v744c == 1, beating_just := 1]
    dt[v744d == 1, beating_just := 1]
    dt[v744e == 1, beating_just := 1]
  }
  
  # exposure to family planning messages on media
  dt[, fp_exp_radio := v384a]
  dt[, fp_exp_tv := v384b]
  dt[, fp_exp_newsp := v384c]
  dt[, fp_exp_media := ifelse(rowSums(!is.na(.SD), na.rm=TRUE) == 0, NA,
                              ifelse(rowSums(.SD == 1, na.rm=TRUE) >= 1, 1, 0)),
     .SDcols = c("fp_exp_radio", "fp_exp_tv", "fp_exp_newsp")]
  
  
  # decision maker for using contraception
  # ONLY if married/in-union
  if ("v632" %in% names(dt)){
    dt[, decision_contra := v632] 
    dt[!is.na(decision_contra), decision_use_respondent := ifelse(decision_contra == 1, 1, 0)]
    dt[!is.na(decision_contra), decision_use_partner := ifelse(decision_contra == 2, 1, 0)]
    dt[!is.na(decision_contra), decision_use_joint := ifelse(decision_contra == 3, 1, 0)]
    dt[!is.na(decision_contra), decision_use_other := ifelse(decision_contra %in% c(6, 8, 9), 1, 0)]
    
    # check all the most important responses we would expect were included
    if (all(c(1, 2, 3) %ni% unique(dt$decision_contra))) message(unique(dt$v000), ": Double check decision_contra coding for DHS")
    
    # create combined respondent/joint decision to use variable
    dt[, decision_use_joint_respondent := ifelse(decision_use_joint ==1 | decision_use_respondent ==1, 1, 
                                                 ifelse(is.na(decision_use_joint) & is.na(decision_use_respondent), NA, 0))]
  }
  
  # should children be taught about condoms (to avoid AIDS)
  if ("v780" %in% names(dt)){
    dt[, condom_ed := ifelse(v780 == 1, 1, ifelse(v780 %in% c(0,8), 0, NA))] 
  }
  
  # knowledge of methods
  
  # responses which are a modern method
  # check country specific methods if there are more modern ones and add to sum
  modern_method <- c(1:3, 5:7)
  
  # write function to check if any of the responses are 8 which is not asked and will incorrectly increase the number of methods known 
  check_not_asked <- function(var){
    dt[, eval(var) := ifelse(get(var) > 1, NA, get(var))]
    if(all(is.na(unique(dt[get(var)])))){
      print(var)
      print("not asked")
    }
  }
  
  # Create the vector with formatted labels
  vector <- paste0("v304_", sprintf("%02d", modern_method))
  for (val in vector){
    check_not_asked(val)
  }
  
  # sum modern methods which were asked about
  dt[, knowledge_mod := rowSums(.SD, na.rm=T), .SDcols = vector]
  
  
  # last source of contraception for current users
  dt[, fp_source := v326]
  
  # source groupings
  
  # 10's are public, 20's are private, 30's are other private sector, 40's and 96 are other 
  dt[!is.na(fp_source), contra_source_public := ifelse(fp_source >= 10 & fp_source <= 19, 1, 0)]
  dt[!is.na(fp_source), contra_source_priv := ifelse(fp_source >= 20 & fp_source <= 29, 1, 0)]
  dt[!is.na(fp_source), contra_source_other := ifelse((fp_source >= 30 & fp_source <= 49) | fp_source == 96, 1, 0)]
  
  
  # method information index
  
  # told about side effects
  if ("v3a02" %in% names(dt)){
    dt[, fp_se := v3a02]
  }
  
  # told about side effects by health/FP worker
  if ("v3a03" %in% names(dt)){
    dt[fp_se == 0 & !is.na(v3a03), fp_se := v3a03]
  }
  
  # told about how to deal with side effects
  if ("v3a04" %in% names(dt)){
    dt[, fp_dealse := v3a04]
    
    # women who answered "no" to told about side effects typically skip se instructions
    dt[fp_se == 0 & is.na(fp_dealse), fp_dealse := 0]
  }
  
  # told about other methods
  if ("v3a05" %in% names(dt)){
    dt[, fp_othermethod := v3a05]
  }
  
  # told about other methods by health/FP worker
  if ("v3a06" %in% names(dt)){
    dt[fp_othermethod == 0 & !is.na(v3a06), fp_othermethod := v3a06]
  }
  
  # contact with HCW/HCP
  
  # visited by health worker in last 12 months who talked about fp
  if ("v393a" %in% names(dt)) {
    dt[, fp_hcw12m := ifelse(v393a == 1, 1, ifelse(v393a == 0, 0, NA))]
    dt[is.na(v393a), fp_hcw12m := 0]
  } else if ("v393" %in% names(dt)) {
    dt[, fp_hcw12m := ifelse(v393 == 1, 1, ifelse(v393 == 0, 0, NA))]
  } 
  
  # visited health facility in last 12 months
  dt[, facility12m := ifelse(v394 == 1, 1, ifelse(v394 == 0, 0, NA))]
  
  # staff member at facility spoke about fp
  dt[, fp_facility12m := ifelse(v395 == 1, 1, ifelse(v395 == 0, 0, NA))]
  dt[facility12m == 0, fp_facility12m := 0]
  
  
  # had sex in return for gifts, cash or anything else in the past 12 months
  if ("v791a" %in% names(dt)){
    dt[, sex_cash := v791a] 
  } 
  
  # total lifetime number of sexual partners
  if ("v836" %in% names(dt)){
    dt[, sex_partners := ifelse(v836 < 90, v836, NA)] 
  }
  
  # can respondent refuse sex
  # ONLY women married/in-union
  if ("v850a" %in% names(dt)){
    dt[, refuse_sex := ifelse(v850a == 1, 1, ifelse(v850a %in% c(0,8), 0, NA))] 
  } 
  
  # can ask partner to use condom
  # ONLY women married/in-union
  if ("v850b" %in% names(dt)){
    dt[, ask_condom := ifelse(v850b == 1, 1, ifelse(v850b %in% c(0,8), 0, NA))] 
  } 
  
  # experience any sexual violence by husband/partner
  # ONLY women married/in-union
  if ('d108' %in% names(dt)){
    dt[, sexual_violence := d108] 
  } 
  
  # presence of others during sexual activity section
  if ("v815c" %in% names(dt)){
    dt[, woman_pres := v815c]
  }
  if ("v815b" %in% names(dt)){
    dt[, man_pres :=v815b]
  }
  if ("v815b" %in% names(dt) & "v815c" %in% names(dt)){
    dt[, adult_pres := ifelse(woman_pres == 1, 1,0)]
    dt[man_pres ==1, adult_pres:=1]
  }
  
  # can get a condom
  if ("v769" %in% names(dt)) {
    dt[, get_condom := ifelse(v769 == 1, 1, ifelse(v769 %in% c(0,8), 0, NA))] 
  } 
  

  # EXPORT CLEANED DATA --------------------------------------------
  message("||---Export cleaned data")
  
  # filter to variables of interest
  vars_interest <- c("survey","country","cluster","hh_id","id","area_unit","urban","admin_1","admin_2","year","month","cmc_interview_date",
                     "wpweight","pweight","hhid_unique","id_unique","ethnicity","religion","curr_preg","ever_birth",
                     "any_births_2_yr","cmc_first_child","births_last_year","births_3_yr","births_5_yr","ever_term",
                     "cmc_preg_term","term_recall","term_3_years","number_births","any_birth_preg","ever_preg","preg_3_yrs",
                     "age","cmc_woman_dob","age_1st_birth","marital_status","curr_cohabit","former_cohabit","ever_cohabit","cmc_curr_cohabit",
                     "in_first_cohabit","cmc_first_cohabit","age_first_cohabit","no_read","age_1st_sex_imp","never_had_intercourse",
                     "rec_sex_activity","gap_sex_mar","desire_child_then","desire_child_later","fertility_pref","desire_timing",
                     "desire_child_teen","desire_spacing","desire_limiting","desire_soon","desire_later","desire_spacing_nested","preg_wanted_curr",
                     "ideal_child","ideal_child_nr","any_contra","never_used_contra","current_contra",
                     "current_method","mod_contra","trad_contra","mcpr","need_contra","infecund","last_menses_timing","last_menses_months", 
                     "preg_not_wantd","menses_not_returned","ppa","unmet_need_dhs","unmet_need","unmet_need_mod","demand_satisfied",
                     "ov_cycle_correct","employed","beating_just","fp_exp_radio","fp_exp_tv","fp_exp_newsp","fp_exp_media",
                     "decision_contra","decision_use_respondent","decision_use_partner","decision_use_joint","decision_use_other",
                     "decision_use_joint_respondent","condom_ed","knowledge_mod","fp_source","contra_source_public","contra_source_priv",
                     "contra_source_other","fp_se","fp_dealse","fp_othermethod","fp_hcw12m","facility12m","fp_facility12m",
                     "sex_cash","sex_partners","refuse_sex","ask_condom","sexual_violence","woman_pres","man_pres","adult_pres","get_condom")
  cur_vars <- names(dt)[names(dt) %in% vars_interest]
  dt_filter <- dt[, ..cur_vars]
  
  # save the cleaned data
  file_name <- gsub("\\.DTA", "", basename(survey))
  output_file_path <- file.path(out.dir, paste0(file_name, "_wn_extract.csv"))
  write.csv(dt_filter, output_file_path, row.names = FALSE)
  
  message(paste0("Successfully saved ", file_name))
}


# RUN EXTRACTIONS ---------------------------------------------------

# Cameroon
extract_data("/FILEPATH/CMR_DHS4_2004_WN.DTA", "cm")
extract_data("/FILEPATH/CMR_DHS5_2011_WN.DTA", "cm")
extract_data("/FILEPATH/CMR_DHS7_2018_2019_WN.DTA", "cm")

# Ghana
extract_data("/FILEPATH/GHA_DHS4_2003_WN.DTA", "gh")
extract_data("/FILEPATH/GHA_DHS5_2008_WN.DTA", "gh")
extract_data("/FILEPATH/GHA_DHS6_2014_WN.DTA", "gh")
extract_data("/FILEPATH/GHA_DHS8_2022_2023_WN.DTA", "gh")

# Malawi
extract_data("/FILEPATH/MWI_DHS4_2000_WN.DTA", "mw")
extract_data("/FILEPATH/MWI_DHS4_2004_2005_WN.DTA", "mw")
extract_data("/FILEPATH/MWI_DHS6_2010_WN.DTA", "mw")
extract_data("/FILEPATH/MWI_DHS7_2015_2016_WN.DTA", "mw")

# Nepal
extract_data("/FILEPATH/NPL_DHS4_2001_WN.DTA", "np")
extract_data("/FILEPATH/NPL_DHS5_2006_WN.DTA", "np")
extract_data("/FILEPATH/NPL_DHS6_2011_WN.DTA", "np")
extract_data("/FILEPATH/NPL_DHS7_2016_2017_WN.DTA", "np")
extract_data("/FILEPATH/NPL_DHS8_2022_WN.DTA", "np")

# Rwanda
extract_data("/FILEPATH/RWA_DHS4_2000_WN.DTA", "rw")
extract_data("/FILEPATH/RWA_DHS4_2005_WN.DTA", "rw")
extract_data("/FILEPATH/RWA_DHS6_2010_2011_WN.DTA", "rw")
extract_data("/FILEPATH/RWA_DHS7_2014_2015_WN.DTA", "rw")
extract_data("/FILEPATH/RWA_DHS8_2019_2020_WN.DTA", "rw")

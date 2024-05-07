#-------------------Header------------------------------------------------
# Author: Olivia Angelino
# Project: ASHER Decomp
# Purpose: Process DHS contraceptive calendar data
# Date: 2/27/2024
# Notes:
#***************************************************************************

# SET-UP --------------------------------------------------------------------

# clear memory
rm(list=ls())

# Username is pulled automatically
username <- Sys.getenv("USER") 

# runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j <- "/snfs1/"
  h <- paste0("/homes/", username, "/")
} else {
  j <- "J:/"
  h <- "H:/"
}

# load packages, install if missing
pacman::p_load(data.table,tidyverse,dplyr,foreign,haven)

# in/out
out.dir <- '/share/scratch/projects/hssa/asher/data/01_processed/'

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
  dt[, int_year := get_cmc_year(cmc_interview_date)]
  dt[, int_month := get_cmc_month(cmc_interview_date)]
  
  # woman's sample weight
  dt[, wpweight := v005] 
  dt[, pweight := wpweight/1000000]
  
  # unique hh id for each household
  dt[, hhid_unique := paste(hh_id, cluster)]
  
  # unique individual id for each woman
  dt[, id_unique := paste(hhid_unique, id)]
  
  
  ## INDIVIDUAL CHARACTERISTICS -----------------------------------
  message("||---Individual characteristics")

  # DOB of woman (cmc)
  dt[, cmc_woman_dob := v011]
  
  # if any pregnancies terminated before calendar beginning
  dt[, any_term_before_cal := v239]
  
  
  ## CONTRACEPTIVE CALENDAR ---------------------------------------
  message("||---Contraceptive calendar")
  
  # extract calendar variables
  dt[, calendar_starts := v017]
  dt[, calendar_length := v019]
  dt[, calendar_contra_use := vcal_1]
  if ("vcal_2" %in% names(dt)) dt[, calendar_reason_discont := vcal_2]
  
  # RWA DHS 2010-2011 survey fix
  # survey specific method in contraceptive calendar "standard days method" which should be grouped with calendar methods
  if (grepl("RWA_DHS6", survey)) dt[, calendar_contra_use := gsub("D", "S", calendar_contra_use)]
  
  # new data table
  calendar <- dt[, c("survey","cluster","hh_id","id","area_unit","urban","country","cmc_interview_date","int_year","int_month",
                     "wpweight","pweight","hhid_unique","id_unique","cmc_woman_dob","any_term_before_cal",
                     names(dt)[grepl("^calendar", names(dt))]), with = F]
  
  # create data table to store calendar in woman-month long format
  calendar_long <- data.table()
  
  # loop through each calendar start date
  for (cal_start in unique(calendar$calendar_starts)) {
    # determine maximum length of calendar and get month dates and numbers
    months <- rev(as.character(seq(as.Date(paste(get_cmc_year(cal_start), get_cmc_month(cal_start), "1", sep = " "), format = '%Y %m %d'), by = "month", length.out = 80)))
    months_cmc <- rev(seq(from = cal_start, to = cal_start + 79))
    month_nums <- seq(1, 80, 1)
    
    # create a table of all the month numbers for every woman's id and merge with the calendar data
    expand <- CJ(id_unique = calendar[calendar_starts == cal_start]$id_unique, month_num = month_nums)
    tmp <- merge(calendar, expand, by = "id_unique")
    
    # add onto calendar_long
    calendar_long <- rbind(calendar_long, tmp, fill = T)
  }
 
  # create column with real and cmc dates and remove empty rows after the interview month
  calendar_long[, month := months[month_num]]
  calendar_long[, month_cmc := months_cmc[month_num]]
  calendar_long <- calendar_long[month_cmc <= cmc_interview_date]
  
  # parse out the corresponding contraceptive use/birth events
  calendar_long[, event := substr(calendar_contra_use, month_num, month_num)]
  calendar_long[, reason := substr(calendar_reason_discont, month_num, month_num)]
 
  # update event to missing if it is empty
  calendar_long[event == " ", event := "?"]
  
  # update reason for discontinuation to a blank if it is empty
  calendar_long[reason == "", reason := " "]
  
  # contraceptive use
  # alpha, beta, tau are country specific
  calendar_long[event == "0", event_recode := "no method used"]
  calendar_long[event == "1", event_recode := "pill"]
  calendar_long[event == "2", event_recode := "iud"]
  calendar_long[event == "3", event_recode := "injection"]
  if (grepl('DHS2', survey)) {
    calendar_long[event == "4", event_recode := "diaphragm/foam/jelly"]
  } else {
    calendar_long[event == "4", event_recode := "diaphragm"]
  }
  calendar_long[event == "5", event_recode := "condom"]
  calendar_long[event == "6", event_recode := "female sterilization"]
  calendar_long[event == "7", event_recode := "male sterilization"]
  calendar_long[event == "8", event_recode := "rhythm"]
  calendar_long[event == "9", event_recode := "withdrawal"]
  calendar_long[event == "W", event_recode := "other traditional method"]
  calendar_long[event == "N", event_recode := "implants"]
  calendar_long[event == "A", event_recode := "abstinence"]
  calendar_long[event == "L", event_recode := "lactational amenorrhea method"]
  calendar_long[event == "C", event_recode := "female condom"]
  calendar_long[event == "E", event_recode := "emergency contraception"]
  calendar_long[event == "S", event_recode := "calendar methods"]
  calendar_long[event == "M", event_recode := "other modern method"]
  calendar_long[event == "F", event_recode := "foam/jelly"]
  calendar_long[event == "B", event_recode := "birth"]
  calendar_long[event == "T", event_recode := "termination"]
  calendar_long[event == "P", event_recode := "pregnancy"]
  calendar_long[event == "?", event_recode := "missing"]
  calendar_long[event == "a", event_recode := "country specific"]
  calendar_long[event == "b", event_recode := "country specific"]
  calendar_long[event == "t", event_recode := "country specific"]
  
  # reasons for discontinuation
  calendar_long[reason == "1", reason_recode := "became pregnant while using"]
  calendar_long[reason == "2", reason_recode := "wanted to become pregnant"]
  calendar_long[reason == "3", reason_recode := "husband disapproved"]
  calendar_long[reason == "4", reason_recode := "side effects/health concerns"]
  calendar_long[reason == "5", reason_recode := "health concerns"]
  calendar_long[reason == "6", reason_recode := "access/availability"]
  calendar_long[reason == "7", reason_recode := "wanted a more effective method"]
  calendar_long[reason == "8", reason_recode := "inconvenient to use"]
  calendar_long[reason == "9", reason_recode := "infrequent sex/husband away"]
  calendar_long[reason == "C", reason_recode := "cost too much"]
  calendar_long[reason == "F", reason_recode := "up to god/fatalistic"]
  calendar_long[reason == "A", reason_recode := "difficult to get pregnant/menopause"]
  calendar_long[reason == "D", reason_recode := "marital dissolution/separation"]
  calendar_long[reason == "W", reason_recode := "other"]
  calendar_long[reason == "K", reason_recode := "don't know"]
  calendar_long[reason == "?", reason_recode := "missing"]
  
  
  
  ## PROCESS VARIABLES ---------------------------------------------
  message("||---Process variables")

  # calculate age for each month
  calendar_long[, age := as.integer((month_cmc - cmc_woman_dob)/12)]
  
  # identify use of modern methods
  calendar_long[, mod_contra := ifelse(grepl("sterilization|iud|injection|pill|condom|implant|foam|diaphragm|emergency|modern", event_recode), 1, 0)]
  
  # identify use of traditional methods
  calendar_long[, trad_contra := ifelse(grepl("rhythm|withdrawal|lactational amen|calendar|traditional", event_recode), 1, 0)]
  
  # create any_contra
  calendar_long[, any_contra := ifelse(mod_contra == 1 | trad_contra == 1, 1, 0)]
  
  # create outcome variable birth_preg
  calendar_long[, birth_preg := ifelse(grepl("birth|pregnancy|termination", event_recode), 1, 0)]
  
  # account for missing entries
  calendar_long[is.na(event_recode), c("mod_contra","trad_contra","any_contra","birth_preg") := NA]
  
  # group reasons for discontinuation (follows DHS groupings)
  calendar_long[, method_failure := ifelse(reason_recode == "became pregnant while using", 1, 0)]
  calendar_long[, desire_to_become_pregnant := ifelse(reason_recode == "wanted to become pregnant", 1, 0)]
  calendar_long[, other_fertility_related_reasons := ifelse(reason_recode %in% c("infrequent sex/husband away", "difficult to get pregnant/menopause", "marital dissolution/separation"), 1, 0)]
  calendar_long[, side_effects_health_concerns := ifelse(reason_recode %in% c("side effects/health concerns", "health concerns"), 1, 0)]
  calendar_long[, wanted_more_effective_method := ifelse(reason_recode == "wanted a more effective method", 1, 0)]
  calendar_long[, other_method_related := ifelse(reason_recode %in% c("access/availability", "inconvenient to use", "cost too much"), 1, 0)]
  calendar_long[, other_dk := ifelse(method_failure == 0 & desire_to_become_pregnant == 0 & other_fertility_related_reasons == 0 & side_effects_health_concerns == 0 &
                                wanted_more_effective_method == 0 & other_method_related == 0, 1, 0)]
  
  
  # EXPORT CLEANED DATA --------------------------------------------
  message("||---Export cleaned data")
  
  # save the cleaned data
  file_name <- gsub("\\.DTA", "", basename(survey))
  output_file_path <- file.path(out.dir, paste0(file_name, "_calendar_extract.csv"))
  write.csv(calendar_long, output_file_path, row.names = FALSE)
  
  message(paste0("Successfully saved ", file_name))
}


# RUN EXTRACTIONS ---------------------------------------------------

# Nepal
extract_data("/snfs1/DATA/DHS_PROG_DHS/NPL/2006/NPL_DHS5_2006_WN_NPIR51FL_Y2018M11D05.DTA", "np")
extract_data("/snfs1/DATA/DHS_PROG_DHS/NPL/2011/NPL_DHS6_2011_WN_NPIR60FL_Y2018M11D05.DTA", "np")
extract_data("/snfs1/DATA/DHS_PROG_DHS/NPL/2016_2017/NPL_DHS7_2016_2017_WN_NPIR7HFL_Y2018M11D05.DTA", "np")
extract_data("/snfs1/DATA/DHS_PROG_DHS/NPL/2022/NPL_DHS8_2022_WN_NPIR81FL_Y2023M06D23.DTA", "np")

# Ghana
extract_data("/snfs1/DATA/DHS_PROG_DHS/GHA/2008/GHA_DHS5_2008_WN_GHIR5AFL_Y2018M10D15.DTA", "gh")
extract_data("/snfs1/DATA/DHS_PROG_DHS/GHA/2014/GHA_DHS6_2014_WN_GHIR72FL_Y2018M12D10.DTA", "gh")
extract_data("/snfs1/DATA/DHS_PROG_DHS/GHA/2022_2023/GHA_DHS8_2022_2023_WN_GHIR8AFL_Y2024M01D22.DTA", "gh")

# Malawi
extract_data("/snfs1/DATA/DHS_PROG_DHS/MWI/2004_2005/MWI_DHS4_2004_2005_WN_MWIR4EFL_Y2018M12D10.DTA", "mw")
extract_data("/snfs1/DATA/DHS_PROG_DHS/MWI/2010/MWI_DHS6_2010_WN_MWIR61FL_Y2018M12D10.DTA", "mw")
extract_data("/snfs1/DATA/DHS_PROG_DHS/MWI/2015_2016/MWI_DHS7_2015_2016_WN_MWIR7AFL_Y2019M12D11.DTA", "mw")

# Rwanda
extract_data("/snfs1/DATA/DHS_PROG_DHS/RWA/2010_2011/RWA_DHS6_2010_2011_WN_RWIR61FL_Y2019M04D16.DTA", "rw")
extract_data("/snfs1/DATA/DHS_PROG_DHS/RWA/2014_2015/RWA_DHS7_2014_2015_WN_RWIR70FL_Y2019M04D16.DTA", "rw")
extract_data("/snfs1/DATA/DHS_PROG_DHS/RWA/2019_2020/RWA_DHS8_2019_2020_WN_RWIR81FL_Y2021M10D05.DTA", "rw")


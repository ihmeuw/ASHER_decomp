#-------------------Header------------------------------------------------
# Author: Olivia Angelino
# Project: ASHER
# Purpose: Extract pregnancy histories
# Date: 3/26/2024
# Notes:
#***************************************************************************

# SET-UP -----------------------------------------------------------

# clear environment
rm(list=ls())
username <- Sys.info()[["user"]]

if (Sys.info()["sysname"] == "Linux") {
  j <- "/snfs1/"
  h <- paste0("/homes/", username, "/")
  r <- "/mnt/"
  l <-"/ihme/limited_use/"
} else {
  j <- "J:/"
  h <- "H:/"
  r <- "R:/"
  l <- "L:/"
}

# load packages
pacman::p_load(magrittr,tidyverse,parallel,plyr,dplyr,haven,survey,tools,devtools,sf)

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
  
  
  # SURVEY CHARACTERISTICS --------------------------------------
  message("||---Survey characteristics")
  
  # convert variable names to lowercase
  names(dt) <- tolower(names(dt))
  
  # extract survey name and add to dt
  survey_name <- str_extract(survey, "[A-Z]{3}_DHS[0-9]_[0-9]{4}(_[0-9]{4})*")
  dt[, survey := survey_name]
  
  # identifiers
  dt[, country := cur_country]
  dt[, cluster := v001]
  dt[, hh_id := v002]
  dt[, id := v003] 

  # interview date
  dt[, cmc_interview_date := v008]
  
  # unique hh id for each household
  dt[, hhid_unique := paste(hh_id, cluster)]
  
  # unique individual id for each woman
  dt[, id_unique := paste(hhid_unique, id)]
  
  
  # EXTRACT FROM WOMEN'S FILE ----------------------------------------
  message("||---Extracting from women's file")
  
  if (grepl("_WN_", survey)) {
    
    id.vars <- c("survey","country","cluster","hh_id","id","cmc_interview_date","hhid_unique","id_unique")
    
    if (grepl("NPL_DHS[6-7]", survey)) {
      # pregnancy id
      dt_long <- melt(data = dt, id.vars = id.vars, measure.vars = patterns("^pidx97."), value.name = "preg_id") %>% as.data.table
      dt_long <- dt_long[!is.na(preg_id), -c("variable")]
      
      # pregnancy order number 
      tmp <- melt(data = dt, id.vars = id.vars, measure.vars = patterns("^pord97_."), value.name = "preg_order") %>% 
        mutate(preg_id = as.integer(sub("pord97_", "", variable))) %>% dplyr::select(-variable) %>% as.data.table
      dt_long <- merge(dt_long, tmp, by = c(id.vars, "preg_id"), all.x = T)
    } else if (grepl("NPL_DHS5", survey)) {
      # pregnancy id and order number 
      dt_long <- melt(data = dt, id.vars = id.vars, measure.vars = patterns("^bord92_."), value.name = "preg_order") %>% 
        mutate(preg_id = as.integer(sub("bord92_", "", variable))) %>% dplyr::select(-variable) %>% as.data.table
      dt_long <- dt_long[!is.na(preg_order)]
    } else if (grepl("NPL_DHS4", survey)) {
      # pregnancy id and order number 
      dt_long <- melt(data = dt, id.vars = id.vars, measure.vars = patterns("^bord9_."), value.name = "preg_order") %>% 
        mutate(preg_id = as.integer(sub("bord9_", "", variable))) %>% dplyr::select(-variable) %>% as.data.table
      dt_long <- dt_long[!is.na(preg_order)]
    }

    if (grepl("NPL_DHS7", survey)) {
      # CMC date pregnancy ended
      tmp <- melt(data = dt, id.vars = id.vars, measure.vars = patterns("^s215c_."), value.name = "preg_end_cmc") %>% 
        mutate(preg_id = as.integer(sub("s215c_", "", variable))) %>% dplyr::select(-variable) %>% as.data.table
      dt_long <- merge(dt_long, tmp, by = c(id.vars, "preg_id"), all.x = T)
      
    } else if (grepl("NPL_DHS6", survey)) {
      # CMC date of birth
      tmp <- melt(data = dt, id.vars = id.vars, measure.vars = patterns("^s220c_."), value.name = "preg_birth_cmc") %>% 
        mutate(preg_id = as.integer(sub("s220c_", "", variable))) %>% dplyr::select(-variable) %>% as.data.table
      dt_long <- merge(dt_long, tmp, by = c(id.vars, "preg_id"), all.x = T)
      
      # CMC date pregnancy ended
      tmp <- melt(data = dt, id.vars = id.vars, measure.vars = patterns("^s226c_."), value.name = "preg_end_cmc") %>% 
        mutate(preg_id = as.integer(sub("s226c_", "", variable))) %>% dplyr::select(-variable) %>% as.data.table
      dt_long <- merge(dt_long, tmp, by = c(id.vars, "preg_id"), all.x = T)
      
      # fill in preg_end_cmc with preg_birth_cmc when missing
      dt_long[is.na(preg_end_cmc), preg_end_cmc := preg_birth_cmc]
    } else if (grepl("NPL_DHS5", survey)) {
      # CMC date pregnancy ended or birth
      tmp <- melt(data = dt, id.vars = id.vars, measure.vars = patterns("^b3_x_."), value.name = "preg_end_cmc") %>% 
        mutate(preg_id = as.integer(sub("b3_x_", "", variable))) %>% dplyr::select(-variable) %>% as.data.table
      dt_long <- merge(dt_long, tmp, by = c(id.vars, "preg_id"), all.x = T)
    } else if (grepl("NPL_DHS4", survey)) {
      # CMC date pregnancy ended or birth
      tmp <- melt(data = dt, id.vars = id.vars, measure.vars = patterns("^b3_92_."), value.name = "preg_end_cmc") %>% 
        mutate(preg_id = as.integer(sub("b3_92_", "", variable))) %>% dplyr::select(-variable) %>% as.data.table
      dt_long <- merge(dt_long, tmp, by = c(id.vars, "preg_id"), all.x = T)
    }
    
    if (grepl("NPL_DHS[6-7]", survey)) {
      # duration of pregnancy in months
      tmp <- melt(data = dt, id.vars = id.vars, measure.vars = patterns("^s220a_"), value.name = "preg_duration") %>% 
        mutate(preg_id = as.integer(sub("s220a_", "", variable))) %>% dplyr::select(-variable) %>% as.data.table
      dt_long <- merge(dt_long, tmp, by = c(id.vars, "preg_id"), all.x = T)
    } else if (grepl("NPL_DHS5", survey)) {
      # duration of pregnancy in months
      tmp <- melt(data = dt, id.vars = id.vars, measure.vars = patterns("^s229_"), value.name = "preg_duration") %>% 
        mutate(preg_id = as.integer(sub("s229_", "", variable))) %>% dplyr::select(-variable) %>% as.data.table
      dt_long <- merge(dt_long, tmp, by = c(id.vars, "preg_id"), all.x = T)
    } else if (grepl("NPL_DHS4", survey)) {
      # duration of pregnancy in months
      tmp <- melt(data = dt, id.vars = id.vars, measure.vars = patterns("^s227_"), value.name = "preg_duration") %>% 
        mutate(preg_id = as.integer(sub("s227_", "", variable))) %>% dplyr::select(-variable) %>% as.data.table
      dt_long <- merge(dt_long, tmp, by = c(id.vars, "preg_id"), all.x = T)
    }

    # months since pregnancy outcome
    dt_long[, preg_months_since_outcome := cmc_interview_date - preg_end_cmc]
    
    if (grepl("NPL_DHS[6-7]", survey)) {
      # outcome of pregnancy
      tmp <- melt(data = dt, id.vars = id.vars, measure.vars = patterns("^sprego."), value.name = "preg_outcome") %>% 
        mutate(preg_id = as.integer(sub("sprego_", "", variable))) %>% dplyr::select(-variable) %>% as.data.table
      dt_long <- merge(dt_long, tmp[, preg_outcome := as.character(as_factor(preg_outcome))], by = c(id.vars, "preg_id"), all.x = T)
    } else if (grepl("NPL_DHS5", survey)) {
      # outcome of pregnancy
      tmp <- melt(data = dt, id.vars = id.vars, measure.vars = patterns("^s217_"), value.name = "preg_outcome") %>% 
        mutate(preg_id = as.integer(sub("s217_", "", variable))) %>% dplyr::select(-variable) %>% as.data.table
      dt_long <- merge(dt_long, tmp[, preg_outcome := as.character(as_factor(preg_outcome))], by = c(id.vars, "preg_id"), all.x = T)
    } else if (grepl("NPL_DHS4", survey)) {
      # outcome of pregnancy
      tmp <- melt(data = dt, id.vars = id.vars, measure.vars = patterns("^s216_"), value.name = "preg_outcome") %>% 
        mutate(preg_id = as.integer(sub("s216_", "", variable))) %>% dplyr::select(-variable) %>% as.data.table
      dt_long <- merge(dt_long, tmp[, preg_outcome := as.character(as_factor(preg_outcome))], by = c(id.vars, "preg_id"), all.x = T)
    }
    
    # wantedness of pregnancy
    tmp <- melt(data = dt, id.vars = id.vars, measure.vars = patterns("^m10_."), value.name = "preg_wanted") %>% 
      mutate(preg_id = as.integer(sub("m10_", "", variable))) %>% dplyr::select(-variable) %>% as.data.table
    dt_long <- merge(dt_long, tmp[, preg_wanted := as.character(as_factor(preg_wanted))], by = c(id.vars, "preg_id"), all.x = T)
    
    # desired timing of pregnancy
    tmp <- melt(data = dt, id.vars = id.vars, measure.vars = patterns("^m11_."), value.name = "preg_desired_timing") %>% 
      mutate(preg_id = as.integer(sub("m11_", "", variable))) %>% dplyr::select(-variable) %>% as.data.table
    dt_long <- merge(dt_long, tmp, by = c(id.vars, "preg_id"), all.x = T)
    
    dt <- copy(dt_long)
  }
 
 
  # EXTRACT FROM PREG FILE ------------------------------------------
  message("||---Extracting from pregnancy file")
  
  if (grepl("_PGR_", survey)) {
    # pregnancy id 
    dt[, preg_id := pidx]
    
    # pregnancy order number
    dt[, preg_order := pord]
    
    # CMC date pregnancy ended
    dt[, preg_end_cmc := p3]
    
    # duration of pregnancy in months
    dt[, preg_duration := p20]
    
    # months since pregnancy outcome
    dt[, preg_months_since_outcome := p19]
    
    # outcome of pregnancy 
    dt[, preg_outcome := as.character(as_factor(p32))]
    
    # wantedness of pregnancy
    dt[, preg_wanted := as.character(as_factor(m10))]
    
    # desired timing of pregnancy
    dt[, preg_desired_timing := m11]
  }
  
  
  # EXPORT CLEANED DATA ------------------------------------------
  message("||---Export cleaned data")
  
  # filter to variables of interest
  vars_interest <- c("survey","country","cluster","hh_id","id","cmc_interview_date","hhid_unique","id_unique",
                     "preg_id","preg_order","preg_end_cmc","preg_duration","preg_months_since_outcome",
                     "preg_outcome","preg_wanted","preg_desired_timing")
  cur_vars <- names(dt)[names(dt) %in% vars_interest]
  dt_filter <- dt[, ..cur_vars]
  
  # save the cleaned data
  file_name <- gsub("\\.DTA", "", basename(survey))
  output_file_path <- file.path(out.dir, paste0(file_name, "_preg_extract.csv"))
  write.csv(dt_filter, output_file_path, row.names = FALSE)
  
  message(paste0("Successfully saved ", file_name))
}


# RUN EXTRACTIONS ------------------------------------------------

# Ghana
extract_data("/snfs1/DATA/DHS_PROG_DHS/GHA/2022_2023/GHA_DHS8_2022_2023_PGR_GHGR8AFL_Y2024M01D22.DTA", "gh")

# Nepal
extract_data("/snfs1/DATA/DHS_PROG_DHS/NPL/2001/NPL_DHS4_2001_WN_NPIR41FL_Y2019M02D19.DTA", "np")
extract_data("/snfs1/DATA/DHS_PROG_DHS/NPL/2006/NPL_DHS5_2006_WN_NPIR51FL_Y2018M11D05.DTA", "np")
extract_data("/snfs1/DATA/DHS_PROG_DHS/NPL/2011/NPL_DHS6_2011_WN_NPIR60FL_Y2018M11D05.DTA", "np")
extract_data("/snfs1/DATA/DHS_PROG_DHS/NPL/2016_2017/NPL_DHS7_2016_2017_WN_NPIR7HFL_Y2018M11D05.DTA", "np")
extract_data("/snfs1/DATA/DHS_PROG_DHS/NPL/2022/NPL_DHS8_2022_PGR_NPGR81FL_Y2023M06D23.DTA", "np")

#-------------------Header------------------------------------------------
# Author: NAME 
# Project: ASHER
# Purpose: Calculate means in covariates and outcomes by admin 1
# Notes:
#***************************************************************************

# SET-UP -----------------------------------------------------------

# load packages
pacman::p_load(magrittr,tidyverse,parallel,plyr,dplyr,haven,survey,tools,devtools,sf)

# in/out
out.dir <- 'FILEPATH'

# create function to convert CMC date to year and month
get_cmc_year <- function(x) 1900 + as.integer((as.integer(x) - 1) / 12)
get_cmc_month <- function(x) as.integer(x) - 12 * (get_cmc_year(x) - 1900)

files <- list.files(file.path(out.dir, "01_processed"))
gps_files <- files[grepl("gps", files)]

get_admin_1_means <- function(dt_file, cur_country){
  dt <- fread(file.path('FILEPATH', dt_file))
  
  ## rename wealth quintiles to wealth index
  setnames(dt, "wealth_quintiles", "wealth_index")
  ## make wealth index categorical as it is specified in ob
  dt[, wealth_dummies1 := ifelse(wealth_index ==1, 1, 0)]
  dt[, wealth_dummies2 := ifelse(wealth_index ==2, 1, 0)]
  dt[, wealth_dummies3 := ifelse(wealth_index ==3, 1, 0)]
  dt[, wealth_dummies4 := ifelse(wealth_index ==4, 1, 0)]
  dt[, wealth_dummies5 := ifelse(wealth_index ==5, 1, 0)]
  
  # read in gps data
  gps_files_tmp <- gps_files[grepl(cur_country, gps_files)]
  gps_data <- rbindlist(lapply(file.path(out.dir, "01_processed", gps_files_tmp), fread), fill = T)
  
  ## Create outcome c------------
  ## construct outcome to use with dhs endline: in the last 2 years or currently preg, include terminations 
  dt[, term_2_years := ifelse(term_recall<24, 1, 0)] # termination within 2 years of interview
  dt[grepl("DHS", survey) & (any_births_2_yr ==1 | curr_preg ==1 | term_2_years ==1), outcome_c_dhs := 1]
  dt[grepl("DHS", survey) & (any_births_2_yr == 0 & curr_preg ==0 & (is.na(term_2_years) |  term_2_years == 0)), outcome_c_dhs := 0]
  
  # construct outcome to use with mics endline: in the last 2 years or currently preg, does not include terminations 
  dt[any_births_2_yr ==1 | curr_preg ==1 , outcome_c_mics := 1]
  dt[any_births_2_yr == 0 & curr_preg ==0, outcome_c_mics := 0]
  
  # re code had_intercourse to only capture sexual activity among unmarried women 
  # if a woman is married, we set had_intercourse to 0
  # if a woman is unmarried, it remains how it was
  dt[curr_cohabit == 1 & !is.na(had_intercourse), had_intercourse_unmarried := 0]
  dt[curr_cohabit == 0 & !is.na(had_intercourse), had_intercourse_unmarried := had_intercourse]
  
  ## adjust years
  dt[country == "np", year := get_cmc_year(cmc_interview_date - ((56*12)+8))]
  dt[, year := min(year), by = "survey"]
  
  # variables of interest 
  variables <- c("country", 'year','cluster', 'pweight', 'survey', 'outcome_c_dhs', 'outcome_c_mics', 
                 'unmet_need', 'wealth_dummies1', 'wealth_dummies2', 'wealth_dummies3',
                 'wealth_dummies4', 'wealth_dummies5', 'age', 'curr_cohabit','beating_just','educ_single_yrs','had_intercourse_unmarried'
                 )
  dt <- dt[,..variables]
  dt <- dt[age %in% c(seq(15,19))]
  # list of variables to take the mean of 
  variables_means <- c('outcome_c_dhs', 'outcome_c_mics', 
                       'unmet_need', 'wealth_dummies1', 'wealth_dummies2', 'wealth_dummies3',
                       'wealth_dummies4', 'wealth_dummies5', 'age', 'curr_cohabit','beating_just','educ_single_yrs','had_intercourse_unmarried')
  
  # merge data onto gps data
  dt <- merge(dt, gps_data, by = c("country", "cluster","survey"))
  
  # create survey object 
  data_svy <- svydesign(id= ~cluster,  weights = ~pweight, data = dt, nest = TRUE)
  
  # take survey weighted mean of each variable by year, survey, and admin 1
  means_dt <- data.table()
  for (var in variables_means){
    means_tmp <- svyby(~get(var), ~year+survey+admin_1_shp, data_svy , svymean,vartype = c("se", "ci"), na.rm=T) 
    means_tmp <- data.table(means_tmp)
    setnames(means_tmp, "get(var)", "mean")
    means_tmp[, variable := eval(var)]
    means_dt <- rbind(means_dt, means_tmp)
  }
  
  # save csv
  write.csv(means_dt, file.path(out.dir, '01_processed', paste0(cur_country, '_admin_1_means.csv')), row.names=F)
}

get_admin_1_means("cm_all_yrs_prepped.csv", "CMR")
get_admin_1_means("gh_all_yrs_prepped.csv", "GHA")
get_admin_1_means("mw_all_yrs_prepped.csv", "MWI")
get_admin_1_means("np_all_yrs_prepped.csv", "NPL")
get_admin_1_means("rw_all_yrs_prepped.csv", "RWA")



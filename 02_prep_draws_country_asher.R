#-------------------Header------------------------------------------------
## Created by: NAME
## Prep data draws for oaxaca-blinder decomp for ASHER: by country (for manual OB)

## set-up --------
pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey,tidyr,glmnet, ggpattern)

'%ni%' <- Negate('%in%')

## directories    
in.date <-'DATE'
date <- Sys.Date()
in.dir <- data_dir <- file.path('FILEPATH', in.date)
out.dir <-  file.path('FILEPATH', date)
dir.create(out.dir)

# Create draws for other variables --------------------------------------------------
# function to create draws 
create_draws <- function(cur_country,age_group='15_19', outcome='outcome_c'){
  mode_use <- ifelse(cur_country != "mw", "dhs", "mics")
  if (outcome == 'full_sensitivity_outcome_c'){
    if (cur_country != 'mw'){
      variables <- c('outcome_c_dhs', 'unmet_need', 'wealth_dummies1', 'wealth_dummies2', 'wealth_dummies3',
                     'wealth_dummies4', 'wealth_dummies5', 'age', 'curr_cohabit','beating_just','educ_single_yrs','had_intercourse_unmarried',
                     'mod_contra', 'urban', 'mean_yrs_schooling_head')
    } else{
      variables <- c('outcome_c_mics', 'unmet_need', 'wealth_dummies1', 'wealth_dummies2', 'wealth_dummies3',
                     'wealth_dummies4', 'wealth_dummies5', 'age', 'curr_cohabit','beating_just','educ_single_yrs','had_intercourse_unmarried',
                     'mod_contra', 'urban', 'mean_yrs_schooling_head')
    }
  } else{
    if (cur_country != "mw"){
      variables <- c(paste0(outcome, "_dhs"),
                     'unmet_need', 'wealth_dummies1', 'wealth_dummies2', 'wealth_dummies3',
                     'wealth_dummies4', 'wealth_dummies5', 'age', 'curr_cohabit','beating_just','educ_single_yrs','had_intercourse_unmarried',
                     'mod_contra')
    } else{
      variables <- c(paste0(outcome, "_mics"),
                     'unmet_need', 'wealth_dummies1', 'wealth_dummies2', 'wealth_dummies3',
                     'wealth_dummies4', 'wealth_dummies5', 'age', 'curr_cohabit','beating_just','educ_single_yrs','had_intercourse_unmarried',
                     'mod_contra')
    }
  }
  
  
  data_dt <- data.table(fread(file.path(in.dir, paste0(cur_country, "_", mode_use, "_", age_group, "_", outcome, "_baseline_endline_diff_upper_lower.csv"))))
  # calculate standard error
  data_dt[, se := (ci_u - ci_l)/3.92]
  
  data_dt <- data_dt[, .(country, variable, mean_diff, se)]
  
  # now create empty diff dt (diff between draws for endline and baseline)
  draws_diff <- data.table()

  for (var in variables){
    tmp_draws <- data.table()
    tmp_dt <- data_dt[variable == var]
    print(var)
    tmp_diff <- rnorm(n = 1000, mean = tmp_dt$mean_diff, sd = tmp_dt$se)
    tmp_draws[, eval(var) := tmp_diff]
    draws_diff <- cbind(tmp_draws, draws_diff)
      
  }
  draws_diff[, country := cur_country]
  write.csv(draws_diff, file.path(out.dir, paste0(cur_country, '_', outcome, '_draws.csv')), row.names=F)
}

create_draws('cm', outcome='outcome_c')
create_draws('gh',  outcome='outcome_c')
create_draws('mw',  outcome='outcome_c')
create_draws('np',  outcome='outcome_c')
create_draws('rw',  outcome='outcome_c')

create_draws('cm',  outcome='outcome_b')
create_draws('gh', outcome= 'outcome_b')
create_draws('mw', outcome= 'outcome_b')
create_draws('np', outcome= 'outcome_b')
create_draws('rw', outcome= 'outcome_b')

create_draws('cm',  outcome='full_sensitivity_outcome_c')
create_draws('gh', outcome= 'full_sensitivity_outcome_c')
create_draws('mw', outcome= 'full_sensitivity_outcome_c')
create_draws('np', outcome= 'full_sensitivity_outcome_c')
create_draws('rw', outcome= 'full_sensitivity_outcome_c')

# Create draws for only sexually active women  --------------------------------------------------
# function to create draws 
create_draws_part_2 <- function(cur_country,age_group='15_19', outcome='outcome_c'){
  mode_use <- ifelse(cur_country != "mw", "dhs", "mics")
  if (cur_country != "mw"){
    variables <- c('outcome_c_dhs',   'unmet_need', 'wealth_dummies1', 'wealth_dummies2', 'wealth_dummies3',
                   'wealth_dummies4', 'wealth_dummies5', 'age', 'curr_cohabit','beating_just','educ_single_yrs','had_intercourse_unmarried','age_1st_sex_imp')
  } else{
    variables <- c('outcome_c_mics',  'unmet_need', 'wealth_dummies1', 'wealth_dummies2', 'wealth_dummies3',
                   'wealth_dummies4', 'wealth_dummies5', 'age', 'curr_cohabit','beating_just','educ_single_yrs','had_intercourse_unmarried','age_1st_sex_imp')
  }
  
  data_dt <- data.table(fread(file.path(in.dir, paste0(cur_country, "_", mode_use, "_", age_group, "_part_2_", outcome, ".csv"))))
  # calculate standard error
  data_dt[, se := (ci_u - ci_l)/3.92]
  
  data_dt <- data_dt[, .(country, variable, mean_diff, se)]
  
  # now create empty diff dt (diff between draws for endline and baseline)
  draws_diff <- data.table()
  
  for (var in variables){
    tmp_draws <- data.table()
    tmp_dt <- data_dt[variable == var]
    print(var)
    tmp_diff <- rnorm(n = 1000, mean = tmp_dt$mean_diff, sd = tmp_dt$se)
    tmp_draws[, eval(var) := tmp_diff]
    draws_diff <- cbind(tmp_draws, draws_diff)
    
  }
  draws_diff[, country := cur_country]
  write.csv(draws_diff, file.path(out.dir, paste0(cur_country, '_draws_part_2.csv')), row.names=F)
}

create_draws_part_2('cm')
create_draws_part_2('gh')
create_draws_part_2('mw')
create_draws_part_2('np')
create_draws_part_2('rw')

# Create draws for only sexually active women with contra imputation  --------------------------------------------------
# function to create draws 
create_draws_part_2_contra <- function(cur_country,age_group='15_19', outcome='outcome_c'){
  mode_use <-'dhs' # dhs only for contra imputation
  variables <- c('outcome_c_dhs',   'mod_contra', 'wealth_dummies1', 'wealth_dummies2', 'wealth_dummies3',
                 'wealth_dummies4', 'wealth_dummies5', 'age', 'curr_cohabit','beating_just','educ_single_yrs','had_intercourse_unmarried','age_1st_sex_imp')
  
  
  data_dt <- data.table(fread(file.path(in.dir, paste0('diff_means_', cur_country, "_", mode_use, "_", age_group, "_", outcome, "_mod_contra_imputed_part_2.csv"))))
  # calculate standard error
  data_dt[, se := (ci_u - ci_l)/3.92]
  
  data_dt <- data_dt[, .(country, variable, mean_diff, se)]
  
  # now create empty diff dt (diff between draws for endline and baseline)
  draws_diff <- data.table()
  
  for (var in variables){
    tmp_draws <- data.table()
    tmp_dt <- data_dt[variable == var]
    print(var)
    tmp_diff <- rnorm(n = 1000, mean = tmp_dt$mean_diff, sd = tmp_dt$se)
    tmp_draws[, eval(var) := tmp_diff]
    draws_diff <- cbind(tmp_draws, draws_diff)
    
  }
  draws_diff[, country := cur_country]
  write.csv(draws_diff, file.path(out.dir, paste0(cur_country, '_draws_part_2_contra.csv')), row.names=F)
}

create_draws_part_2_contra('gh')
create_draws_part_2_contra('mw')
create_draws_part_2_contra('np')
create_draws_part_2_contra('rw')

# Create draws for sensitivity analysis women dropped if married after preg  --------------------------------------------------
# function to create draws 
create_draws_rtr_marriage_preg <- function(cur_country,age_group='15_19', outcome='marriage_preg_drop_outcome_c'){
  mode_use <- ifelse(cur_country != "mw", "dhs", "mics")
  
  if (mode_use == "mics"){
    variables <- c("outcome_c_mics",
                 'unmet_need', 'wealth_dummies1', 'wealth_dummies2', 'wealth_dummies3',
                 'wealth_dummies4', 'wealth_dummies5', 'age', 'curr_cohabit','beating_just','educ_single_yrs','had_intercourse_unmarried')
  } else{
    variables <- c("outcome_c_dhs",
                   'unmet_need', 'wealth_dummies1', 'wealth_dummies2', 'wealth_dummies3',
                   'wealth_dummies4', 'wealth_dummies5', 'age', 'curr_cohabit','beating_just','educ_single_yrs','had_intercourse_unmarried')
  }

  data_dt <- data.table(fread(file.path(in.dir, paste0(cur_country, "_", mode_use, "_", age_group, "_", outcome, ".csv"))))

  # calculate standard error
  data_dt[, se := (ci_u - ci_l)/3.92]
  
  data_dt <- data_dt[, .(country, variable, mean_diff, se)]
  
  # now create empty diff dt (diff between draws for endline and baseline)
  draws_diff <- data.table()
  
  for (var in variables){
    tmp_draws <- data.table()
    tmp_dt <- data_dt[variable == var]
    print(var)
    tmp_diff <- rnorm(n = 1000, mean = tmp_dt$mean_diff, sd = tmp_dt$se)
    tmp_draws[, eval(var) := tmp_diff]
    draws_diff <- cbind(tmp_draws, draws_diff)
    
  }
  draws_diff[, country := cur_country]
  write.csv(draws_diff, file.path(out.dir, paste0(cur_country, '_', outcome, '_draws.csv')), row.names=F)
}

create_draws_rtr_marriage_preg('gh')
create_draws_rtr_marriage_preg('mw')
create_draws_rtr_marriage_preg('np')
create_draws_rtr_marriage_preg('rw')
create_draws_rtr_marriage_preg('cm')

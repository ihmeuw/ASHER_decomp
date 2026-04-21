#-------------------Header------------------------------------------------

## Created by: NAME
## Prep coefficient draws for oaxaca-blinder decomp for ASHER
# specify by outcome and coef group

## set-up --------
pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey,tidyr,glmnet, ggpattern)

'%ni%' <- Negate('%in%')
# date of ob run for coefficients to use 
ob.in.date <- 'DATE'
date <- Sys.Date()

## directories    
in.dir <-  file.path('FILEPATH', ob.in.date)
out.dir <-  file.path('FILEPATH',date)
dir.create(out.dir)

# function to create draws 
create_draws <- function(cur_country, coef_group, outcome){
  # specify endline data type
  if (cur_country == 'mw'){
    endline <- 'mics'
  } else {
    endline <- 'dhs'
  }
  
  # read in coefficients from OB run 
  coef_dt <- fread(file.path(in.dir, paste0("alt_", endline, "_oaxaca_15_19_ci_detail_", coef_group, "_", outcome, '_', cur_country, ".csv")), fill = T)
  
  names(coef_dt) <- c("variable", "lower", "upper")
  coef_dt <- coef_dt[rowSums(coef_dt == "" | is.na(coef_dt)) != ncol(coef_dt)]
  
  ## extract mean value from data
  coef_dt_mean_only <- coef_dt[seq(1, .N, by = 2), -c("upper")]
  names(coef_dt_mean_only) <- c("variable", "mean")
  coef_dt_mean_only[, significance := ifelse(grepl("\\*+", mean), "significant", "insignificant")]
  
  coef_dt_mean_only <- coef_dt_mean_only[, variable := gsub('["=\\*]', '', variable)]
  coef_dt_mean_only <- coef_dt_mean_only[, mean := gsub('["=\\*]', '', mean)]
  
  if (outcome == 'full_sensitivity_outcome_c'){
    coef_dt_mean_only <- coef_dt_mean_only[2:13]
  } else{
    coef_dt_mean_only <- coef_dt_mean_only[2:11]
  }

  coef_dt_ci_only <- coef_dt[seq(2, .N, by = 2)]
  coef_dt_ci_only <- coef_dt_ci_only[, lower := gsub('["=\\*\\]', '', lower)]
  coef_dt_ci_only <- coef_dt_ci_only[, lower := gsub('\\[', '', lower)]
  
  coef_dt_ci_only <- coef_dt_ci_only[, upper := gsub('["=\\*\\]', '', upper)]
  coef_dt_ci_only <- coef_dt_ci_only[, upper := gsub('\\]', '', upper)]
  
  if (outcome == 'full_sensitivity_outcome_c'){
    coef_dt_ci_only <- coef_dt_ci_only[2:13]
  } else{
    coef_dt_ci_only <- coef_dt_ci_only[2:11]
  }
  
  coef_dt_ci_only[, variable := unique(coef_dt_mean_only$variable)]
  
  coef_dt <- merge(coef_dt_mean_only, coef_dt_ci_only, by = "variable")
  coef_dt[, catg := 'coef']
  
  coef_dt[, mean := as.numeric(mean)]
  coef_dt[, upper := as.numeric(upper)]
  coef_dt[, lower := as.numeric(lower)]
  
  # calculate SD which we will need to create draws
  # assume upper - mean is 1.96 SD, and mean - lower is 1.96 sd, and take mean of these two values
  coef_dt[, sd_upper := (upper - mean)/1.96]
  coef_dt[, sd_lower := (mean-lower)/1.96]
  coef_dt[, sd := (sd_upper + sd_lower)/2]
  # now remove sd upper and sd lower columns
  coef_dt <- coef_dt[, -c("sd_upper", "sd_lower")]
    
  coef_draws_dt <- data.table()
  for (var in unique(coef_dt$variable)){
      print(var)
      tmp_dt <- coef_dt[variable == eval(var)]
      tmp_draws <- data.table(rnorm(1000, tmp_dt$mean, tmp_dt$sd))
      names(tmp_draws) <- eval(var)
      coef_draws_dt <- cbind(coef_draws_dt, tmp_draws)
    }
  
  coef_draws_dt[, country := cur_country]
  write.csv(coef_draws_dt, file.path(out.dir, paste0(cur_country, '_coef_draws_', coef_group, '_', outcome, '.csv')), row.names=F)
}

create_draws('cm', 'endline', 'outcome_c')
create_draws('gh', 'endline', 'outcome_c')
create_draws('mw', 'endline', 'outcome_c')
create_draws('np', 'endline', 'outcome_c')
create_draws('rw', 'endline', 'outcome_c')

create_draws('cm', 'endline', 'outcome_b')
create_draws('gh', 'endline', 'outcome_b')
create_draws('mw', 'endline', 'outcome_b')
create_draws('np', 'endline', 'outcome_b')
create_draws('rw', 'endline', 'outcome_b')

create_draws('cm', 'pooled', 'outcome_c')
create_draws('gh', 'pooled', 'outcome_c')
create_draws('mw', 'pooled', 'outcome_c')
create_draws('np', 'pooled', 'outcome_c')
create_draws('rw', 'pooled', 'outcome_c')

create_draws('cm', 'endline', 'full_sensitivity_outcome_c')
create_draws('gh', 'endline', 'full_sensitivity_outcome_c')
create_draws('mw', 'endline', 'full_sensitivity_outcome_c')
create_draws('np', 'endline', 'full_sensitivity_outcome_c')
create_draws('rw', 'endline', 'full_sensitivity_outcome_c')

create_draws('cm', 'endline', 'mics_outcome_c')
create_draws('gh', 'endline', 'mics_outcome_c')
create_draws('np', 'endline', 'mics_outcome_c')
create_draws('rw', 'endline', 'mics_outcome_c')


# function to create draws: among sexually active women only: just run for outcome c 
create_draws_part_2 <- function(cur_country, coef_group, outcome){
  # specify endline data type
  if (cur_country == 'mw'){
    endline <- 'mics'
  } else {
    endline <- 'dhs'
  }
  
  # read in coefficients from OB run 
  coef_dt <- fread(file.path(in.dir, paste0("alt_", endline, "_oaxaca_15_19_ci_detail_endline_part_2_", outcome, '_', cur_country, ".csv")), fill = T)

  names(coef_dt) <- c("variable", "lower", "upper")
  coef_dt <- coef_dt[rowSums(coef_dt == "" | is.na(coef_dt)) != ncol(coef_dt)]
  
  ## extract mean value from data
  coef_dt_mean_only <- coef_dt[seq(1, .N, by = 2), -c("upper")]
  names(coef_dt_mean_only) <- c("variable", "mean")
  coef_dt_mean_only[, significance := ifelse(grepl("\\*+", mean), "significant", "insignificant")]
  
  coef_dt_mean_only <- coef_dt_mean_only[, variable := gsub('["=\\*]', '', variable)]
  coef_dt_mean_only <- coef_dt_mean_only[, mean := gsub('["=\\*]', '', mean)]
  
  coef_dt_mean_only <- coef_dt_mean_only[2:11]
  
  coef_dt_ci_only <- coef_dt[seq(2, .N, by = 2)]
  coef_dt_ci_only <- coef_dt_ci_only[, lower := gsub('["=\\*\\]', '', lower)]
  coef_dt_ci_only <- coef_dt_ci_only[, lower := gsub('\\[', '', lower)]
  
  coef_dt_ci_only <- coef_dt_ci_only[, upper := gsub('["=\\*\\]', '', upper)]
  coef_dt_ci_only <- coef_dt_ci_only[, upper := gsub('\\]', '', upper)]
  
  coef_dt_ci_only <- coef_dt_ci_only[2:11]
  
  coef_dt_ci_only[, variable := unique(coef_dt_mean_only$variable)]
  
  coef_dt <- merge(coef_dt_mean_only, coef_dt_ci_only, by = "variable")
  coef_dt[, catg := 'coef']
  
  coef_dt[, mean := as.numeric(mean)]
  coef_dt[, upper := as.numeric(upper)]
  coef_dt[, lower := as.numeric(lower)]
  
  # calculate SD which we will need to create draws
  # assume upper - mean is 1.96 SD, and mean - lower is 1.96 sd, and take mean of these two values
  coef_dt[, sd_upper := (upper - mean)/1.96]
  coef_dt[, sd_lower := (mean-lower)/1.96]
  coef_dt[, sd := (sd_upper + sd_lower)/2]
  # now remove sd upper and sd lower columns
  coef_dt <- coef_dt[, -c("sd_upper", "sd_lower")]
  
  coef_draws_dt <- data.table()
  for (var in unique(coef_dt$variable)){
    print(var)
    tmp_dt <- coef_dt[variable == eval(var)]
    tmp_draws <- data.table(rnorm(1000, tmp_dt$mean, tmp_dt$sd))
    names(tmp_draws) <- eval(var)
    coef_draws_dt <- cbind(coef_draws_dt, tmp_draws)
  }
  
  coef_draws_dt[, country := cur_country]
  write.csv(coef_draws_dt, file.path(out.dir, paste0(cur_country, '_coef_draws_', coef_group, '_', outcome, '_part_2.csv')), row.names=F)
}

create_draws_part_2('cm', 'endline', 'outcome_c')
create_draws_part_2('gh', 'endline', 'outcome_c')
create_draws_part_2('mw', 'endline', 'outcome_c')
create_draws_part_2('np', 'endline', 'outcome_c')
create_draws_part_2('rw', 'endline', 'outcome_c')

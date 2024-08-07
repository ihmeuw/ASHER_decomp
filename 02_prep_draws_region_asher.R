#-------------------Header------------------------------------------------
# Project: IHME ASHER Decomposition
# Creation date: 6/23/2024
# Prep data draws for oaxaca-blinder decomp for ASHER: by region by country (sensitivity analysis)

# SET-UP ----------------------------------------------------------

# clear memory
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

## set-up --------
pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey,tidyr,glmnet, ggpattern)

'%ni%' <- Negate('%in%')

## directories    
in.dir <- 'FILEPATH'
out.dir <-  file.path('FILEPATH', Sys.Date())
dir.create(out.dir)

## read in list of variable names
variable_list <- data.table(read_xlsx('FILEPATH/variable_availability.xlsx'))
variable_list <- variable_list[DHS_phase1 == 'Y']

# Create draws for other variables --------------------------------------------------
# function to create draws 
create_draws <- function(cur_country, admin_1_dt_file, endline){
  if (endline == "DHS"){
    variables <- c(variable_list$Variable, 'any_birth_preg_2_yr_dhs','wealth_dummies1', 'wealth_dummies2', 'wealth_dummies3', 'wealth_dummies4', 'wealth_dummies5')
  } else{
    variables <- c(variable_list$Variable, 'any_birth_preg_2_yr_mics','wealth_dummies1', 'wealth_dummies2', 'wealth_dummies3', 'wealth_dummies4', 'wealth_dummies5')
  }
  
  data_dt <- data.table(fread(file.path(in.dir, admin_1_dt_file)))
  ## label baseline so we can group data for oaxaca-blinder and filter to baseline-endline data
  data_dt[, latest := max(year)]
  data_dt[, earliest := min(year)]
  data_dt[, baseline := ifelse(year == earliest,1,0)]
  data_dt[, endline := ifelse(year == latest, 1,0)]
  data_dt <- data_dt[baseline ==1 | endline == 1]
  
  # now create empty diff dt (diff between draws for endline and baseline)
  draws_diff <- data.table()

  # calculate SD which we will need to create draws
  # assume upper - mean is 1.96 SD, and mean - lower is 1.96 sd, and take mean of these two values
  setnames(data_dt, c("ci_l", "ci_u"), c("lower", "upper"))
  data_dt[, sd_upper := (upper - mean)/1.96]
  data_dt[, sd_lower := (mean-lower)/1.96]
  data_dt[, sd := (sd_upper + sd_lower)/2]
  # now remove sd upper and sd lower columns
  data_dt <- data_dt[, -c("sd_upper", "sd_lower")]
    
  for (region in unique(data_dt$admin_1_shp)){
    print(region)
    tmp_draws_diff <- data.table()
    
    for (var in variables){
      print(var)
      # baseline
      tmp_baseline <- data_dt[baseline ==1 & variable == eval(var) & admin_1_shp == eval(region)]
      tmp_draws_baseline <- data.table(rnorm(1000, tmp_baseline$mean, tmp_baseline$sd))
      names(tmp_draws_baseline) <- eval(var)
      
       # endline 
      tmp_endline <- data_dt[endline ==1 & variable == eval(var) & admin_1_shp == eval(region)]
      tmp_draws_endline <- data.table(rnorm(1000, tmp_endline$mean, tmp_endline$sd))
      names(tmp_draws_endline) <- eval(var)
      
      diff_df <- data.table()
      print("Calculating difference...")
      tmp_diff <- as.numeric(tmp_draws_endline[,get(eval(var))]) - as.numeric(tmp_draws_baseline[,get(eval(var))])
      diff_df[, eval(var) := tmp_diff]
      
      tmp_draws_diff <- cbind(tmp_draws_diff, diff_df)
      
    }
  
    tmp_draws_diff[, admin_1_region := region]
    draws_diff <- rbind(draws_diff, tmp_draws_diff)
    
  }
  draws_diff[, country := cur_country]
  write.csv(draws_diff, file.path(out.dir, paste0(cur_country, '_region_draws.csv')), row.names=F)
}

create_draws('cm', 'CMR_admin_1_means.csv', 'DHS')
create_draws('gh', 'GHA_admin_1_means.csv', 'DHS')
create_draws('mw', 'MWI_admin_1_means.csv', 'MICS')
create_draws('np', 'NPL_admin_1_means.csv', 'DHS')
create_draws('rw', 'RWA_admin_1_means.csv', 'DHS')
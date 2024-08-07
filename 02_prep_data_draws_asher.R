#-------------------Header------------------------------------------------

# Project: IHME ASHER Decomposition
# Creation date: 6/26/2024
# Prep data draws from baseline and endline for oaxaca-blinder decomp for ASHER to use in analysis using pooled data for regression (sensitivity analysis)
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

# date that data was prepped for ob
ob.in.date <- "2024-06-20"

## directories    
in.dir <- file.path('FILEPATH', ob.in.date)
out.dir <-  file.path('FILEPATH', Sys.Date())
dir.create(out.dir)

## read in list of variable names
variable_list <- data.table(read_xlsx('FILEPATH/variable_availability.xlsx'))
variable_list <- variable_list[DHS_phase1 == 'Y']

# read in prepped ob data 
ob_dhs_dt <- fread(file.path(in.dir, 'ob_input_prepped_df_dhs.csv'))
ob_mics_dt <- fread(file.path(in.dir, 'ob_input_prepped_df_mics.csv'))

id.vars <- c("country", "cluster", "psu_unique", "year", "pweight", "baseline")

# Create draws for other variables --------------------------------------------------
# function to create draws 
create_draws <- function(cur_country){
  variables <- c(variable_list$Variable, 'wealth_dummies1', 'wealth_dummies2', 'wealth_dummies3', 'wealth_dummies4', 'wealth_dummies5')

  if (cur_country == "mw") {
    data_dt <- ob_mics_dt[country == cur_country & age %in% c(seq(15,19))]
  } else{
    data_dt <- ob_dhs_dt[country == cur_country & age %in% c(seq(15,19))]
  }
  
  ## label baseline so we can group data for oaxaca-blinder and filter to baseline-endline data
  data_dt[, latest := max(year)]
  data_dt[, earliest := min(year)]
  data_dt[, baseline := ifelse(year == earliest,1,0)]
  data_dt[, endline := ifelse(year == latest, 1,0)]
  data_dt <- data_dt[baseline ==1 | endline == 1]
  
  data_dt <- data_dt[, -c("latest", "earliest", "endline")]
  # create survey object 
  data_svy <- svydesign(id= ~cluster,  weights = ~pweight, data = data_dt, nest = TRUE)
  
  # now create empty diff dt (diff between draws for endline and baseline)
  draws_diff <- data.table()

  for (var in variables){
    print(var)
    # select_vars <- c(id.vars, var)
    # tmp_dt <- data_dt[, ..select_vars]
   
     # calculate survey weighted SD and mean which we will need to create draws
    means_tmp <- data.table(svyby(~get(var), ~baseline, data_svy , svymean,vartype = c("se", "ci"), na.rm=T))
    setnames(means_tmp, "get(var)", "mean")

      # baseline
      tmp_baseline <- means_tmp[baseline == 1]
      tmp_draws_baseline <- data.table(rnorm(1000, tmp_baseline$mean, tmp_baseline$se))
      names(tmp_draws_baseline) <- eval(var)
      
       # endline 
      tmp_endline <-  means_tmp[baseline == 0]
      tmp_draws_endline <- data.table(rnorm(1000, tmp_endline$mean, tmp_endline$se))
      names(tmp_draws_endline) <- eval(var)
      
      diff_df <- data.table()
      print("Calculating difference...")
      tmp_diff <- as.numeric(tmp_draws_endline[,get(eval(var))]) - as.numeric(tmp_draws_baseline[,get(eval(var))])
      diff_df[, eval(var) := tmp_diff]
      
      draws_diff <- cbind(draws_diff, diff_df)
      
    }
  
    draws_diff[, country := cur_country]
    write.csv(draws_diff, file.path(out.dir, paste0(cur_country, '_baseline_endline_draws.csv')), row.names=F)
}

create_draws('cm')
create_draws('gh')
create_draws('mw')
create_draws('np')
create_draws('rw')

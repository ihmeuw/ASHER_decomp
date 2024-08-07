#-------------------Header------------------------------------------------

# Project: IHME ASHER Decomposition
# Creation date: 6/26/2024
# Prep coefficient draws for oaxaca-blinder decomp for ASHER: using pooled data (sensitivity analysis)

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
# date of ob run for coefficients to use 
ob.in.date <- '2024-06-26'

## directories    
in.dir <-  file.path('FILEPATH', ob.in.date)
out.dir <-  file.path('FILEPATH', Sys.Date())
dir.create(out.dir)

# function to create draws 
create_draws <- function(cur_country){
  # specify endline data type
  if (cur_country == 'mw'){
    endline <- 'mics'
  } else {
    endline <- 'dhs'
  }
  
  # read in coefficients from OB run 
  coef_dt <- fread(file.path(in.dir, paste0(endline, "_oaxaca_15_19_pooled_reg_", cur_country, ".csv")), fill = T)
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
  write.csv(coef_draws_dt, file.path(out.dir, paste0(cur_country, '_coef_draws.csv')), row.names=F)
}

create_draws('cm')
create_draws('gh')
create_draws('mw')
create_draws('np')
create_draws('rw')
## Created by: Corinne Bintz
## Creation date: 2/8/2024
## plot change in values used in OB per country per age group 
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

## set-up --------
'%ni%' <- Negate('%in%')

## select input data date
in.date <- '2024-02-13'

pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey,tidyr,glmnet)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source)) # IHME-specific line. comment out if running outside IHME

in.dir <- file.path('/share/scratch/projects/hssa/asher/phase1/results/lasso/', in.date, 'no_age')
out.dir <- '/share/scratch/projects/hssa/asher/plots/'
out.dir_data <- file.path('/share/scratch/projects/hssa/asher/phase1/data', in.date)

## read in dhs data frames
dhs_df_15_24 <- fread(file.path('/share/scratch/projects/hssa/asher/phase1/data/', in.date, 'ob_input_prepped_df_dhs.csv'))
dhs_df_15_19 <- fread(file.path('/share/scratch/projects/hssa/asher/phase1/data/', in.date, 'ob_input_prepped_df_dhs.csv'))[age %in% c(seq(15,19))]

# create list of vars for dhs
vars_dhs <- names(dhs_df_15_24)
vars_dhs <- vars_dhs[!vars_dhs %in% c("country", "year", "pweight", "psu_unique", "strata_unique", "current_method_catg", "no_sex_activity_last_4_weeks",
                                      "age","any_birth_preg")]

## read in MICS data frames
mics_df_15_24 <- fread(file.path('/share/scratch/projects/hssa/asher/phase1/data/', in.date, 'ob_input_prepped_df_mics.csv'))
mics_df_15_19 <- fread(file.path('/share/scratch/projects/hssa/asher/phase1/data/', in.date, 'ob_input_prepped_df_mics.csv'))[age %in% c(seq(15,19))]

# create list of vars for mics
vars_mics <- names(mics_df_15_24)
vars_mics <- vars_mics[!vars_mics %in% c("country", "year", "pweight", "psu_unique", "strata_unique", "current_method_catg", "no_sex_activity_last_4_weeks",
                                         "age", "any_birth_preg")]

## function to organize means table into means and se when there are mulitple variables in the svy mean call 
organize_mean_se_multiple <- function(dt){
  means_tmp <-  dt[!grepl("se\\.(?!x)", dt$variable,perl = TRUE) & !grepl("ci_l", dt$variable) & !grepl("ci_u", dt$variable)]
  setnames(means_tmp, "value", "mean")
  se_ci_l <-  dt[grepl("ci_l", variable)]
  setnames(se_ci_l, "value", "ci_l")
  
  se_ci_u <-  dt[grepl("ci_u", variable)]
  setnames(se_ci_u, "value", "ci_u")
  
  se_ci_l[, variable := gsub("ci_l.", "", variable )]
  se_ci_u[, variable := gsub("ci_u.", "", variable )]
  
  mean_se <- merge(means_tmp,se_ci_l, by = c("variable", "baseline"))
  mean_se <- merge(mean_se,se_ci_u, by = c("variable", "baseline"))
  
  return(mean_se)
}



## performs a survey t test
perform_svyttest <- function(variable_name, survey_design, current_data,cur_country) {
  ## first, check if variable is completely missing in one of the years
  current_data_baseline <- current_data[baseline == 1]
  current_data_latest <- current_data[baseline == 0]
  
  if(all(is.na(current_data_baseline[[variable_name]])) | all(is.na(current_data_latest[[variable_name]]))){
    return()
  }
  
  # Construct the formula string and convert it to a formula object
  formula_str <- paste(variable_name, "~", "baseline")
  formula_obj <- as.formula(formula_str)
  
  # Perform the svyttest using the constructed formula and the provided survey design
  test_result <- svyttest(formula_obj, survey_design, na.rm=T)
  
  ## create temp dt with results
  tmp_dt <- data.table(variable = variable_name, p_value_ttest =test_result$p.value, country = cur_country)
  
  ## add tmp dt to dt
  return(tmp_dt)
}


get_plot_diff <- function(cur_country, mode, age_group) {
  ttest_dt <- data.table()
  print(cur_country)
  print(age_group)
  print(mode)
  ## see all possible variables based on mode

  if (mode == "dhs"){
    covariates <- vars_dhs
  } else{
    covariates <- vars_mics
  }
  
  

  # Create the right-hand side of the formula as a string
  formula_rhs <- paste(covariates, collapse = " + ")
  
  # Create the full formula string by adding the left-hand side
  formula_str <- paste("~", formula_rhs)
  
  # Convert the string into a formula object
  formula <- as.formula(formula_str)
  
  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")
  
  if (age_group == "15_24"){
    if (mode == "dhs"){
      current_data <- dhs_df_15_24[country ==cur_country]
    } else {
      current_data <- mics_df_15_24[country ==cur_country]
    }
    
  } else{ 
    if (mode == "dhs"){
      current_data <- dhs_df_15_19[country ==cur_country]
    } else{
      current_data <- mics_df_15_19[country ==cur_country]
    }
  }
 
  
  data_svy <- svydesign(id= ~psu_unique,  weights = ~pweight, data = current_data, nest = TRUE)
  
  means_p1 <- svyby(formula, ~baseline, data_svy , svymean,vartype = c("se", "ci")) ## no na's so can do all together


  means_p1_long <- data.table(melt(means_p1, id.var = "baseline"))
  
  mean_se_p1 <- organize_mean_se_multiple(means_p1_long)
  
  mean_se_p1[, country := cur_country]

  ## perform t tests
  for (var in unique(mean_se_p1$variable)){
    tmp_dt <-  perform_svyttest(var, data_svy, current_data,cur_country)
    ttest_dt <- rbind(ttest_dt, tmp_dt)
  }

  
  mean_se_p1 <- merge(mean_se_p1, ttest_dt, by = c("variable", "country"))

## wide for differences
mean_se_p1_wide <- dcast(country + variable + p_value_ttest~ baseline, value.var = c('mean'), data = mean_se_p1)
mean_se_p1_wide[, significance := ifelse(p_value_ttest < 0.05, "significant", "insignificant")]
setnames(mean_se_p1_wide, c("0", "1"), c("mean_latest", "mean_baseline"))
mean_se_p1_wide[, abs_change_mean := mean_latest-mean_baseline]
mean_se_p1_wide[, percent_change_mean := abs_change_mean/mean_baseline]

#order to align with plot
mean_se_p1_wide[, variable := factor(variable, levels = unique(vars_dhs))]
ggplot(data= mean_se_p1_wide, aes(x=variable, y = abs_change_mean))+
  geom_bar(stat = 'identity')+
  theme_bw()+
  labs(x = 'Variable', y ="Absolute change in mean", 
       title = paste0('Changes in means, ', age_group, ", ", cur_country, ", ", mode)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

write.csv(mean_se_p1_wide, file.path(out.dir_data, paste0(paste(cur_country, mode, age_group, sep = "_"),".csv") ), row.names=F)
ggsave(file.path(out.dir, paste0(age_group, '_change_ob_vars_', cur_country, "_", mode, ".png")), width = 5, height = 5)

}

## plot 15-24
get_plot_diff("cm", "dhs", "15_24")                  
get_plot_diff("mw", "dhs", "15_24")         
get_plot_diff("mw", "mics", "15_24")                  
get_plot_diff("rw", "dhs", "15_24")                  
get_plot_diff("gh", "dhs", "15_24")  
get_plot_diff("np", "dhs", "15_24")   

## plot 15-19
get_plot_diff("cm", "dhs", "15_19")                  
get_plot_diff("mw", "dhs", "15_19")         
get_plot_diff("mw", "mics", "15_19")                  
get_plot_diff("rw", "dhs", "15_19")                  
get_plot_diff("gh", "dhs", "15_19")                  
get_plot_diff("np", "dhs", "15_19")                  

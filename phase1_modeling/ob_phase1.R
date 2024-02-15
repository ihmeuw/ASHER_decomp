## Created by: Corinne Bintz
## Creation date: 1/23/2024
##  Run oaxaca-blinder decomp for asher decomp phase 1
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

#install.packages('oaxaca', lib= file.path(h, 'R/'))
library("oaxaca", lib.loc =file.path(h, 'R/'))

'%ni%' <- Negate('%in%')

## select input data date
in.date <- '2024-02-01'

## set endline to either MICS (to use Malawi MICS 2019-2020) or DHS (to use Malawi DHS 2015-2016)
endline <- 'MICS'

pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey,tidyr,glmnet)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source)) # IHME-specific line. comment out if running outside IHME

dir <- '/share/scratch/projects/hssa/asher/phase1/'
in.dir <- file.path('/share/scratch/projects/hssa/asher/phase1/data/', in.date)
out.dir <- file.path('/share/scratch/projects/hssa/asher/phase1/results/ob/', Sys.Date())
dir.create(out.dir, recursive = T)

## read in covariates selected based on lasso regression ------- TODO: make this depend on age group and pooled or not 
covariates_15_19 <- fread(file.path(dir, paste0('/results/lasso/', in.date, '/lasso_select_pooled_15_19_', tolower(endline), '.csv')))
covariates_15_19 <- data.table(covariates_15_19)
covariates_15_19 <- covariates_15_19[coef !=0 & covariate != "(Intercept)"]

covariates_15_24 <- fread(file.path(dir, paste0('/results/lasso/', in.date, '/lasso_select_pooled_15_24_', tolower(endline), '.csv')))
covariates_15_24 <- data.table(covariates_15_24)
covariates_15_24 <- covariates_15_24[coef !=0 & covariate != "(Intercept)"]

## read in input data -------
input_df <- data.table(fread(file.path(in.dir, paste0('/ob_input_prepped_df_', tolower(endline), '.csv'))))
input_df_15_19 <- input_df[age %in% c(seq(15,19))]
input_df_15_24 <- copy(input_df)

# combine for ob
# ob_df_15_19 <- cbind(input_df_15_19, outcome_df_15_19)
# ob_df_15_24 <- cbind(input_df_15_24, outcome_df_15_24)

## pooled  15-24-------
## with method as categorical 
pooled_ob_15_24 <- oaxaca(any_birth_preg ~ age + educ_single_yrs + mcpr + unmet_need + curr_cohabit + fp_exp_media + desire_child_teen + attend_school +
                            never_had_intercourse +rural + long_acting_method_mod + 
                            short_acting_method_mod + other_method_trad + sex_activity_last_4_weeks + wealth_index | baseline |  long_acting_method_mod + 
                            short_acting_method_mod + other_method_trad , data = input_df_15_24)
pooled_ob_15_24$n
pooled_ob_15_24$y ## difference in endline and baseline

## look at threefold decomposition
pooled_ob_15_24$threefold$overall

## plot
plot_ob <- plot(pooled_ob_15_24, components = c("endowments","coefficients"))

ggsave( file.path(out.dir, paste0('pooled_ob_15_24_', tolower(endline), '.png')), width = 10, height = 10)

## pooled  15-19------
## with method as categorical 
pooled_ob_15_19 <-  oaxaca(any_birth_preg ~ age + educ_single_yrs + mcpr + unmet_need + curr_cohabit + fp_exp_media + desire_child_teen + attend_school +
                             never_had_intercourse +rural +rec_sex_activity_missing + long_acting_method_mod + 
                             short_acting_method_mod + other_method_trad + sex_activity_last_4_weeks + wealth_index | baseline |  long_acting_method_mod + 
                             short_acting_method_mod + other_method_trad , data = ob_df_15_19)
pooled_ob_15_19$n
pooled_ob_15_19$y ## difference in endline and baseline

## look at threefold decomposition
pooled_ob_15_19$threefold$overall

## plot
plot_ob <- plot(pooled_ob_15_19, components = c("endowments","coefficients"))

ggsave( file.path(out.dir, paste0('pooled_ob_15_19_', tolower(endline), '.png')), width = 10, height = 10)

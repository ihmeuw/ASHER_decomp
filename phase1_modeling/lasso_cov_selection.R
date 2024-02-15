## Created by: Corinne Bintz
## Creation date: 1/23/2024
##  run lasso covariate selection for asher decomp
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
in.date <- '2024-02-07'

## set endline to either MICS (to use Malawi MICS 2019-2020) or DHS (to use Malawi DHS 2015-2016)
endline <- 'MICS'

pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey,tidyr,glmnet)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source)) # IHME-specific line. comment out if running outside IHME

in.dir <- file.path('/share/scratch/projects/hssa/asher/phase1/data/', in.date)
out.dir <- file.path('/share/scratch/projects/hssa/asher/phase1/results/lasso/', Sys.Date(), 'no_age')
dir.create(out.dir, recursive = T)

## read in input and outcome data ---------
if (endline == "MICS"){
  input_df_15_19 <- data.table(fread(file.path(in.dir, '/lasso_input_prepped_df_15_19_mics.csv')))
  input_df_15_24 <- data.table(fread(file.path(in.dir, '/lasso_input_prepped_df_15_24_mics.csv')))
  
  outcome_df <- data.table(fread(file.path(in.dir, '/outcome_prepped_df_mics.csv')))
} else{
  input_df_15_19 <- data.table(fread(file.path(in.dir, '/lasso_input_prepped_df_15_19_dhs.csv')))
  input_df_15_24 <- data.table(fread(file.path(in.dir, '/lasso_input_prepped_df_15_24_dhs.csv')))
  
  outcome_df <- data.table(fread(file.path(in.dir, '/outcome_prepped_df_dhs.csv')))
}

## function to select covariates pooled ------
lasso_select_pooled <- function(age_group, endline){
  if (age_group == "15_19"){
    outcome_df <- outcome_df[age %in% c(seq(15,19))]
    input_df <- input_df_15_19
  } else{ ## outcome df by default 15-24
    input_df <- input_df_15_24
  }
  outcome_df <- outcome_df[, -c('country', 'baseline', 'age')]
  y <- as.matrix(outcome_df)

  input_df <- input_df[, -c('country', 'baseline', 'year')] 
  x <- as.matrix(input_df)
  
  #perform k-fold cross-validation to find optimal lambda value
  cv_model <- cv.glmnet(x, y, alpha = 1)
  
  #find optimal lambda value that minimizes test MSE
  best_lambda <- cv_model$lambda.min
  
  best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda,intercept=FALSE)
  coef_mat <- coef(best_model)
  coef_mat <- as.matrix(coef_mat)
  coef_df <- as.data.frame(coef_mat)
  coef_dt <- data.table(covariate = rownames(coef_df),
                        coef = coef_df$s0)
  
  write.csv(coef_dt, file.path(out.dir, paste0('lasso_select_pooled_', age_group,  '_', endline, '.csv')))

}

## function to select covariates by country ------ : Just choosing among variables in both MICS and DHS 
lasso_select_country <- function(age_group,cur_country, endline){
  
  input_df <- fread(file.path(in.dir, paste0('lasso_input_prepped_df_', age_group, '_', endline, '.csv')))
  input_df <- input_df[country == cur_country]

  names(which(colSums(is.na(input_df)) > 0))
  
  outcome_df <- fread(file.path(in.dir, paste0('outcome_prepped_df_', endline, '.csv')))
  outcome_df <- outcome_df[country == cur_country]
  
  if (age_group == "15_19"){## outcome df by default 15-24
    outcome_df <- outcome_df[age %in% c(seq(15,19))]
  }
  
  outcome_df <- outcome_df[, -c('country', 'baseline', 'age')]
  y <- as.matrix(outcome_df)
  
  input_df <- input_df[, -c('country', 'baseline', 'year', 'age')]
  x <- as.matrix(input_df)
 
  #perform k-fold cross-validation to find optimal lambda value
  cv_model <- cv.glmnet(x, y, alpha = 1)
  
  #find optimal lambda value that minimizes test MSE
  best_lambda <- cv_model$lambda.min
  
  best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda,intercept=FALSE)
  coef_mat <- coef(best_model)
  coef_mat <- as.matrix(coef_mat)
  coef_df <- as.data.frame(coef_mat)
  coef_dt <- data.table(covariate = rownames(coef_df),
                        coef = coef_df$s0)
  
  write.csv(coef_dt, file.path(out.dir, paste0('lasso_select_', age_group, '_', cur_country, '_', endline,'.csv')))
}

## run lasso covariate selection pooled for each outcome type -------
lasso_select_pooled("15_19", tolower(endline))
lasso_select_pooled("15_24", tolower(endline))

## run lasso covariate selection for each country for each outcome type -------
lasso_select_country("15_19", 'cm',tolower(endline))
lasso_select_country("15_24", 'cm',tolower(endline))
lasso_select_country("15_19", 'gh',tolower(endline))
lasso_select_country("15_24", 'gh',tolower(endline))
lasso_select_country("15_19", 'mw',tolower(endline))
lasso_select_country("15_24", 'mw',tolower(endline))
lasso_select_country("15_19", 'np',tolower(endline))
lasso_select_country("15_24", 'np',tolower(endline))
lasso_select_country("15_19", 'rw',tolower(endline))
lasso_select_country("15_24", 'rw',tolower(endline))

 
## Created by: Corinne Bintz
## Creation date: 1/30/2024
##  Check education distribution to determine how to specify in ob model 
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

'%ni%' <- Negate('%in%')

pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey,tidyr,glmnet)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source)) # IHME-specific line. comment out if running outside IHME

## select input data date
in.date <- '2024-01-30'

in.dir <- file.path('/share/scratch/projects/hssa/asher/phase1/data/', in.date)
out.dir <- file.path('/share/scratch/projects/hssa/asher/plots')

## read in data
ind_dt <- data.table(fread(file.path(in.dir, '/merged_baseline_endline_mics.csv')))
ind_dt[, country := substr(country,0,2)]

ind_dt[, latest := max(year), by = "country"]
ind_dt[, baseline := ifelse(year == latest, 0,1)]

ind_dt <- ind_dt[, -c("latest")] # don't need this column anymore

## add age group
ind_dt[, age_group := ifelse(age %in% c(seq(15,19)),"15-19",
                                        ifelse(age %in% c(seq(20,24)), "20-24",
                                               ifelse(age %in% c(seq(25,29)), "25-29", 
                                                      ifelse(age %in% c(seq(30,34)), "30-34", 
                                                             ifelse(age %in% c(seq(35,39)), "35-39",
                                                                    ifelse(age %in% c(seq(40,44)), "40-44", 
                                                                           ifelse(age %in% c(seq(45,49)), "45-49",NA)))))))]
## calculate mean of any births or currently pregnant and education in single years by age group
ind_dt_means <- ind_dt[, .(mean_any_birth_preg = mean(any_birth_preg, na.rm=T), 
                           mean_educ_single_yrs = mean(educ_single_yrs, na.rm=T)), by = age_group]
## plot
ggplot(data = ind_dt_means, aes(x = mean_educ_single_yrs, y = mean_any_birth_preg, color = age_group))+
  geom_point() +
  theme_bw()+
  labs(x="Mean education in single years", y ="Mean of any birth or current pregnancy")

ggsave(file.path(out.dir, 'education_dist.png'), width =10, height=10)

## calculate mean of any births or currently pregnant and education in single years by age group and country and year 
ind_dt_means_country_year <- ind_dt[, .(mean_any_birth_preg = mean(any_birth_preg, na.rm=T), 
                           mean_educ_single_yrs = mean(educ_single_yrs, na.rm=T)), by = c('age_group', 'country', 'baseline')]

ind_dt_means_country_year[, baseline:=factor(baseline, labels = c("endline", "baseline"), levels = c(0,1))]
## plot
ggplot(data = ind_dt_means_country_year, aes(x = mean_educ_single_yrs, y = mean_any_birth_preg, color = age_group, shape =baseline))+
  facet_wrap(~country)+
  geom_point() +
  theme_bw()+
  labs(x="Mean education in single years", y ="Mean of any birth or current pregnancy")

ggsave(file.path(out.dir, 'education_dist_country_year.png'), width =10, height=10)


ggplot(data = ind_dt_means_country_year, aes(x = mean_educ_single_yrs, y = mean_any_birth_preg, color = country, shape =baseline))+
  facet_wrap(~age_group)+
  geom_point() +
  theme_bw()+
  labs(x="Mean education in single years", y ="Mean of any birth or current pregnancy")

ggsave(file.path(out.dir, 'education_dist_age_facet_country_year.png'), width =10, height=10)

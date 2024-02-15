## Created by: Corinne Bintz
## Creation date: 1/9/2024
## extract mean years of schooling of household head and edu years from HHM file 
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
pacman::p_load(magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey, tools)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source)) # IHME-specific line. comment out if running outside IHME
out.dir <- '/share/scratch/projects/hssa/asher/processed/'

extract_data <- function(survey, country){


dt <- data.table(read_dta(file.path(l, survey)))
#### SURVEY CHARACTERISTICS ####
# no ID variable in the dataset; need to compute ourselves
names(dt) <- tolower(names(dt))

# Convert only character columns to lowercase
dt[] <- lapply(dt, function(x) if(is.character(x)) tolower(x) else x)
dt[, cluster := hh1]
dt[, hh_id := hh2]
dt[, id := hl1] ## line number 
dt[, area_unit := hh6]
dt[, country := country]

dt[, hhweight := hhweight]

#This variable is not in CMR 2000, CMR 2006, GHA 2006 survey
dt[, psu := psu]
dt[, strata := stratum]
dt[, region := hh7]
dt[, year := hh5y]

#unique cluster id for each nid
dt[, cluster_unique := paste(cluster)]

# unique psu for each nid
dt[, psu_unique := paste(psu)]

# unique hh id for each nid
dt[, hhid_unique := paste( hh_id, cluster)]

# unique individual id for each nid
dt[, id_unique := id]

#mean education household head
dt[, mean_yrs_schooling_head := helevel]

## calculate education level achieved using ed team's custom code
dt[, edu_years_in_level := ed5b]
dt[, edu_level_categ := ed5a]

mask_edu_years_level <- dt$edu_years_in_level > 90
mask_edu_years_level <- mask_edu_years_level | dt$edu_level_categ == 0
dt[mask_edu_years_level ==TRUE, edu_years_in_level := NA]

# Impute edu_level_cont from string range in edu_level_categ
## for MWI 2019-2020
educ_catg_labs_mw <- list("0" = 0,
                       "1" = 0,
                       "2"= 8,
                       "3" = 10,
                       "4", 12,
                       "5" = 12,
                       "8" = NA,
                       "9" = NA)
# for GHA 2017-2018
educ_catg_labs_gh <- list("0" = 0,
                           "1" = 0,
                           "2"= 6,
                           "3" = 6,
                           "4"= 9,
                           "5" = 9,
                           "6" = 12,
                           "8" = NA,
                           "9" = NA)

dt[, edu_level_categ := as.character(edu_level_categ)]

if(country == "mw"){
  dt[, edu_min := ifelse(!is.na(edu_level_categ), unlist(educ_catg_labs_mw[edu_level_categ]), edu_level_categ)]
}
if (country == "gh"){
  dt[, edu_min := ifelse(!is.na(edu_level_categ), unlist(educ_catg_labs_gh[edu_level_categ]), edu_level_categ)]
  
}

dt[, edu_years_in_level := as.numeric(edu_years_in_level)]
dt[, educ_single_yrs := as.numeric(edu_min) + edu_years_in_level]

## filter just to variables of interest
vars_interest <- c("cluster", "hhid", "id", "area_unit", "country", "hhweight", "psu", "strata", 
                   "region", "year", "cluster_unique", "psu_unique", "hhid_unique", "mean_yrs_schooling_head", "country", 'id_unique','educ_single_yrs')
cur_vars <- names(dt)[names(dt) %in% vars_interest]
dt_filter <- dt[, ..cur_vars]

file_name <- file_path_sans_ext(basename(survey))
output_file_path <- file.path(out.dir, paste0(file_name, "_hhm_extract.csv"))
write.csv(dt_filter, output_file_path, row.names = FALSE)

}

extract_data("IDENT/PROJECT_FOLDERS/UNICEF_MICS/MWI/2019_2020/MWI_MICS6_2019_2020_HHM_Y2022M01D31.DTA", "mw")
extract_data("IDENT/PROJECT_FOLDERS/UNICEF_MICS/GHA/2017_2018/GHA_MICS6_2017_2018_HHMY2020M04D10.DTA", "gh")

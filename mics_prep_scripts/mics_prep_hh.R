## Created by: Corinne Bintz
## Creation date: 1/9/2024
## extract religion of household head from HH file 
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

## Enter survey here
survey <- c("IDENT/PROJECT_FOLDERS/UNICEF_MICS/MWI/2019_2020/MWI_MICS6_2019_2020_HH_Y2022M01D31.DTA")
#enter current country here
cur_country <-"mwi"
extract_data <- function(survey, cur_country){
dt <- data.table(read_dta(file.path(l, survey)))
#### SURVEY CHARACTERISTICS ####
# no ID variable in the dataset; need to compute ourselves
names(dt) <- tolower(names(dt))

# Convert only character columns to lowercase
dt[] <- lapply(dt, function(x) if(is.character(x)) tolower(x) else x)
dt[, cluster := hh1]
dt[, hh_id := hh2]
dt[, area_unit := hh6]
dt[, country := cur_country]

dt[, hhweight := hhweight]

#This variable is not in CMR 2000, CMR 2006, GHA 2006 survey
dt[, psu := psu]
dt[, strata := stratum]
dt[, region := hh7]
dt[, year := hh5y]

#unique cluster id for each nid
dt[, cluster_unique := paste( cluster)]

# unique psu for each nid
dt[, psu_unique := paste( psu)]

# unique hh id for each nid
dt[, hhid_unique := paste( hh_id, cluster)]

#mean education household head
dt[, religion := hc1a]
dt[, religion := as_factor(religion)]
dt[, religion := as.character(religion)]

## filter just to variables of interest
vars_interest <- c("cluster", "hhid", "id", "area_unit", "country", "hhweight", "psu", "strata", 
                   "region", "year", "cluster_unique", "psu_unique", "hhid_unique", "religion", "country", 'id_unique')
cur_vars <- names(dt)[names(dt) %in% vars_interest]
dt_filter <- dt[, ..cur_vars]

file_name <- file_path_sans_ext(basename(survey))
output_file_path <- file.path(out.dir, paste0(file_name, "_hh_extract.csv"))
write.csv(dt_filter, output_file_path, row.names = FALSE)
}

## enter survey and country here here 
extract_data("IDENT/PROJECT_FOLDERS/UNICEF_MICS/GHA/2017_2018/GHA_MICS6_2017_2018_HH_Y2020M04D10.DTA", "gh")
extract_data("IDENT/PROJECT_FOLDERS/UNICEF_MICS/MWI/2019_2020/MWI_MICS6_2019_2020_HH_Y2022M01D31.DTA", "mw")

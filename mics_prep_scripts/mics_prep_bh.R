## Created by: Corinne Bintz
## Creation date: 1/9/2024
## extract variables from birth history data
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

# create function to convert CMC date to year and month
get_cmc_year <- function(x) 1900 + as.integer((as.integer(x) - 1) / 12)
get_cmc_month <- function(x) as.integer(x) - 12 * (get_cmc_year(x) - 1900)


extract_data <- function(survey, cur_country){
dt <- data.table(read_dta(file.path(l, survey)))
#### SURVEY CHARACTERISTICS ####
# no ID variable in the dataset; need to compute ourselves
names(dt) <- tolower(names(dt))

# Convert only character columns to lowercase
dt[] <- lapply(dt, function(x) if(is.character(x)) tolower(x) else x)
dt[, cluster := hh1]
dt[, hh_id := hh2]
dt[, id := wm3] ## line number 
dt[, area_unit := hh6]
dt[, country := cur_country]

dt[, wpweight := wmweight]
dt[, pweight := wpweight/1000000]

#This variable is not in CMR 2000, CMR 2006, GHA 2006 survey
dt[, psu := psu]
dt[, strata := stratum]
dt[, region := hh7]

## convert year and month out of cmc format
dt[, wdoi_year := get_cmc_year(wdoi)]
dt[, wdoi_month := get_cmc_month(wdoi)]

dt[, year := wdoi_year]

dt[, bh4c_year := get_cmc_year(bh4c)]
dt[, bh4c_month := get_cmc_month(bh4c)]

#unique cluster id for each nid
dt[, cluster_unique := paste( cluster)]

# unique psu for each nid
dt[, psu_unique := paste( psu)]

# unique hh id for each nid
dt[, hhid_unique := paste( hh_id, cluster)]

# unique individual id 
dt[, id_unique :=  id]

## subtract child cmc from intervew cmc and see if less than 3 or less than 5 depending on variable
dt[bh4c_month > wdoi_month, child_age_months := ((wdoi_year-bh4c_year-1)*12) + (wdoi_month + 12 - bh4c_month)]
dt[wdoi_month >= bh4c_month, child_age_months := ((wdoi_year-bh4c_year)*12) + (wdoi_month - bh4c_month)]

dt[,birth_3_yr:= ifelse(child_age_months < 36, 1, 0)]
dt[,birth_5_yr:= ifelse(child_age_months < 60, 1, 0)]

## age at first birth: find child with birth order of 1 (brthord) and use mother's age at birt (magebrt): grouped: <20, 20-34, 25+
dt[, age_1st_birth := ifelse(brthord ==1,magebrt, 0 )]

## sum birth_3_yr by woman, birth_5_yr by woman 

## filter just to variables of interest
vars_interest <- c("cluster", "area_unit", "country", "wpweight", "pweight", "psu", "strata", 
                   "region", "year", "cluster_unique", "psu_unique", "hhid_unique", "country", 'id_unique',
                   "birth_3_yr", "birth_5_yr", "age_1st_birth", 'hh_id')
cur_vars <- names(dt)[names(dt) %in% vars_interest]
dt_filter <- dt[, ..cur_vars]


dt_filter_births <- dt_filter[, .(births_3_yr = sum(birth_3_yr),
                                       births_5_yr = sum(birth_5_yr),
                                  age_1st_birth = max(age_1st_birth)), by = c("cluster", "area_unit", "country", "wpweight", "pweight", "psu", "strata", 
                                                                               "region", "year", "cluster_unique", "psu_unique", "hhid_unique", 'id_unique','hh_id')]

dt_filter_births[, age_1st_birth := ifelse(age_1st_birth == 1, "<20",
                             ifelse(age_1st_birth ==2, "20-34",
                                    ifelse(age_1st_birth ==3, "35+",NA)))]

dt_filter_births[, births_3_yr := ifelse(is.na(births_3_yr), 0, births_3_yr )]
dt_filter_births[, births_5_yr := ifelse(is.na(births_5_yr), 0, births_5_yr )]

file_name <- file_path_sans_ext(basename(survey))
output_file_path <- file.path(out.dir, paste0(file_name, "_bh_extract.csv"))
write.csv(dt_filter_births, output_file_path, row.names = FALSE)
}

extract_data("IDENT/PROJECT_FOLDERS/UNICEF_MICS/GHA/2017_2018/GHA_MICS6_2017_2018_BH_Y2020M04D10.DTA", "gh")
extract_data("IDENT/PROJECT_FOLDERS/UNICEF_MICS/MWI/2019_2020/MWI_MICS6_2019_2020_BH_Y2022M01D31.DTA", "mw")

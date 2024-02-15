## Corinne Bintz
## 1-19-2024
## Create school attendance from HHM 


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
pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey,readstata13 )

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source)) # IHME-specific line. comment out if running outside IHME

asher.dir<-'/mnt/team/hs/pub/users/iramarto/asher/dhs_data_dta/'
out.dir <- '/mnt/share/scratch/projects/hssa/asher/processed/'
files<-list.files(paste0(asher.dir))


'%ni%' <- Negate('%in%')

# create function to convert CMC date to year and month
get_cmc_year <- function(x) 1900 + as.integer((as.integer(x) - 1) / 12)
get_cmc_month <- function(x) as.integer(x) - 12 * (get_cmc_year(x) - 1900)


survey <- c("RWA_DHS8_2019_2020_HHM_RWPR81FL_Y2021M10D05.DTA")

## Function to prepare household data prior to creating wealth quintiles
prep_hhm_data <- function(survey){
  print(survey)
  
  dt<-as.data.table(read_dta(paste0(asher.dir, survey)))
  names(dt) <- tolower(names(dt)) # some country-years have capital letter while others do not
  dt[] <- lapply(dt, tolower) ##make all values lower case for easier string comparison
  nrow(dt)
  
  #######################################
  ### SURVEY CHARACTERISTICS ###
  #######################################
  dt[, country :=hv000]
  dt[, cluster := hv001]
  dt[, hh_id := hv002]
  dt[, id := hvidx] 
  dt[, area_unit := hv004]
  dt[, hhpweight := hv005]
  dt[, cmc_interview_date := hv008]
  dt[, psu := hv021]
  dt[, strata := hv022]
  dt[, region := hv024]
  dt[, year := hv007]
  if(unique(dt$country)%like%"np"){ ## nepal has a different calendar
    dt[, year:=as.numeric(hv007)-57]
  } else {
    dt[, year := hv007]
  }
  
  #unique cluster id for each nid
  dt[, cluster_unique := paste(country, year, cluster)]
  # unique psu for each nid
  dt[, psu_unique := paste(country, year, psu)]
  # unique hh id for each nid
  dt[, hhid_unique := paste(country, year, hh_id, cluster)] ## added cluster
  # unique individual id for each nid
  dt[, id_unique := paste(country, year, id)]
  
  dt[, attend_school := ifelse(hv121 %in% c(1,2), 1,
                               ifelse(hv121 == 0, 0, NA))]

  sa_l <- dt[, c('hhid_unique', "cluster", "id_unique", "id", "hh_id", "country", "year", "attend_school")]
  # attended school during the prior year
  file_name <- str_replace(survey, ".DTA", "")
  write.csv(sa_l, file.path(out.dir, paste0(file_name, "_indiv_in_household.csv")), row.names = FALSE)
}
  
#-------------------Header------------------------------------------------
# Project: IHME ASHER Decomposition
# Purpose: Extract GPS and prepare administrative names from DHS surveys
# Date: 1/19/2024
# Notes:
#***************************************************************************

# SET-UP -----------------------------------------------------------

# clear environment
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

# load packages
pacman::p_load(magrittr,tidyverse,parallel,plyr,dplyr,haven,survey,tools,devtools,sf)

# in/out
out.dir <- "FILEPATH"

# create function for the opposite of %in%
'%ni%' <- Negate('%in%')

# current admin 2 shapefile
shp2 <- st_read(file.path("FILEPATH"))


# EXTRACT DATA FUNCTION -----------------------------------------------

extract_data <- function(survey, cur_country) {
  
  # read in data 
  dt <- data.table(read_dta(survey))
  
  
  # SURVEY CHARACTERISTICS --------------------------------------
  message("||---Survey characteristics")
  
  # convert variable names to lowercase
  names(dt) <- tolower(names(dt))
  
  # extract survey name and add to dt
  survey_name <- str_extract(survey, "[A-Z]{3}_DHS[0-9]_[0-9]{4}(_[0-9]{4})*")
  dt[, survey := survey_name]
  
  # identifiers
  dt[, cluster := dhsclust]
  dt[, country := cur_country]
  
  
  # LATITUDE/LONGITUDE -------------------------------------------
  message("||---GPS Coordinates")
  
  # latitude
  dt[, lat := latnum]
  
  # longitude
  dt[, long := longnum]
  
  
  # MAP ONTO CURRENT SHAPEFILE -----------------------------------
  message("||---Mapping onto current IHME shapefile")
  
  # full name of current country
  if (cur_country == "cm") country <- "Cameroon"
  if (cur_country == "gh") country <- "Ghana"
  if (cur_country == "mw") country <- "Malawi"
  if (cur_country == "np") country <- "Nepal"
  if (cur_country == "rw") country <- "Rwanda"
  
  # subset admin 2 shapefile 
  adm2 <- subset(shp2, shp2$ADM0_NAME == country) %>% dplyr::select(ADM1_NAME, ADM2_NAME, geometry)
  
  # map coordinates onto shapefile
  tmp <- st_as_sf(dt[!is.na(lat)], coords = c("long", "lat"), crs = 4326)
  tmp <- st_join(tmp, adm2, join = st_within)
  tmp <- cbind(tmp, st_coordinates(tmp)) %>% as.data.table
  setnames(tmp, c("ADM1_NAME", "ADM2_NAME", "X", "Y"), c("admin_1_shp", "admin_2_shp", "long", "lat"))
  dt <- rbind(dt[is.na(lat)], tmp[, -c("geometry"), with = F], fill = TRUE) %>% as.data.table
  
  # flag any which did not map (may be near the borders, or incorrect GPS)
  message(paste0("||----WARNING: ", nrow(dt[is.na(admin_1_shp)]), " clusters did not map onto shapefile!!!"))
  
  
  # EXPORT CLEANED DATA ------------------------------------------
  message("||---Export cleaned data")
  
  # filter to variables of interest
  vars_interest <- c("survey","cluster","country","lat","long","admin_1_shp","admin_2_shp")
  cur_vars <- names(dt)[names(dt) %in% vars_interest]
  dt_filter <- dt[, ..cur_vars]
  
  # save the cleaned data
  file_name <- gsub("\\.DTA", "", basename(survey))
  output_file_path <- file.path(out.dir, paste0(file_name, "_gps_extract.csv"))
  write.csv(dt_filter, output_file_path, row.names = FALSE)
  
  message(paste0("Successfully saved ", file_name))
}


# RUN EXTRACTIONS ------------------------------------------------

# Cameroon
extract_data("/FILEPATH/CMR_DHS4_2004_GPS.DTA", "cm")
extract_data("/FILEPATH/CMR_DHS5_2011_GPS.DTA", "cm")
extract_data("/FILEPATH/CMR_DHS7_2018_2019_HHM.DTA", "cm") 

# Ghana
extract_data("/FILEPATH/GHA_DHS4_2003_GPS.DTA", "gh")
extract_data("/FILEPATH/GHA_DHS5_2008_GPS.DTA", "gh")
extract_data("/FILEPATH/GHA_DHS6_2014_GPS.DTA", "gh")
extract_data("/FILEPATH/GHA_DHS8_2022_2023_GPS.DTA", "gh")

# Malawi
extract_data("/FILEPATH/MWI_DHS4_2000_GPS.DTA", "mw")
extract_data("/FILEPATH/MWI_DHS4_2004_2005_GPS.DTA", "mw")
extract_data("/FILEPATH/MWI_DHS6_2010_GPS.DTA", "mw")
extract_data("/FILEPATH/MWI_DHS7_2015_2016_GPS.DTA", "mw")

# Nepal
extract_data("/FILEPATH/NPL_DHS4_2001_GPS.DTA", "np")
extract_data("/FILEPATH/NPL_DHS5_2006_GPS.DTA", "np")
extract_data("/FILEPATH/NPL_DHS6_2011_GPS.DTA", "np")
extract_data("/FILEPATH/NPL_DHS7_2016_2017_GPS.DTA", "np")
extract_data("/FILEPATH/NPL_DHS8_2022_GPS.DTA", "np")

# Rwanda
# no GPS for RWA 2000
extract_data("/FILEPATH/RWA_DHS4_2005_GPS.DTA", "rw")
extract_data("/FILEPATH/RWA_DHS6_2010_2011_GPS.DTA", "rw")
extract_data("/FILEPATH/RWA_DHS7_2014_2015_GPS.DTA", "rw")
extract_data("/FILEPATH/RWA_DHS8_2019_2020_GPS.DTA", "rw")
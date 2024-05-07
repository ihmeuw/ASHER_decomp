#-------------------Header------------------------------------------------
# Author: Olivia Angelino
# Project: ASHER
# Purpose: Extract GPS and prepare administrative names
# Date: 1/19/2024
# Notes:
#***************************************************************************

# SET-UP -----------------------------------------------------------

# clear environment
rm(list=ls())
username <- Sys.info()[["user"]]

if (Sys.info()["sysname"] == "Linux") {
  j <- "/snfs1/"
  h <- paste0("/homes/", username, "/")
  r <- "/mnt/"
  l <-"/ihme/limited_use/"
} else {
  j <- "J:/"
  h <- "H:/"
  r <- "R:/"
  l <- "L:/"
}

# load packages
pacman::p_load(magrittr,tidyverse,parallel,plyr,dplyr,haven,survey,tools,devtools,sf)

# in/out
out.dir <- '/share/scratch/projects/hssa/asher/data/01_processed/'

# create function for the opposite of %in%
'%ni%' <- Negate('%in%')

# current admin 2 shapefile
shp2 <- st_read(file.path("/snfs1/WORK/11_geospatial/admin_shapefiles/current/lbd_standard_admin_2.shp"))


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
  
  # ihme_loc_id of current country
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
extract_data("/snfs1/DATA/DHS_PROG_DHS/CMR/2004/CMR_DHS4_2004_GPS_CMGE42FL_Y2018M05D29.DTA", "cm")
extract_data("/snfs1/DATA/DHS_PROG_DHS/CMR/2011/CMR_DHS5_2011_GPS_CMGE61FL_Y2018M06D11.DTA", "cm")
#extract_data("/snfs1/DATA/DHS_PROG_DHS/CMR/2018_2019/CMR_DHS7_2018_2019_HHM_CMPR71FL_Y2020M06D10.DTA", "cm") NOT A DTA BUT EXISTS

# Ghana
extract_data("/snfs1/DATA/DHS_PROG_DHS/GHA/2003/GHA_DHS4_2003_GPS_GHGE4BFL_Y2018M10D15.DTA", "gh")
extract_data("/snfs1/DATA/DHS_PROG_DHS/GHA/2008/GHA_DHS5_2008_GPS_GHGE5AFL_Y2018M10D15.DTA", "gh")
extract_data("/snfs1/DATA/DHS_PROG_DHS/GHA/2014/GHA_DHS6_2014_GPS_GHGE71FL_Y2018M12D10.DTA", "gh")
extract_data("/snfs1/DATA/DHS_PROG_DHS/GHA/2022_2023/GHA_DHS8_2022_2023_GPS_GHGE8AFL_Y2024M01D22.DTA", "gh")

# Malawi
extract_data("/snfs1/DATA/DHS_PROG_DHS/MWI/2000/MWI_DHS4_2000_GPS_MWGE43FL_Y2019M01D07.DTA", "mw")
extract_data("/snfs1/DATA/DHS_PROG_DHS/MWI/2004_2005/MWI_DHS4_2004_2005_GPS_MWGE4BFL_Y2018M12D10.DTA", "mw")
extract_data("/snfs1/DATA/DHS_PROG_DHS/MWI/2010/MWI_DHS6_2010_GPS_MWGE62FL_Y2018M12D10.DTA", "mw")
extract_data("/snfs1/DATA/DHS_PROG_DHS/MWI/2015_2016/MWI_DHS7_2015_2016_GPS_MWGE7AFL_Y2018M12D10.DTA", "mw")

# Nepal
extract_data("/snfs1/DATA/DHS_PROG_DHS/NPL/2001/NPL_DHS4_2001_GPS_NPGE42FL_Y2019M02D19.DTA", "np")
extract_data("/snfs1/DATA/DHS_PROG_DHS/NPL/2006/NPL_DHS5_2006_GPS_NPGE53FL_Y2018M11D05.DTA", "np")
extract_data("/snfs1/DATA/DHS_PROG_DHS/NPL/2011/NPL_DHS6_2011_GPS_NPGE61FL_Y2018M11D05.DTA", "np")
extract_data("/snfs1/DATA/DHS_PROG_DHS/NPL/2016_2017/NPL_DHS7_2016_2017_GPS_NPGE7AFL_Y2018M11D05.DTA", "np")
extract_data("/snfs1/DATA/DHS_PROG_DHS/NPL/2022/NPL_DHS8_2022_GPS_NPGE81FL_Y2023M06D23.DTA", "np")

# Rwanda
# no GPS for RWA 2000
extract_data("/snfs1/DATA/DHS_PROG_DHS/RWA/2005/RWA_DHS4_2005_GPS_RWGE54FL_Y2019M03D18.DTA", "rw")
extract_data("/snfs1/DATA/DHS_PROG_DHS/RWA/2010_2011/RWA_DHS6_2010_2011_GPS_RWGE61FL_Y2019M04D16.DTA", "rw")
extract_data("/snfs1/DATA/DHS_PROG_DHS/RWA/2014_2015/RWA_DHS7_2014_2015_GPS_RWGE72FL_Y2019M04D16.DTA", "rw")
extract_data("/snfs1/DATA/DHS_PROG_DHS/RWA/2019_2020/RWA_DHS8_2019_2020_GPS_RWGE81FL_Y2021M10D05.DTA", "rw")

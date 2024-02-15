### ASHER DECOMP
## WEALTH QUINTILE CREATION

## Created by: Annie Haakenstad
## Creation date: 10/20/2023

## Most recently edited by: Corinne Bintz
## Most recent editing date: 11-6-2023


## 1) Create the "attended school" variable from the roster module
## 2) Create "wealth quintile"  for all households


## note: run this code for all baseline years first so you have the vars that will go into wealth quintile analysis for each country!
## this is assuming that the baseline year will be the limiting factor for vars.
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
pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey,readstata13 )

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source)) # IHME-specific line. comment out if running outside IHME

asher.dir<-'/mnt/team/hs/pub/users/iramarto/asher/dhs_data_dta/'
out.dir <- '/mnt/share/scratch/projects/hssa/asher/processed/'
files<-list.files(paste0(asher.dir))


'%ni%' <- Negate('%in%')

# create function to convert CMC date to year and month
get_cmc_year <- function(x) 1900 + as.integer((as.integer(x) - 1) / 12)
get_cmc_month <- function(x) as.integer(x) - 12 * (get_cmc_year(x) - 1900)

#### household datasets ####
hh<-files[files%like%"_HH_"]
# list of variables needed in addition to survey characteristics --> should double check and update
# survey characteristics not included
# hv205, hh$hv012, hh$hv013, hh$hv216,
# "hv206", "hv207", "hv208", "hv209", "hv210", "hv211", "hv212",
# "hv221", "hv243a", "hv243b", "hv243c", "hv243d", "hv247",
# "hv206", "hv207", "hv208", "hv209", "hv210", "hv211", "hv212",
# "hv221", "hv243a", "hv243b", "hv243c", "hv243d", hv246a-hv246f, hv247
# sh110s, sh110l, sh110k, hv244, hv245
# hv246a, hv246b, hv246c, hv246d, hv246e, hv246f,
# hv247, hv225
# hv201, hv205, hv213, hv214, hv215 , hv 226, hv244, hv245

survey <- c("MWI_DHS4_2000_HH_MWHR41FL_Y2019M01D07.DTA")

## Function to prepare household data prior to creating wealth quintiles
prep_hh_data <- function(survey){
  print(survey)

    dt<-as.data.table(read.dta13(paste0(asher.dir, survey)))
    names(dt) <- tolower(names(dt)) # some country-years have capital letter while others do not
    dt[] <- lapply(dt, tolower) ##make all values lower case for easier string comparison
    nrow(dt)

  #######################################
  ### SURVEY CHARACTERISTICS ###
  #######################################
  dt[, country :=hv000]
  dt[, cluster := hv001]
  dt[, hh_id := hv002]
  dt[, id := hv003] # respondent's line number (answering household survey)
  dt[, area_unit := hv004]
  dt[, hhpweight := hv005] #household sample weight (6 decimals)
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

  id_vars<-c("country", "cluster", "hh_id", "id", "area_unit", "hhpweight", "cmc_interview_date",
             "psu", "strata", "region", "year", "cluster_unique", "psu_unique", "hhid_unique", "id_unique")

  # look at original wealth quintiles
  # hv270 hv271
  summary(dt$hv270) ## does not exist in MWI 2000 nor RWA 2000 nor NPL 2001
  #########################################################
  #### PART 1 :  INDIVIDUAL VARIABLES IN HOUSEHOLD RECODE
  #########################################################
  ## --> other follow up for household recode
  # 1) education level of household head
  # 2) whether live with natural mother or father
  # 3) also currently in school and why stopped
  ## --> what really want is the wealth and education level of the family the adolescent grew up in
  # but these may be the best proxies

  #why stopped may only  be in Ghana, actually
  #there are better ways to do this

  if (survey %like% "MWI_DHS7_2015" ){
    sa <- dt[, c("hhid_unique", "hv121_01", "hv121_02", "hv121_03", "hv121_04", "hv121_05",
                 "hv121_06", "hv121_07", "hv121_08", "hv121_09",
                 "hv121_10", "hv121_11", "hv121_12", "hv121_13", "hv121_14",
                 "hv121_15", "hv121_16", "hv121_17", "hv121_18", "hv121_19",
                 "hv121_20")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:21))
  } else if ( survey %like% "RWA_DHS8_2019"){
    sa <- dt[, c("hhid_unique", "hv121_01", "hv121_02", "hv121_03", "hv121_04", "hv121_05",
                 "hv121_06", "hv121_07", "hv121_08", "hv121_09",
                 "hv121_10", "hv121_11", "hv121_12", "hv121_13", "hv121_14",
                 "hv121_15", "hv121_16")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:17))
  } else if ( survey %like% "RWA_DHS4_2000"){
    sa <- dt[, c("hhid_unique", "hv121_01", "hv121_02", "hv121_03", "hv121_04", "hv121_05",
                 "hv121_06", "hv121_07", "hv121_08", "hv121_09",
                 "hv121_10", "hv121_11", "hv121_12", "hv121_13", "hv121_14",
                 "hv121_15", "hv121_16", "hv121_17", "hv121_18", "hv121_19",
                 "hv121_20")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:21))
  } else if (survey %like% "GHA_DHS4_1998"){
    sa <- dt[, c("hhid_unique", "hv110_01", "hv110_02", "hv110_03", "hv110_04", "hv110_05",
                 "hv110_06", "hv110_07", "hv110_08", "hv110_09",
                 "hv110_10", "hv110_11", "hv110_12", "hv110_13", "hv110_14",
                 "hv110_15", "hv110_16", "hv110_17", "hv110_18", "hv110_19",
                 "hv110_20", "hv110_21", "hv110_22", "hv110_23")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:24))
  } else if (survey %like% "GHA_DHS5_2008"){
    sa <- dt[, c("hhid_unique", "hv121_01", "hv121_02", "hv121_03", "hv121_04", "hv121_05",
                 "hv121_06", "hv121_07", "hv121_08", "hv121_09",
                 "hv121_10", "hv121_11", "hv121_12", "hv121_13", "hv121_14",
                 "hv121_15", "hv121_16", "hv121_17", "hv121_18", "hv121_19",
                 "hv121_20", "hv121_21", "hv110_22")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:23))
  } else if (survey %like% "RWA_DHS4_2000"){
    sa <- dt[, c("hhid_unique", "hv121_01", "hv121_02", "hv121_03", "hv121_04", "hv121_05",
                 "hv121_06", "hv121_07", "hv121_08", "hv121_09",
                 "hv121_10", "hv121_11", "hv121_12", "hv121_13", "hv121_14",
                 "hv121_15", "hv121_16", "hv121_17", "hv121_18", "hv121_19",
                 "hv121_20")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:21))
  } else if (survey %like% "MWI_DHS4_2004"){
    sa <- dt[, c("hhid_unique", "hv110_01", "hv110_02", "hv110_03", "hv110_04", "hv110_05",
                 "hv110_06", "hv110_07", "hv110_08", "hv110_09",
                 "hv110_10", "hv110_11", "hv110_12", "hv110_13", "hv110_14",
                 "hv110_15", "hv110_16", "hv110_17", "hv110_18")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:19))
  } else if (survey %like% "MWI_DHS6_2010"){
    sa <- dt[, c("hhid_unique", "hv121_01", "hv121_02", "hv121_03", "hv121_04",
                 "hv121_05", "hv121_06", "hv121_07", "hv121_08", "hv121_09",
                 "hv121_10", "hv121_11", "hv121_12", "hv121_13", "hv121_14",
                 "hv121_15", "hv121_16", "hv121_17", "hv121_18", "hv121_19",
                 "hv121_20", "hv121_21", "hv121_22", "hv121_23")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:24))
  } else if (survey %like% "RWA_DHS5_2005"){
    sa <- dt[, c("hhid_unique", "hv110_01", "hv110_02", "hv110_03", "hv110_04",
                 "hv110_05", "hv110_06", "hv110_07", "hv110_08", "hv110_09",
                 "hv110_10", "hv110_11", "hv110_12", "hv110_13", "hv110_14",
                 "hv110_15", "hv110_16")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:17))
  } else if (survey %like% "RWA_DHS6_2010"){
    sa <- dt[, c("hhid_unique", "hv121_01", "hv121_02", "hv121_03", "hv121_04",
                 "hv121_05", "hv121_06", "hv121_07", "hv121_08", "hv121_09",
                 "hv121_10", "hv121_11", "hv121_12", "hv121_13", "hv121_14",
                 "hv121_15", "hv121_16", "hv121_17", "hv121_18", "hv121_19",
                 "hv121_20")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:21))
  } else if (survey %like% "RWA_DHS7_2014"){
    sa <- dt[, c("hhid_unique", "hv121_01", "hv121_02", "hv121_03", "hv121_04",
                 "hv121_05", "hv121_06", "hv121_07", "hv121_08", "hv121_09",
                 "hv121_10", "hv121_11", "hv121_12", "hv121_13", "hv121_14",
                 "hv121_15", "hv121_16", "hv121_17", "hv121_18", "hv121_19",
                 "hv121_20", "hv121_21", "hv121_22")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:23))
  }else if (survey %like% "CMR_DHS4_2004"){
    sa <- dt[, c("hhid_unique", "hv121_01", "hv121_02", "hv121_03", "hv121_04",
                 "hv121_05", "hv121_06", "hv121_07", "hv121_08", "hv121_09",
                 "hv121_10", "hv121_11", "hv121_12", "hv121_13", "hv121_14",
                 "hv121_15", "hv121_16", "hv121_17", "hv121_18", "hv121_19",
                 "hv121_20", "hv121_21", "hv121_22", "hv121_23", "hv121_24",
                 "hv121_25", "hv121_26", "hv121_27", "hv121_28", "hv121_29",
                 "hv121_30", "hv121_31", "hv121_32", "hv121_33", "hv121_34",
                 "hv121_35", "hv121_36", "hv121_37", "hv121_38", "hv121_39",
                 "hv121_40", "hv121_41", "hv121_42", "hv121_43", "hv121_44")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:45))

  }else if (survey %like% "CMR_DHS7_2018"){
    sa <- dt[, c("hhid_unique", "hv121_01", "hv121_02", "hv121_03", "hv121_04",
                 "hv121_05", "hv121_06", "hv121_07", "hv121_08", "hv121_09",
                 "hv121_10", "hv121_11", "hv121_12", "hv121_13", "hv121_14",
                 "hv121_15", "hv121_16", "hv121_17", "hv121_18", "hv121_19",
                 "hv121_20", "hv121_21", "hv121_22", "hv121_23", "hv121_24",
                 "hv121_25", "hv121_26", "hv121_27", "hv121_28", "hv121_29",
                 "hv121_30", "hv121_31", "hv121_32", "hv121_33", "hv121_34",
                 "hv121_35", "hv121_36", "hv121_37", "hv121_38", "hv121_39",
                 "hv121_40")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:41))

  } else if (survey %like% "GHA_DHS4_2003"){
    sa <- dt[, c("hhid_unique", "hv121_01", "hv121_02", "hv121_03", "hv121_04",
                 "hv121_05", "hv121_06", "hv121_07", "hv121_08", "hv121_09",
                 "hv121_10", "hv121_11", "hv121_12", "hv121_13", "hv121_14",
                 "hv121_15", "hv121_16", "hv121_17", "hv121_18", "hv121_19",
                 "hv121_20", "hv121_21", "hv121_22", "hv121_23", "hv121_24",
                 "hv121_25", "hv121_26", "hv121_27", "hv121_28", "hv121_29",
                 "hv121_30", "hv121_31", "hv121_32", "hv121_33", "hv121_34",
                 "hv121_35", "hv121_36", "hv121_37", "hv121_38", "hv121_39",
                 "hv121_40")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:41))

  } else if (survey %like% "MWI_DHS4_2000"){
    sa <- dt[, c("hhid_unique", "hv121_01", "hv121_02", "hv121_03", "hv121_04",
                 "hv121_05", "hv121_06", "hv121_07", "hv121_08", "hv121_09",
                 "hv121_10", "hv121_11", "hv121_12", "hv121_13", "hv121_14",
                 "hv121_15", "hv121_16", "hv121_17", "hv121_18", "hv121_19",
                 "hv121_20", "hv121_21", "hv121_22", "hv121_23", "hv121_24",
                 "hv121_25", "hv121_26", "hv121_27", "hv121_28", "hv121_29",
                 "hv121_30")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:31))

  } else if (survey %like% "MWI_DHS7_2015"){
    sa <- dt[, c("hhid_unique", "hv121_01", "hv121_02", "hv121_03", "hv121_04",
                 "hv121_05", "hv121_06", "hv121_07", "hv121_08", "hv121_09",
                 "hv121_10", "hv121_11", "hv121_12", "hv121_13", "hv121_14",
                 "hv121_15", "hv121_16", "hv121_17", "hv121_18", "hv121_19")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:20))
  } else if (survey %like% "NPL_DHS4_2001"){
    sa <- dt[, c("hhid_unique", "hv121_01", "hv121_02", "hv121_03", "hv121_04",
                 "hv121_05", "hv121_06", "hv121_07", "hv121_08", "hv121_09",
                 "hv121_10", "hv121_11", "hv121_12", "hv121_13", "hv121_14",
                 "hv121_15", "hv121_16", "hv121_17", "hv121_18", "hv121_19",
                 "hv121_20", "hv121_21", "hv121_22", "hv121_23", "hv121_24",
                 "hv121_25", "hv121_26", "hv121_27", "hv121_28", "hv121_29")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:30))

  } else if (survey %like% "NPL_DHS8_2022"){
    sa <- dt[, c("hhid_unique", "hv121_01", "hv121_02", "hv121_03", "hv121_04",
                 "hv121_05", "hv121_06", "hv121_07", "hv121_08", "hv121_09",
                 "hv121_10", "hv121_11", "hv121_12", "hv121_13", "hv121_14",
                 "hv121_15", "hv121_16", "hv121_17", "hv121_18", "hv121_19",
                 "hv121_20", "hv121_21", "hv121_22", "hv121_23", "hv121_24",
                 "hv121_25", "hv121_26")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:27))

  }else if (survey %like% "NPL_DHS5_2006"){
    sa <- dt[, c("hhid_unique", "hv121_01", "hv121_02", "hv121_03", "hv121_04",
                 "hv121_05", "hv121_06", "hv121_07", "hv121_08", "hv121_09",
                 "hv121_10", "hv121_11", "hv121_12", "hv121_13", "hv121_14",
                 "hv121_15", "hv121_16", "hv121_17", "hv121_18", "hv121_19",
                 "hv121_20", "hv121_21", "hv121_22", "hv121_23", "hv121_24",
                 "hv121_25", "hv121_26", "hv121_27", "hv121_28", "hv121_29",
                 "hv121_30")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:31))

  }else if (survey %like% "NPL_DHS6_2011"){
    sa <- dt[, c("hhid_unique", "hv121_01", "hv121_02", "hv121_03", "hv121_04",
                 "hv121_05", "hv121_06", "hv121_07", "hv121_08", "hv121_09",
                 "hv121_10", "hv121_11", "hv121_12", "hv121_13", "hv121_14",
                 "hv121_15", "hv121_16", "hv121_17", "hv121_18", "hv121_19",
                 "hv121_20", "hv121_21", "hv121_22", "hv121_23", "hv121_24",
                 "hv121_25", "hv121_26", "hv121_27", "hv121_28", "hv121_29",
                 "hv121_30", "hv121_31")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:32))

  }else if (survey %like% "NPL_DHS7_2016"){
    sa <- dt[, c("hhid_unique", "hv121_01", "hv121_02", "hv121_03", "hv121_04",
                 "hv121_05", "hv121_06", "hv121_07", "hv121_08", "hv121_09",
                 "hv121_10", "hv121_11", "hv121_12", "hv121_13", "hv121_14",
                 "hv121_15", "hv121_16", "hv121_17", "hv121_18", "hv121_19",
                 "hv121_20", "hv121_21", "hv121_22", "hv121_23", "hv121_24",
                 "hv121_25", "hv121_26", "hv121_27", "hv121_28", "hv121_29",
                 "hv121_30", "hv121_31", "hv121_32", "hv121_33", "hv121_34",
                 "hv121_35", "hv121_36", "hv121_37", "hv121_38")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:39))

  } else if (survey %like% "GHA_DHS8_2022"){
    sa <- dt[, c("hhid_unique", "hv121_01", "hv121_02", "hv121_03", "hv121_04",
                 "hv121_05", "hv121_06", "hv121_07", "hv121_08", "hv121_09",
                 "hv121_10", "hv121_11", "hv121_12", "hv121_13", "hv121_14",
                 "hv121_15", "hv121_16", "hv121_17", "hv121_18", "hv121_19",
                 "hv121_20", "hv121_21", "hv121_22", "hv121_23", "hv121_24",
                 "hv121_25", "hv121_26", "hv121_27", "hv121_28", "hv121_29",
                 "hv121_30", "hv121_31", "hv121_32", "hv121_33", "hv121_34",
                 "hv121_35")]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:36))
    
  }else {


    sa <- dt[, c("hhid_unique", "hv121_01", "hv121_02", "hv121_03", "hv121_04", "hv121_05",
                 "hv121_06", "hv121_07", "hv121_08", "hv121_09",
                 "hv121_10", "hv121_11", "hv121_12", "hv121_13", "hv121_14",
                 "hv121_15", "hv121_16", "hv121_17", "hv121_18", "hv121_19",
                 "hv121_20", "hv121_21", "hv121_22", "hv121_23", "hv121_24", "hv121_25" )]
    sa_l <- reshape(sa, direction="long", idvar="hhid_unique",  timevar="line_number",
                    v.names=c( "attend_school"), varying = list(2:26))

  }
  

     sa_l[, attend_school := ifelse(attend_school %in% c("currently attending","attended at some time"), 1, 
                                       ifelse(!is.na(attend_school), 0, NA))]
     
    setnames(sa_l, "line_number", "id") 
    sa_l[, id := as.character(id)]
    
    
    
    
  #   ## warning!  This is assuming that the line number/ id number is the suffix here
  # # that might not always be the case and then this would need to be changed

   sa_l_merge <- merge(sa_l, dt[, c('hhid_unique', "cluster", "hh_id", "country", "year")], by = c( 'hhid_unique'))

 # attended school during the prior year
   file_name <- str_replace(survey, ".DTA", "")
  write.csv(sa_l_merge, file.path(out.dir, paste0(file_name, "_indiv_in_household.csv")), row.names = FALSE)


#########################################################
### PART 2 :  WEALTH QUINTILE CONTRUCTION ###############
#########################################################

# Sourced from 2016 time stamp page: https://dhsprogram.com/topics/wealth-index/Wealth-Index-Construction.cfm
# Rustein article with SPSS code: https://dhsprogram.com/programming/wealth%20index/Steps_to_constructing_the_new_DHS_Wealth_Index.pdf
# SPSS code was translated into R with the help of ChatGPT through ChatIHME
# it was then reviewed and adapted for the current project

# There will be some differences between rounds, so the code below may need to be modified if a
# variable is excluded in one round.  The same variables need to be used in
# construction of the index across all nids for a given country.


##########################################################
## ASSETS AND PHYSICAL CHARACTERISTICS OF THE RESIDENCE ##
##########################################################

# Number of members per sleeping room

  ## convert to numeric in case character
  #Setting up if else statements for some surveys
  if (survey %like% "MWI_DHS4_2004") {
    cols_to_convert <- c("hv012", "hv013", "sh29a")
  } else if (survey %like% "GHA_DHS4_2004") {
    cols_to_convert <- c("hv012", "hv013", "hv216")
  } else if (survey %like% "MWI_DH6_2010") {
    cols_to_convert <- c("hv012", "hv013", "hv216")
  } else if (survey %like% "RWA_DH5_2005") {
    cols_to_convert <- c("hv012", "hv013", "hv216")
  } else {
    cols_to_convert <- c("hv012", "hv013", "sh26a", "hv216")
  }
  #continue with the rest of the conversions
  cols_to_convert <- cols_to_convert[cols_to_convert %in% names(dt)]
  cols_to_convert <- cols_to_convert[sapply(dt[, ..cols_to_convert], class) != "numeric"]

  dt[, (cols_to_convert) := lapply(.SD, as.numeric), .SDcols = cols_to_convert]
  if (survey %like% "CMR_DHS4_2004") { ## changed hv216 to sh26a for CMR 2004
     # Rooms for sleeping in the household: not in MWI 2000 nor RWA 2000 nor NPL 2001 nor MWI 2004 nor RWA 2005

  dt[, memsleep :=  ifelse(hv012 == "0", hv013, hv012) ]# de jure, if not, de factor hh members
  dt[, memsleep :=  ifelse(sh26a > 0, trunc(memsleep / sh26a), memsleep)] # 216 is number of rooms
  dt[, memsleep := ifelse(sh26a == 0, hv012, memsleep)] # if no rooms, all sleep together
  dt[, memsleep := ifelse(memsleep  >= 95, NA, memsleep)]
  } else if (!survey %like% "MWI_DHS4_2000" &!survey %like% "RWA_DHS4_2000"
             &!survey %like% "NPL_DHS4_2001" &!survey %like% "GHA_DHS4_2003"
             &!survey %like% "MWI_DHS4_2004" &!survey %like% "RWA_DHS5_2005") {
    dt[, memsleep :=  ifelse(hv012 == 0, hv013, hv012) ]# de jure, if not, de factor hh members
    dt[, memsleep :=  ifelse(hv216 > 0, trunc(memsleep / hv216), memsleep)] # 216 is number of rooms
    dt[, memsleep <- ifelse(hv216 == 0, hv012, memsleep)] # if no rooms, all sleep together
    dt[, memsleep := ifelse(memsleep  >= 95, NA, memsleep)]
  } else if (survey %like% "MWI_DH6_2010" & survey %like% "RWA_DHS6_2010"
             & survey %like% "RWA_DHS7_2014" & survey %like% "NPL_DHS5_2006"
             & survey %like% "NPL_DHS6_2011" & survey %like% "NPL_DHS7_2016") {
    # Apply logic specific to this survey
    # You can use 'hv216' here as it is available in this survey
    dt[, memsleep :=  ifelse(hv012 == 0, hv013, hv012) ]
    dt[, memsleep :=  ifelse(hv216 > 0, trunc(memsleep / hv216), memsleep)]
    dt[, memsleep <- ifelse(hv216 == 0, hv012, memsleep)]
    dt[, memsleep := ifelse(memsleep  >= 95, NA, memsleep)]
  }

# Electricity
dt[, electricity := ifelse(hv206 == "yes", 1, 0) ]

## Assets
dt[, radio := ifelse(hv207 == "yes", 1, 0)  ]
dt[, tv_color := ifelse(hv208 == "yes", 1, 0)  ]
dt[, fridge := ifelse(hv209 == "yes", 1, 0)  ]
dt[, bike := ifelse(hv210 == "yes", 1, 0)  ]
dt[, moto := ifelse(hv211 == "yes", 1, 0)  ] #motorcycle or scooter
dt[, car := ifelse(hv212 == "yes", 1, 0)  ]
dt[, land_ph := as.integer(hv221 == "yes")]

if ("hv243a" %in% names(dt)){
  print('in dt')
  dt[, mobile_ph := as.integer(hv243a == "yes")]
}

if ("hv243b" %in% names(dt)){
  print('in dt')
  dt[, watch := as.integer(hv243b == "yes")]
}

if ("hv243c" %in% names(dt)){
  print('in dt')
  dt[, animal_cart := as.integer(hv243c == "yes")]
}

if ("hv243d" %in% names(dt)){
  print('in dt')
  dt[, motor_boat := as.integer(hv243d == "yes")]
}

if ("sh110s" %in% names(dt)){
  print('in dt')
  dt[, internet := as.integer(sh110s == "yes")] # via any device, not in original DHS file, may need to be cut NOT IN CMR
}
if ("sh110l" %in% names(dt)){
  print('in dt')
  dt[, comp := as.integer(sh110l == "yes")] # or tablet, not in original DHS file, may need to be cut NOT IN CMR
}

if ("sh110k" %in% names(dt)){
  print('in dt')
  dt[, wash_mach := as.integer(sh110k == "yes")] # not in original DHS file, may need to be cut NOT IN CMR
}

## Livestock
#hh$any_livestock <- ifelse(hh$hv246 > 0, 1, 0) # does not exist in ghana
if ("hv246a" %in% names(dt)){ #NOT IN CMR 2004 or MWI 2000
  print('in dt')
dt[, cattle := as.integer(hv246a > 0)]
}
if ("hv246b" %in% names(dt)){ #NOT IN CMR 2004 or MWI 2000
  print('in dt')
dt[, cows := as.integer(hv246b > 0)]
}
if ("hv246c" %in% names(dt)){ #NOT IN CMR 2004 or MWI 2000
  print('in dt')
  dt[, horse_donk := as.integer(hv246c > 0)]
}
if ("hv246d" %in% names(dt)){ #NOT IN CMR 2004 or MWI 2000
  print('in dt')
  dt[, goat := as.integer(hv246d > 0)]
}
if ("hv246e" %in% names(dt)){ #NOT IN CMR 2004 or MWI 2000
  print('in dt')
  dt[, sheep := as.integer(hv246e > 0)]
}
if ("hv246f" %in% names(dt)){ #NOT IN CMR 2004 or MWI 2000
  print('in dt')
  dt[, chickens := as.integer(hv246f > 0)]
}

# Bank account
if ("hv246f" %in% names(dt)){ #NOT IN CMR 2004 or MWI 2000
  print('in dt')
  dt[, bank_acc := as.integer(hv247 == "yes")]
}

# Drinking water supply
dt[, h2oires := as.integer(hv201 %in% c("piped into dwelling", "in the house"))]  # or 11 "Piped into dwelling"
dt[, h2oyrd := as.integer(hv201 %in% c("piped to yard/plot", "piped into yard/plot", "in the courtyard", "piped into house/yard/plot"), "piped into compound/plot" )]   # or 12 "Piped into yard/plot"
dt[, h2opub := as.integer(hv201 %in% c("public tap/standpipe", "community standpipe", "public tap",
                                       "of a neighbor", "along the road", "public/nieghbor's tap", "stone tap/dhara"))]   # or 14 "Public tap / standpipe"
dt[, h2obwell := as.integer(hv201 %in% c("tube well or borehole", "borehole", "well in house/yard/plot", "public/neighbor's well","tubewell in yard/plot"))] # or 21 "Tube well or borehole"
dt[, h2ipwell := as.integer(hv201 %in% c("protected well", "protected well in yard/plot",
                                         "protected without pump", "well with pump","protected public well",
                                         "protected well in dwelling"))] #or 31 "Protected dug well"
dt[, h2iowell := as.integer(hv201 %in% c("unprotected well","open well in yard/plot","non protected well","open public well"))] #or 32 "Unprotected dug well"
dt[, h2opspg := as.integer(hv201 == "protected spring","protected source")]  # or 41 "Protected Spring"
dt[, h2ouspg := as.integer(hv201 %in% c("unprotected spring", "spring"))]  #or 42 "Unprotected Spring"
dt[, h2orain := as.integer(hv201 == "rainwater")]  # or 51 "Water from rain"
dt[, h2otruck := as.integer(hv201 %in% c("tanker truck", "tanker truck/boswer"))] #or 61 "Water from tanker truck"
dt[, h2ocart := as.integer(hv201 == "cart with small tank")] #or 62 "Water from cart with small tank"
dt[, h2osurf := as.integer(hv201%in% c("river/stream","dam", "pond/lake", "river/dam/lake/ponds/stream/canal/irrigation channel",
                                       "river, stream", "pond, lake","river/stream not protected", "sprong/kuwa","river/stream/pond/lake" ))]  # or 43 "Surface water-river, lake, dam, etc."
dt[, h2obot := as.integer(hv201 %in% c("bottled water","satchel water"))]  #or 71 "Water from bottle"
dt[, h2ooth := as.integer(hv201 =="other")]  # or 96"Other water source"


## improved water supply
dt[!is.na(h2oires), h20_home := 0]
dt[h2oires == 1, h20_home := 1]

dt[!is.na(h2oires), h20_improve := 0]
dt[h2oyrd == 1 | h2opub == 1 | h2obwell == 1 | h2ipwell == 1 | h2otruck == 1 | h2obot == 1, h20_improve := 1]

# Toilet facilities
if ("hv205" %in% names(dt)){
# Perform the operations on the deduplicated subset
dt[, flushs := ifelse(hv205 == "flush to piped sewer system", 1, 0)]
dt[, flusht := ifelse(hv205 == "flush to septic tank", 1, 0)]
dt[, flushp := ifelse(hv205 == "flush to pit latrine", 1, 0)]
dt[, flushe := ifelse(hv205 == "flush to somewhere else", 1, 0)]
dt[, flushdk := ifelse(hv205 %in% c("flush, don't know where", "flush toilet"), 1, 0)]
dt[, latvip := ifelse(hv205 %in% c("ventilated improved pit latrine (vip)", "ventilated improved pit (vip) latrine","ventilated/improved pit latrine",
                                   "ventilated improved pit latrine", "vip latrine"), 1, 0)]
dt[, latpits := ifelse(hv205 %in% c("pit latrine with slab", "improved pit toilet latrine"), 1, 0)]
dt[, latpit := ifelse(hv205 %in% c("pit latrine without slab/open pit", "rudimentary pit toilet latrine", "traditional pit toilet"), 1, 0)]
dt[, latpail := ifelse(hv205 %in% c("bucket toilet","bucket, pan"), 1, 0)]
dt[, lathang := ifelse(hv205 == "hanging toilet/latrine", 1, 0)]
dt[, latbush := ifelse(hv205 %in% c("no facility/bush/field", "no facility","no facility, bush, field", "no facility/bush, field"), 1, 0)]
dt[, latoth := ifelse(hv205 %in% c("Other toilet facility", "Other type of latrine/toilet", "other"), 1, 0)]

} else {
  print("Toilet facilities info is missing")
  dt[, toilet_facilities := NA]
}

#improved toilet facilities
dt[, toilet_improve := 0]
dt[flushs == 1, toilet_improve := 1]
dt[flusht == 1, toilet_improve := 1]
dt[flushp == 1, toilet_improve := 1]
dt[flushe == 1, toilet_improve := 1]
dt[flushdk == 1, toilet_improve := 1]
dt[latvip == 1, toilet_improve := 1]

# Toilet share

# Create or modify the `shared_toilet` column
if ("hv225" %in% names(dt)){
  dt[, shared_toilet := as.integer(hv225 %in% c("yes, other household only", "yes, public", "yes"))]
} else {
  print("shared toilet info is missing")
  dt[, shared_toilet := NA]
}

# Create or modify other columns based on conditions
dt[, latshare := ifelse(shared_toilet == 1, 1, 0)]
dt[, sflushs := ifelse(flushs == 1 & shared_toilet == 1, 1, 0)]
dt[, sflusht := ifelse(flusht == 1 & shared_toilet == 1, 1, 0)]
dt[, sflushp := ifelse(flushp == 1 & shared_toilet == 1, 1, 0)]
dt[, sflushe := ifelse(flushe == 1 & shared_toilet == 1, 1, 0)]
dt[, slatvip := ifelse(latvip == 1 & shared_toilet == 1, 1, 0)]
dt[, slatpits := ifelse(latpits == 1 & shared_toilet == 1, 1, 0)]
dt[, slatpit := ifelse(latpit == 1 & shared_toilet == 1, 1, 0)]
dt[, slathang := ifelse(lathang == 1 & shared_toilet == 1, 1, 0)]
dt[, slatoth := ifelse(latoth == 1 & shared_toilet == 1, 1, 0)]


# Flooring
dt[, dirtfloo := ifelse(hv213 %in% c("earth, sand", "dung", "earth/sand", "earth / sand", "palm/bamboo", "earth, mud, dung",
                                     "mud mixed with dung","earth/sand/mud"  ), 1, 0)]
dt[, woodfloo := ifelse(hv213 == "wood planks", 1, 0)]
dt[, prqfloo := ifelse(hv213 %in% c("parquet, polished wood", "parquet or polished wood"), 1, 0)]
dt[, vinlfloo := ifelse(hv213 %in% c("vinyl or asphalt strips","vinyl, asphalt strips", "linoleum/rubber carpet", "vinyl or asphalt strips","linoleum, carpet",
                                     "linoleum" ), 1, 0)]
dt[, tilefloo := ifelse(hv213 %in% c("ceramic/marble/porcelain tiles / terraz","ceramic tiles","ceramic tiles/coastal brick","terrazzo"  ), 1, 0)]
dt[, cemtfloo := ifelse(hv213 %in% c( "bricks without cement", "cement"), 1, 0)]
dt[, rugfloo := ifelse(hv213 %in% c("woolen carpets/ synthetic carpet", "carpet"), 1, 0)]
dt[, othfloo := ifelse(hv213 == "other", 1, 0)]

## improved floor
dt[!is.na(dirtfloo), floor_improve := 1]
dt[dirtfloo == 1 | woodfloo == 1 | othfloo == 1, floor_improve := 0]

# Walls
## all na for CMR 2004 & MWI 2000 & RWA 2000 & NPL 2001
if ("hv214" %in% names(dt)) {
dt[, nowall := ifelse(hv214 == "no walls", 1, 0)]
dt[, natwall := ifelse(hv214 %in% c("cane / palm / trunks", "dirt","cane/palm/trunks","tree trunks with mud and cement",
                                    "bamboo/tree trunks with mud"), 1, 0)]
dt[, mudwall := ifelse(hv214 == "bambo with mud", 1, 0)]
dt[, stonwall := ifelse(hv214 == "stone with mud", 1, 0)]
dt[, adobwall := ifelse(hv214 == "uncovered adobe", 1, 0)]
dt[, metalwall := ifelse(hv214 == "metal/galvanized sheet",1,0)]
dt[, plywall := ifelse(hv214 == "plastic sheeting" , 1, 0)]
dt[, plasticwall := ifelse(hv214 == "plywood", 1, 0)]
dt[, cardwall := ifelse(hv214 == "cardboard", 1, 0)]
dt[, rwoodwall := ifelse(hv214 == "reused wood", 1, 0)]
dt[, cmtwall := ifelse(hv214 == "cement", 1, 0)]
dt[, stonwall := ifelse(hv214 %in% c("stone with lime / cement","stone with lime/cement", "stone with mud"), 1, 0)]
dt[, brkwall := ifelse(hv214%in%c("bricks","oven fired bricks","oven fired bricks with cement"), 1, 0)]
dt[, cmtbwall := ifelse(hv214 %in% c("cement block", "cement blocks"), 1, 0)]
dt[, cadobwall := ifelse(hv214 %in% c("covered adobe with cement", "covered adobe"), 1, 0)]
dt[, woodwall := ifelse(hv214 %in% c("wood plans / shingles", "wood planks/shingles","reused wood" ), 1, 0)]
dt[, othwall := ifelse(hv214 == "other", 1, 0)]

## improved walls
## all na for CMR
dt[!is.na(nowall), wall_improve := 1]
dt[nowall == 1 | natwall == 1 | mudwall == 1 | stonwall == 1 | adobwall == 1 | cardwall == 1 | othwall == 1 | plasticwall==1, wall_improve := 0]
}

# Roofing
## all na for CMR 2004 and RWA 2000
if ("hv215" %in% names(dt)) {
dt[, noroof := ifelse(hv215 == "no roof", 1, 0)]
dt[, metalroof := ifelse(hv215 %in% c("metal sheet","metal/galvanized sheet"), 1, 0)]
dt[, natroof := ifelse(hv215 %in% c("thatch / palm leaf", "thatch/palm leaf"), 1, 0)]
dt[, matroof := ifelse(hv215 %in% c("rustic materials/plastic", "rustic mat"), 1, 0)]
dt[, bambroof := ifelse(hv215 %in% c("palm / bamboo", "palm/bamboo"),1, 0)]
dt[, wproof := ifelse(hv215 == "wood planks", 1, 0)]
dt[, cardroof := ifelse(hv215 == "cardboard", 1, 0)]
dt[, metroof := ifelse(hv215 == "metal", 1, 0)]
dt[, woodroof := ifelse(hv215 == "wood", 1, 0)]
dt[, asbroof := ifelse(hv215 == "asbestos/slate roofing sheets", 1, 0)]
dt[, tileroof := ifelse(hv215 %in% c("ceramic tiles","industrial tiles"), 1, 0)]
dt[, cmtroof := ifelse(hv215 %in% c("cement", "calamine / cement fiber", "calamine/cement fiber","cement/concrete"), 1, 0)]
dt[, shngroof := ifelse(hv215 == "roofing shingles", 1, 0)]
dt[, othroof := ifelse(hv215 == "other", 1, 0)]


## improved roofing
dt[!is.na(noroof), roof_improve := 1]
dt[noroof == 1 | natroof == 1 | matroof == 1 | bambroof == 1 | woodroof == 1 | cardroof == 1 | othroof == 1, roof_improve := 0]
}
# Cooking Fuel
if ("hv226" %in% names(dt)){
  dt[, cookelec := ifelse(hv226 == "electricity", 1, 0)]
  dt[, cookchulo := ifelse(hv226 == "improved smokeless chulo", 1, 0)]
  dt[, cooklpg := ifelse(hv226 %in% c( "lpg / natural gas", "lpg", "lpg, natural gas","liquefied petroleum gas (lpg)/cooking gas"), 1, 0)] ## not sure if "lpg, natural gas" should go in lpg or cookbio for CMR 2004
  dt[, cookgas := ifelse(hv226%in%c("cooking gas","gasoline/diesel"), 1, 0)]
  dt[, cookbio := ifelse(hv226 %in% c("natural gas","biogas","piped natural gas"), 1, 0)]
  dt[, cookkero := ifelse(hv226 %in% c("kerosene","kerosene/paraffin"), 1, 0)]
  dt[, cookcoal := ifelse(hv226 %in% c("coal, lignite","coal/lignite"), 1, 0)]
  dt[, cookchar := ifelse(hv226 %in%c( "briquette", "charcoal"), 1, 0)]
  dt[, cookwood := ifelse(hv226 %in% c("wood", "firewood, straw", "traditional firewood/charcoal/dung", "sawdust",
                                       "processed biomass (pellets) or woodchips","saw dust" ), 1, 0)] ## not sure if "firewood, straw" should go in straw or wood for CMR 2004 and MWI 2000
  dt[, cookstraw := ifelse(hv226 == "straw/shrubs/grass", 1, 0)]
  dt[, cookcrop := ifelse(hv226 == "agricultural crop", 1, 0)]
  dt[, cooksolar := ifelse(hv226 == "solar energy", 1, 0)]
  dt[, cookdung := ifelse(hv226 %in% c("dung", "animal dung","animal dung/waste"), 1, 0)]
  dt[, cooknone := ifelse(hv226 == "no food cooked in house", 1, 0)]
  dt[, cookoth := ifelse(hv226 == "other", 1, 0)]
  } else {
    print("'hv226' is not a column in the data frame.")
    dt[, c("cookelec", "cookchulo", "cooklpg", "cookgas", "cookbio", "cookkero", "cookcoal", "cookchar", "cookwood", "cookstraw", "cookcrop", "cooksolar", "cookdung", "cooknone", "cookoth") := NA]
  }

# modern fuel
dt[!is.na(cookelec), mod_fuel := 1]
dt[ cookchar == 1 | cookwood == 1 | cookstraw == 1 |  cookdung == 1 | cookoth == 1, mod_fuel := 0]

# Land area owned
if (all(c("hv244", "hv245") %in% names(dt))){
  dt$hv245 <- as.numeric(dt$hv245)
  dt[, any_land := ifelse(hv244 == "yes", 1, 0)]
  dt[, landarea := ifelse(!is.na(hv245), hv245/10, NA)]
  dt[any_land == 1 & hv245 == 0, landarea := 0.5]
  dt[is.na(hv245) | hv245 == 998, landarea := NA]
  dt[any_land == 0, landarea := 0]
}


## THESE VARIABLES IN THE DHS EXAMPLE DO NOT EXIST IN 2014 GHANA DHS
# 1) domestic servant or not
#hh$domestic <- 0
#hh$domestic[hh$hv101 == 6] <- 1 # need to go through each member
#from individual recode
#hh$domestic[hh$hv717 == 6] <- 1 # if not related to head of household (v716)
# needs to be done for parnter
#hh$domestic[hh$hv704 == 6] # if not related to household head (v705)
# 2) Solid waste/garbage collection
# 3) Whether own house


#standardize numerical values and impute with mean where have lots of NAs
if ("landarea" %in% names(dt)){## missing in CMR 2004 & MWI 2000
  dt$landarea <- scale(dt$landarea, center =TRUE, scale = TRUE)
  dt$landarea[is.na(dt$landarea) == TRUE] <- mean(dt$landarea, na.rm =TRUE)
}
if ("memsleep" %in% names(dt)) {## missing in MWI 2000
dt$memsleep <- scale(dt$memsleep, center =TRUE, scale = TRUE)
dt$memsleep[is.na(dt$memsleep) == TRUE] <- mean(dt$memsleep, na.rm =TRUE)
}

## filter just to vars of interest before saving to save space

save_vars <- c( id_vars,"hhid_unique", 'year', "electricity", "radio", "tv_color", "fridge", "bike", "moto", "car",
                    "land_ph", "mobile_ph", "watch", "animal_cart", "motor_boat", "bank_acc",
                    "internet", "comp", "wash_mach", "any_land", "landarea",
                    "memsleep", "h2oires", "h2oyrd", "h2opub", "h2obwell",
                    "h2ipwell", "h2iowell", "h2opspg", "h2ouspg", "h2orain",
                    "h2otruck", "h2ocart", "h2osurf", "h2obot", "h2ooth", "flushs",
                    "flusht", "flushp", "flushe", "latvip", "latpits", "latpit", "latpail",
                    "lathang", "latbush", "latoth", "latshare", "sflushs", "sflusht",
                    "sflushp", "slatvip", "slatpits", "slatpit", "slathang", "slatoth",
                    "dirtfloo", "woodfloo", "prqfloo", "vinlfloo", "tilefloo", "cemtfloo",
                    "rugfloo", "othfloo", "nowall", "natwall", "mudwall", "stonwall", "adobwall",
                    "plywall", "cardwall", "rwoodwall", "cmtwall", "brkwall", "cmtbwall", "cadobwall",
                    "woodwall", "othwall", "noroof", "natroof", "matroof", "bambroof", "wproof",
                    "cardroof", "metroof", "woodroof", "asbroof", "tileroof", "cmtroof", "shngroof",
                    "othroof", "cookelec", "cooklpg", "cookgas", "cookbio", "cookkero", "cookcoal",
                    "cookchar", "cookwood", "cookstraw", "cookcrop", "cookdung", "cooknone", "cookoth","hv025","h20_improve",
                "toilet_improve", "floor_improve","roof_improve", "mod_fuel","hv271","hv270" )
cur_vars <- names(dt)[names(dt) %in% save_vars]
dt <- dt[, ..cur_vars]
dt[, survey :=  str_extract(file_name, "[A-Z]{3}_DHS[0-9]_[0-9]{4}(_[0-9]{4})*")]

## save dt for factor analysis once vars are sorted
write.csv(dt, file.path(out.dir, paste0(file_name, "_hh_wealth_quintile_dt.csv")), row.names = FALSE)
}

## CMR
prep_hh_data("CMR_DHS4_2004_HH_CMHR44FL_Y2018M05D29.DTA")
prep_hh_data("CMR_DHS7_2018_2019_HH_CMHR71FL_Y2020M06D10.DTA")
prep_hh_data("CMR_DHS5_2011_HH_CMHR61FL_Y2018M06D11.DTA")

## GHA
prep_hh_data("GHA_DHS4_2003_HH_GHHR4BFL_Y2018M10D15.DTA")
prep_hh_data("GHA_DHS4_1998_1999_HH_GHHR41FL_Y2018M10D15.DTA")
prep_hh_data("GHA_DHS5_2008_HH_GHHR5AFL_Y2018M10D15.DTA")
prep_hh_data("GHA_DHS7_2014_HH_GHHR72FL_Y2018M12D10.DTA")
prep_hh_data("GHA_DHS8_2022_2023_HH_GHHR8AFL_Y2024M01D22.DTA")

## MWI
 prep_hh_data("MWI_DHS4_2000_HH_MWHR41FL_Y2019M01D07.DTA")
prep_hh_data("MWI_DHS7_2015_2016_HH_MWHR7AFL_Y2019M12D11.DTA")
prep_hh_data("MWI_DHS4_2004_2005_HH_MWHR4EFL_Y2018M12D10.DTA")
prep_hh_data("MWI_DHS6_2010_HH_MWHR61FL_Y2018M12D10.DTA")

## NPL

prep_hh_data("NPL_DHS4_2001_HH_NPHR41FL_Y2019M02D19.DTA")
prep_hh_data("NPL_DHS5_2006_HH_NPHR51FL_Y2018M11D05.DTA")
prep_hh_data("NPL_DHS6_2011_HH_NPHR60FL_Y2018M11D05.DTA")
prep_hh_data("NPL_DHS7_2016_2017_HH_NPHR7HFL_Y2018M11D05.DTA")
prep_hh_data("NPL_DHS8_2022_HH_NPHR81FL_Y2023M06D23.DTA")

## RWA
prep_hh_data("RWA_DHS8_2019_2020_HH_RWHR81FL_Y2021M10D05.DTA")
prep_hh_data("RWA_DHS4_2000_HH_RWHR41FL_Y2019M03D18.DTA")
prep_hh_data("RWA_DHS5_2005_HH_RWHR53FL_Y2019M03D18.DTA")
prep_hh_data("RWA_DHS6_2010_2011_HH_RWHR61FL_Y2019M04D16.DTA")
prep_hh_data("RWA_DHS7_2014_2015_HH_RWHR70FL_Y2019M04D16.DTA")

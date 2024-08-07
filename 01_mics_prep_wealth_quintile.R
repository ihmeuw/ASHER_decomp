#-------------------Header------------------------------------------------
# Project: IHME ASHER Decomposition
# Purpose: Extract components of the wealth index from MICS surveys
# Date: 12/6/2023
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
pacman::p_load(magrittr,tidyverse,parallel,plyr,dplyr,haven,survey,tools,devtools)

# in/out
out.dir <- "FILEPATH"

# create function for the opposite of %in%
'%ni%' <- Negate('%in%')


# EXTRACT DATA FUNCTION -----------------------------------------------

extract_data <- function(survey, cur_country) {
  
  # read in data 
  if (grepl("NPL_MICS6", survey)) {
    dt <- data.table(read_sav(survey))
  } else {
    dt <- data.table(read_dta(survey))
  }

  
  ## SURVEY CHARACTERISTICS ----------------------------------------
  message("||---Survey characteristics")
  
  # convert variable names to lowercase
  names(dt) <- tolower(names(dt))
  
  # convert character columns to lowercase
  dt[] <- lapply(dt, function(x) if(is.character(x)) tolower(x) else x)
  
  # extract survey name and add to dt
  survey_name <- str_extract(survey, "[A-Z]{3}_MICS[0-9]_[0-9]{4}(_[0-9]{4})*")
  dt[, survey := survey_name]
  
  # identifiers
  dt[, cluster := ifelse(grepl("MICS2", survey), hi1, hh1)] 
  dt[, hh_id := ifelse(grepl("MICS2", survey), hi2, hh2)]
  dt[, area_unit := ifelse(grepl("MICS2", survey), hi6, hh6)]
  dt[, country := cur_country]
  dt[, region := ifelse(grepl("RWA_MICS2", survey), as.character(as_factor(province)), 
                        ifelse(grepl("MICS2", survey), as.character(as_factor(hi7)), 
                               ifelse(grepl("MWI_MICS3", survey), as.character(as_factor(hhreg)), as.character(as_factor(hh7)))))]
  message(paste0("||-Unique regions: ", paste(unique(dt$region), collapse=", ")))
  
  # strata 
  dt[, strata := ifelse(grepl("CMR_MICS2", survey), paste(hi7, strates), 
                        ifelse(grepl("RWA_MICS2", survey), paste(province, hi6),
                               ifelse(grepl("MWI_MICS3", survey), paste(hhdis, hh6),
                                      ifelse(grepl("MWI_MICS5", survey), strata,
                                             ifelse(grepl("CMR_MICS3|GHA_MICS3", survey), paste(hh7, hh6), stratum)))))]
  
  
  # household's sample weight
  dt[, hhweight := hhweight]
  
  # unique hh id for each household
  dt[, hhid_unique := paste(hh_id, cluster)]
  

  ## Household Assets ----------------------------------------
  message("||---Household assets")
  
  # Persons per sleeping room
  dt[, n_rooms_sleeping := ifelse(grepl("MICS2", survey), hi9, ifelse(grepl("MICS[3-5]", survey), hc2, hc3))]
  dt[, n_hh_members := ifelse(grepl("MICS2", survey), himem, 
                              ifelse(grepl("MWI_MICS3", survey), hh10, 
                                     ifelse(grepl("MICS[3-5]", survey), hh11, hh48)))]
  dt[n_rooms_sleeping < 98, memsleep := n_hh_members / n_rooms_sleeping]
  
  # Electricity
  # MISSING FROM CMR 2000 AND RWA 2000
  if (!grepl("MICS2", survey)) {
    dt[, electricity := ifelse(grepl("MICS3", survey), hc9a, ifelse(grepl("MICS[4-5]", survey), hc8a, hc8))]
    if (grepl("MICS[3-5]", survey)) dt[, electricity := ifelse(electricity %in% c(1), 1, 0)]
    if (grepl("MICS6", survey)) dt[, electricity := ifelse(electricity %in% c(1,2), 1, 0)]
  }
  
  # Assets
  
  # radio
  dt[, radio := ifelse(grepl("MICS3", survey), hc9b, ifelse(grepl("MICS[4-5]", survey), hc8b, hc7b))]    
  
  # refrigerator
  dt[, fridge := ifelse(grepl("MWI_MICS3", survey), hc9f, ifelse(grepl("MICS3", survey), hc9h, ifelse(grepl("MICS[4-5]", survey), hc8e, hc9b)))] 
 
  # bicycle
  dt[, bike := ifelse(grepl("GHA_MICS3", survey), hc10a, ifelse(grepl("MICS3", survey), hc10b, ifelse(grepl("MICS[4-5]", survey), hc9c, hc10b)))]
  
  # motorcycle/scooter
  dt[, moto := ifelse(grepl("GHA_MICS3", survey), hc10b, ifelse(grepl("MICS3", survey), hc10c, ifelse(grepl("MICS[4-5]", survey), hc9d, hc10c)))]
  
  # car/truck/van
  dt[, car := ifelse(grepl("GHA_MICS3", survey), hc10d, ifelse(grepl("MICS3", survey), hc10e, ifelse(grepl("MICS[4-5]", survey), hc9f, hc10e)))]
  
  # fixed telephone line
  dt[, land_ph := ifelse(grepl("MWI_MICS3", survey), hc9e, ifelse(grepl("MICS3", survey), hc9g, ifelse(grepl("MICS[4-5]", survey), hc8d, hc7a)))]
  
  # mobile telephone
  dt[, mobile_ph := ifelse(grepl("MWI_MICS3", survey), hc9d, ifelse(grepl("MICS3", survey), hc9f, ifelse(grepl("MICS[4-5]", survey), hc9b, hc12)))]
  
  # wristwatch
  # missing from CMR 2006, GHA 2006
  if (!grepl("CMR_MICS3|GHA_MICS3", survey)) dt[, watch := ifelse(grepl("MWI_MICS3", survey), hc10a, ifelse(grepl("MICS[4-5]", survey), hc9a, hc10a))]
  
  # animal drawn cart
  dt[, animal_cart := ifelse(grepl("GHA_MICS3", survey), hc10c, ifelse(grepl("MICS3", survey), hc10d, ifelse(grepl("MICS[4-5]", survey), hc9e, hc10d)))]
  
  # boat with motor
  # NPL surveys do not specify with motor
  if (!grepl("NPL",survey)) dt[, motor_boat := ifelse(grepl("MICS3", survey), hc10f, ifelse(grepl("MICS[4-5]", survey), hc9g, hc10f))]
  
  # internet at home
  # missing from GHA 2006, 2011, CMR 2006, MWI 2013-14, NPL 2010, 2014
  if (grepl("MICS6|MWI_MICS3|CMR_MICS5", survey)) dt[, internet := ifelse(grepl("MICS3", survey), hc9h, ifelse(grepl("MICS5", survey), hc8i, hc13))]    
  
  # computer/tablet
  dt[, comp := ifelse(grepl("MWI_MICS3", survey), hc9g, 
                      ifelse(grepl("MICS3", survey), hc9d, 
                             ifelse(grepl("NPL_MICS4", survey), hc8l, 
                                    ifelse(grepl("GHA_MICS4", survey), hc8h,
                                           ifelse(grepl("CMR_MICS5", survey), hc8f,
                                                  ifelse(grepl("NPL_MICS5", survey), hc8l,
                                                         ifelse(grepl("MWI_MICS5", survey), hc8k, hc11)))))))]
  # sometimes laptop asked separately, record then combine with computer
  if (grepl("GHA_MICS4|NPL_MICS5", survey)) {
    dt[, laptop := ifelse(grepl("GHA_MICS4", survey), hc8g, hc9h)]
    dt[comp != 1 & laptop == 1, comp := 1]
  } 
  
  # television
  dt[, tv := ifelse(grepl("MICS3", survey), hc9c, ifelse(grepl("MICS[4-5]", survey), hc8c, ifelse(grepl("GHA_MICS6", survey), hc9d, hc9a)))]
  # sometimes color, or plasma tv asked separately, record then combine with tv
  if (grepl("GHA_MICS(4|6)", survey)) {
    dt[, tv_color := ifelse(grepl("MICS4", survey), hc8c1, hc9e)]
    dt[tv != 1 & tv_color == 1, tv := 1]
    if (grepl("MICS6", survey)) {
      dt[, tv_plasma := hc9f]
      dt[tv != 1 & tv_plasma == 1, tv := 1]
    }
  }
  
  # washing machine
  if (grepl("GHA_MICS(4|6)|NPL", survey)) dt[, wash_mach := ifelse(grepl("NPL_MICS(4|5)", survey), hc8q, 
                                                                   ifelse(grepl("GHA_MICS4", survey), hc8f, 
                                                                          ifelse(grepl("GHA_MICS6", survey), hc9h, hc9d)))]
  
  # recode all assets from 1/2/9 to 1/0/NA
  asset_cols <- c("radio","fridge","bike","moto","car","land_ph","mobile_ph","watch",
                  "animal_cart","motor_boat","internet","comp","tv","wash_mach")
  cols <- names(dt)[names(dt) %in% asset_cols]
  dt[, (cols) := lapply(.SD, function(x) ifelse(x == 1, 1, ifelse(x == 2, 0, NA))), .SDcols = cols]
  
  # for some MICS6 surveys, if a household does not have electricity, questions about fridge, tv, washing machine are skipped
  if (grepl("NPL_MICS6|GHA_MICS6", survey)) dt[electricity == 0, c("fridge", "tv", "wash_mach") := 0]
  
  
  ## Livestock ----------------------------------------
  message("||---Livestock")
  
  # generate continuous counts
  
  # cows and cattle (sometimes separate, sometimes grouped)
  if (grepl("NPL|MWI_MICS6", survey)) dt[, cows_only := ifelse(grepl("MICS(4|5)", survey), hc14a, hc18a)]
  if (grepl("MICS(3|6)", survey)) dt[, cattle_only := ifelse(grepl("MICS3", survey), hc14a, hc18a)]
  if (grepl("GHA_MICS(4|6)|MWI_MICS5|CMR_MICS5", survey)) dt[, cows_cattle := ifelse(grepl("MICS(4|5)", survey), hc14a, hc18a)]

  # horses, donkeys
  dt[, horse_donk := ifelse(grepl("CMR_MICS3", survey), hc14c, ifelse(grepl("MICS[3-5]", survey), hc14b, hc18c))]
  
  # goats
  # WARNING: NPL 2010 GOATS ARE GROUPED WITH SHEEP
  dt[, goat := ifelse(grepl("CMR_MICS3", survey), hc14d, ifelse(grepl("MICS[3-5]", survey), hc14c, hc18d))]
  
  # sheep
  # WARNING: NPL 2010 THIS IS "LAMB"
  dt[, sheep := ifelse(grepl("CMR_MICS3", survey), hc14e, ifelse(grepl("MICS[3-5]", survey), hc14d, hc18e))]
  
  # chickens
  dt[, chickens := ifelse(grepl("MWI_MICS3", survey), hc14f, ifelse(grepl("MICS3", survey), hc14g, ifelse(grepl("MICS[4-5]", survey), hc14e, hc18f)))]
  
  # pigs
  dt[, pigs := ifelse(grepl("CMR_MICS3", survey), hc14f, ifelse(grepl("MICS3", survey), hc14e, ifelse(grepl("MICS[4-5]", survey), hc14f, hc18g)))]
  
  # replace NA's with 0's
  livestock_cols <- c("cows_only","cattle_only","cows_cattle","horse_donk","goat","sheep","chickens","pigs")
  cols <- names(dt)[names(dt) %in% livestock_cols]
  dt[, (cols) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = cols]
  
  # replace "missing" with NA's
  dt[, (cols) := lapply(.SD, function(x) ifelse(x > 97, NA, x)), .SDcols = cols]
  
  # generate grouped animal variables
  # not used for our DHS wealth index version, but are in the MICS wealth index
  if ("cows_only" %in% names(dt)) {
    dt[, cows_only0 := ifelse(cows_only == 0, 1, ifelse(is.na(cows_only), 9, 0))]
    dt[, cows_only1 := ifelse(cows_only >= 1 & cows_only <= 4, 1, ifelse(is.na(cows_only), 9, 0))]
    dt[, cows_only5 := ifelse(cows_only >= 5 & cows_only <= 9, 1, ifelse(is.na(cows_only), 9, 0))]
    dt[, cows_only10 := ifelse(cows_only >= 10, 1, ifelse(is.na(cows_only), 9, 0))]
  }
  
  if ("cattle_only" %in% names(dt)) {
    dt[, cattle_only0 := ifelse(cattle_only == 0, 1, ifelse(is.na(cattle_only), 9, 0))]
    dt[, cattle_only1 := ifelse(cattle_only >= 1 & cattle_only <= 4, 1, ifelse(is.na(cattle_only), 9, 0))]
    dt[, cattle_only5 := ifelse(cattle_only >= 5 & cattle_only <= 9, 1, ifelse(is.na(cattle_only), 9, 0))]
    dt[, cattle_only10 := ifelse(cattle_only >= 10, 1, ifelse(is.na(cattle_only), 9, 0))]
  }
  
  if ("cows_cattle" %in% names(dt)) {
    dt[, cows_cattle0 := ifelse(cows_cattle == 0, 1, ifelse(is.na(cows_cattle), 9, 0))]
    dt[, cows_cattle1 := ifelse(cows_cattle >= 1 & cows_cattle <= 4, 1, ifelse(is.na(cows_cattle), 9, 0))]
    dt[, cows_cattle5 := ifelse(cows_cattle >= 5 & cows_cattle <= 9, 1, ifelse(is.na(cows_cattle), 9, 0))]
    dt[, cows_cattle10 := ifelse(cows_cattle >= 10, 1, ifelse(is.na(cows_cattle), 9, 0))]
  }
  
  dt[, horse_donk0 := ifelse(horse_donk == 0, 1, ifelse(is.na(horse_donk), 9, 0))]
  dt[, horse_donk1 := ifelse(horse_donk >= 1 & horse_donk <= 4, 1, ifelse(is.na(horse_donk), 9, 0))]
  dt[, horse_donk5 := ifelse(horse_donk >= 5 & horse_donk <= 9, 1, ifelse(is.na(horse_donk), 9, 0))]
  dt[, horse_donk10 := ifelse(horse_donk >= 10, 1, ifelse(is.na(horse_donk), 9, 0))]
  
  dt[, goat0 := ifelse(goat == 0, 1, ifelse(is.na(goat), 9, 0))]
  dt[, goat1 := ifelse(goat >= 1 & goat <= 9, 1, ifelse(is.na(goat), 9, 0))]
  dt[, goat10 := ifelse(goat >= 10 & goat <= 29, 1, ifelse(is.na(goat), 9, 0))]
  dt[, goat30 := ifelse(goat >= 30, 1, ifelse(is.na(goat), 9, 0))]
  
  dt[, sheep0 := ifelse(sheep == 0, 1, ifelse(is.na(sheep), 9, 0))]
  dt[, sheep1 := ifelse(sheep >= 1 & sheep <= 9, 1, ifelse(is.na(sheep), 9, 0))]
  dt[, sheep10 := ifelse(sheep >= 10 & sheep <= 29, 1, ifelse(is.na(sheep), 9, 0))]
  dt[, sheep30 := ifelse(sheep >= 30, 1, ifelse(is.na(sheep), 9, 0))]
  
  dt[, chicken0 := ifelse(chickens == 0, 1, ifelse(is.na(chickens), 9, 0))]
  dt[, chicken1 := ifelse(chickens >= 1 & chickens <= 9, 1, ifelse(is.na(chickens), 9, 0))]
  dt[, chicken10 := ifelse(chickens >= 10 & chickens <= 29, 1, ifelse(is.na(chickens), 9, 0))]
  dt[, chicken30 := ifelse(chickens >= 30, 1, ifelse(is.na(chickens), 9, 0))]
  
  dt[, pig0 := ifelse(pigs == 0, 1, ifelse(is.na(pigs), 9, 0))]
  dt[, pig1 := ifelse(pigs >= 1 & pigs <= 4, 1, ifelse(is.na(pigs), 9, 0))]
  dt[, pig5 := ifelse(pigs >= 5 & pigs <= 9, 1, ifelse(is.na(pigs), 9, 0))]
  dt[, pig10 := ifelse(pigs >= 10, 1, ifelse(is.na(pigs), 9, 0))]
  
  
  ## Bank Account ----------------------------------------
  message("||---Bank account")
  
  # Bank account
  # MISSING FROM CMR 2006, GHA 2006, MWI 2006
  if (!grepl("MICS3", survey)) {
    dt[, bank_acc := ifelse(grepl("MICS(4|5)", survey), hc15, hc19)]     # household mem owns bank account
    dt[, bank_acc := ifelse(bank_acc == 1, 1, ifelse(bank_acc == 2, 0, NA))]
  }
   
  
  ## Water Supply ----------------------------------------
  message("||---Water supply")
  
  # Drinking water supply
  dt[, drink_water_supp := as.character(as_factor(ws1))]
  dt[, drink_water_supp := tolower(drink_water_supp)]
  
  dt[, h2oires := ifelse(grepl("piped into dwelling|robinet dans le logement", drink_water_supp), 1, 0)]                      # piped into dwelling 
  dt[, h2oyrd := ifelse(grepl("piped .*yard|dans la .*cour", drink_water_supp), 1, 0)]                                        # piped into yard/plot
  dt[, h2opub := ifelse(grepl("public tap|piped to neigh|kiosk|robinet public|robinet .*voisin", drink_water_supp), 1, 0)]    # public tap/standpipe, piped into neighbors, water kiosk
  dt[, h2obwell := ifelse(grepl("borehole|tubewell|tube well|puits a pompe|forage", drink_water_supp), 1, 0)]                 # tube well or borehole
  dt[, h2opwell := ifelse(grepl("^protected well|puits prot| protected well", drink_water_supp), 1, 0)]                       # protected dug well
  dt[, h2oowell := ifelse(grepl("unprotected well|puits non prot", drink_water_supp), 1, 0)]                                  # unprotected dug well
  dt[, h2ospg := ifelse(grepl("spring", drink_water_supp), 1, 0)]                                                             # spring
  dt[, h2opspg := ifelse(grepl("protected spring|source prot", drink_water_supp), 1, 0)]                                      # protected spring
  dt[, h2ouspg := ifelse(grepl("unprotected spring|source non prot", drink_water_supp), 1, 0)]                                # unprotected spring
  dt[, h2orain := ifelse(grepl("rainwater|eau de pluie", drink_water_supp), 1, 0)]                                            # water from rain
  dt[, h2otruck := ifelse(grepl("tanker-truck|camion citerne|camion-citerne", drink_water_supp), 1, 0)]                       # water from tanker truck
  dt[, h2ocart := ifelse(grepl("cart with small tank|charrette avec petite citerne", drink_water_supp), 1, 0)]                # water from cart with small tank
  dt[, h2osurf := ifelse(grepl("river|lake|surface", drink_water_supp), 1, 0)]                                                # surface water - river, lake, dam, etc.
  dt[, h2obot := ifelse(grepl("sachet|bottle|packaged|eau minerale|bouteille", drink_water_supp), 1, 0)]                      # water from bottle, satchel water
  dt[, h2ooth := ifelse(grepl("other|autre", drink_water_supp), 1, 0)]                                                        # other water source

  # return responses that did not get coded into any 1's
  h2o_cols <- names(dt)[grepl("^h2o", names(dt))]
  message(paste("Unproccessed h2o responses:", paste(unique(dt[rowSums(dt[,c(h2o_cols),with=F])==0]$drink_water_supp), collapse = ", ")))
  
  # if missing, set all floor columns to NA
  dt[grepl("missing|manquant|no response", drink_water_supp) | is.na(drink_water_supp), (h2o_cols) := NA]
  
  
  # Improved water supply
  dt[!is.na(h2oires), h2o_home := 0]
  dt[h2oires == 1, h2o_home := 1]
  
  dt[!is.na(h2oires), h2o_improve := 0]
  dt[h2oires == 1 | h2oyrd == 1 | h2opub == 1 | h2obwell == 1 | h2opwell == 1 | h2opspg == 1 | 
       h2orain == 1 | h2otruck == 1 | h2ocart == 1 | h2obot == 1, h2o_improve := 1]
  
  
  
  ## Toilet Facilities ----------------------------------------
  message("||---Toilet facilities")
  
  # type of toilet facility
  dt[, toilet_type := ifelse(grepl("MICS3", survey), as.character(as_factor(ws7)), 
                             ifelse(grepl("MICS(4|5)", survey), as.character(as_factor(ws8)), as.character(as_factor(ws11))))]
  dt[, toilet_type := tolower(toilet_type)]                     # lowercase
  dt[, toilet_type :=  gsub('[[:punct:]]+', '', toilet_type)]   # remove punctuation

  # Toilet facilities
  dt[, flushs := ifelse(grepl("piped sewer system|flus to piped|branchee a egout|système dégouts", toilet_type), 1, 0)]                     # flush to piped sewer system
  dt[, flusht := ifelse(grepl("septic tank|flush to sept|fosse septique", toilet_type), 1, 0)]                                              # flush to septic tank
  dt[, flushp := ifelse(grepl("flush to pit|branchee aux latrines|reliée à des latrines", toilet_type), 1, 0)]                              # flush to pit latrine
  dt[, flushe := ifelse(grepl("flush to somewhere else|flush to open drain|branchee a autre|reliée à autre", toilet_type), 1, 0)]           # flush to somewhere else
  dt[, flushdk := ifelse(grepl("flush to unknown|flush dont know|flush to dk|endroit inconnupas|flush  dont", toilet_type), 1, 0)]          # flush, don't know where, flush toilet
  dt[, latvip := ifelse(grepl("ventilated improved|ameliorees auto aerees|\\blav\\b", toilet_type), 1, 0)]                                  # ventilated improved pit latrine (vip)
  dt[, latpits := ifelse(grepl("pit latrine (with|w) slab|pit latrine with seat|latrines couvertes|fosses avec dalle", toilet_type), 1, 0)] # pit latrine with slab, improve pit toilet latrine
  dt[, latpit := ifelse(grepl("without slab|latrines a fosses|fosse sans dalle", toilet_type), 1, 0)]                                       # pit latrine without slab/open pit, traditional pit toilet
  dt[, latpail := ifelse(grepl("bucket|seaux", toilet_type), 1, 0)]                                                                         # bucket toilet, pan
  dt[, lathang := ifelse(grepl("hanging|latrines suspendues", toilet_type), 1, 0)]                                                          # hanging toilet/latrine
  dt[, latbush := ifelse(grepl("bush|field|brousse|nature", toilet_type), 1, 0)]                                                            # no facility, bush, field
  dt[, latcompost := ifelse(grepl("composting|compostage", toilet_type), 1, 0)]                                                             # composting toilet
  dt[, latoth := ifelse(grepl("other|autre|mobile", toilet_type), 1, 0)]                                                                    # other
  
  # return responses that did not get coded into any 1's
  toilet_cols <- c("flushs","flusht","flushp","flushe","flushdk","latvip","latpits",
                   "latpit","latpail","lathang","latbush","latcompost","latoth")
  cols <- names(dt)[names(dt) %in% toilet_cols]
  message(paste("Unproccessed toilet responses:", paste(unique(dt[rowSums(dt[,c(cols),with=F])==0]$toilet_type), collapse = ", ")))
  
  # if missing, set all toilet columns to NA
  dt[grepl("missing|manquant|no response", toilet_type) | is.na(toilet_type), (cols) := NA]
  
  # Improved toilet facilities
  # any toilet that flushes, ventilated improved pit, composting toilet, pit with slab
  dt[!is.na(flushs), toilet_improve := 0]
  dt[flushs == 1 | flusht == 1 | flushp == 1 | flushe == 1 | flushdk == 1 | latvip == 1 | latpits == 1 | latcompost == 1, toilet_improve := 1]
  
  
  # Toilet shared
  dt[, shared_toilet := ifelse(grepl("MICS3", survey), ws8, 
                               ifelse(grepl("MICS(4|5)", survey), ws9, ws15))]
  
  # some toilet type responses are not asked about shared toilet
  # if answered toilet type, but not shared, assume "no" or 2
  dt[!is.na(toilet_improve) & is.na(shared_toilet), shared_toilet := 2]
  
  # combined toilet type and shared variables
  dt[, latshare := ifelse(shared_toilet == 1, 1, ifelse(shared_toilet == 2, 0, NA))]
  dt[, sflushs := ifelse(flushs == 1 & shared_toilet == 1, 1, 0)]
  dt[, sflusht := ifelse(flusht == 1 & shared_toilet == 1, 1, 0)]
  dt[, sflushp := ifelse(flushp == 1 & shared_toilet == 1, 1, 0)]
  dt[, sflushe := ifelse(flushe == 1 & shared_toilet == 1, 1, 0)]
  dt[, slatvip := ifelse(latvip == 1 & shared_toilet == 1, 1, 0)]
  dt[, slatpits := ifelse(latpits == 1 & shared_toilet == 1, 1, 0)]
  dt[, slatpit := ifelse(latpit == 1 & shared_toilet == 1, 1, 0)]
  dt[, slathang := ifelse(lathang == 1 & shared_toilet == 1, 1, 0)]
  dt[, slatoth := ifelse(latoth == 1 & shared_toilet == 1, 1, 0)]
  
  
  
  ## Floor/Wall/Roof Types ----------------------------------------
  message("||---Floor/wall/roof types")
  
  # Type of floor
  dt[, floor_type := ifelse(grepl("MICS[3-5]", survey), as.character(as_factor(hc3)), as.character(as_factor(hc4)))]
  dt[, floor_type := tolower(floor_type)]                     # lowercase
  dt[, floor_type :=  gsub('[[:punct:]]+', '', floor_type)]   # remove punctuation
  
  dt[, dirtfloo := ifelse(grepl("earth|mud|dung|terre|bouse", floor_type), 1, 0)]                               # earth/sand, dung, mud bricks
  dt[, woodfloo := ifelse(grepl("wood|bamboo|planche .*bois", floor_type), 1, 0)]                               # wood planks
  dt[, prqfloo := ifelse(grepl("parquet", floor_type), 1, 0)]                                                   # parquet or polished wood
  dt[, vinlfloo := ifelse(grepl("vinyl|linoleum|asphalt", floor_type), 1, 0)]                                   # vinyl or asphalt strips
  dt[, tilefloo := ifelse(grepl("terrazzo|marble tiles|ceramic tiles|carreaux|carrelage", floor_type), 1, 0)]   # ceramic tiles, terrazzo
  dt[, cemtfloo := ifelse(grepl("c(e|i)ment", floor_type), 1, 0)]                                               # cement
  dt[, stonfloo := ifelse(grepl("stone|burnt brick", floor_type), 1, 0)]                                        # stone, bricks
  dt[, rugfloo := ifelse(grepl("carpet|taat|moquette", floor_type), 1, 0)]                                      # carpet
  dt[, othfloo := ifelse(grepl("other|autre", floor_type), 1, 0)]                                               # other
  
  # return responses that did not get coded into any 1's
  floor_cols <- names(dt)[grepl("floo$", names(dt))]
  message(paste("Unproccessed floor responses:", paste(unique(dt[rowSums(dt[,c(floor_cols),with=F])==0]$floor_type), collapse = ", ")))
  
  # if missing, set all floor columns to NA
  dt[grepl("missing|manquant|no response", floor_type) | is.na(floor_type), (floor_cols) := NA]
  
  
  # Improved floor
  # dirt/mud floors, wood, other
  dt[!is.na(dirtfloo), floor_improve := 1]
  dt[dirtfloo == 1 | othfloo == 1, floor_improve := 0]
  
  
  
  # Type of wall
  dt[, wall_type := ifelse(grepl("MICS[3-5]", survey), as.character(as_factor(hc5)), as.character(as_factor(hc6)))]
  dt[, wall_type := tolower(wall_type)]                     # lowercase
  dt[, wall_type :=  gsub('[[:punct:]]+', '', wall_type)]   # remove punctuation
  
  dt[, nowall := ifelse(grepl("no wall|pas de murs", wall_type), 1, 0)]                                                         # no walls
  dt[, natwall := ifelse(grepl("palm leaves|mud|cane|dirt|thatch|bamboo|chaume|canne|mottes de terre", wall_type), 1, 0)]       # cane/palm/trunks, dirt/earth/mud
  dt[, mudwall := ifelse(grepl("mud brick|with mud|terre battue|brique simple|avec boue", wall_type), 1, 0)]                    # mud bricks, bamboo with mud
  dt[, stonwall := ifelse(grepl("stone", wall_type), 1, 0)]                                                                     # stone with mud
  dt[, adobwall := ifelse(grepl("uncovered adobe|adobe non recouvert", wall_type), 1, 0)]                                       # uncovered adobe
  dt[, plywall := ifelse(grepl("plywood|contre plaq", wall_type), 1, 0)]                                                        # plywood
  dt[, plasticwall := ifelse(grepl("plastic covered", wall_type), 1, 0)]                                                        # plastic
  dt[, cardwall := ifelse(grepl("cardboard|carton", wall_type), 1, 0)]                                                          # cardboard
  dt[, rwoodwall := ifelse(grepl("reused wood|bois de r|\\bwood\\b", wall_type), 1, 0)]                                         # reused wood
  dt[, ubrkwall := ifelse(grepl("unburnt brick|", wall_type), 1, 0)]                                                            # unburnt bricks
  
  dt[, cmtwall := ifelse(grepl("c(e|i)ment|landcrete|stone with lime|pierre de taille|pierre avec chaux", wall_type), 1, 0)]    # cement, stone with lime, cut stone
  dt[, bbrkwall := ifelse(grepl("burnt brick|^bricks$|bricks burnt|terre cuite|^briques$", wall_type), 1, 0)]                   # burnt bricks
  dt[, cmtbwall := ifelse(grepl("cement blo|parpaing|blocs de ciment", wall_type), 1, 0)]                                       # cement blocks
  dt[, cadobwall := ifelse(grepl("covered adobe|adobe recouvert", wall_type), 1, 0)]                                            # covered adobe
  dt[, woodwall := ifelse(grepl("wood planks|planche", wall_type), 1, 0)]                                                       # wood planks/shingles
  dt[, slatewall := ifelse(grepl("asbestos|marbre", wall_type), 1, 0)]                                                          # slate/asbestos, marble
  dt[, othwall := ifelse(grepl("other|autre", wall_type), 1, 0)]                                                                # other
  
  # return responses that did not get coded into any 1's
  wall_cols <- names(dt)[grepl("wall$", names(dt))]
  message(paste("Unproccessed wall responses:", paste(unique(dt[rowSums(dt[,c(wall_cols),with=F])==0]$wall_type), collapse = ", ")))
  
  # if missing, set all wall columns to NA
  dt[grepl("missing|manquant|no response", wall_type) | is.na(wall_type), (wall_cols) := NA]
  
  
  # Improved walls
  dt[!is.na(natwall), wall_improve := 1]
  dt[nowall == 1 | natwall == 1 | mudwall == 1 | stonwall == 1 | adobwall == 1 | plasticwall==1 | 
       plywall == 1 | cardwall == 1 | rwoodwall == 1 | ubrkwall == 1 | othwall == 1, wall_improve := 0]
  
  
  
  # Type of roof
  dt[, roof_type := ifelse(grepl("MICS[3-5]", survey), as.character(as_factor(hc4)), as.character(as_factor(hc5)))]
  dt[, roof_type := tolower(roof_type)]                     # lowercase
  dt[, roof_type :=  gsub('[[:punct:]]+', '', roof_type)]   # remove punctuation
  
  dt[, noroof := ifelse(grepl("no roof|pas de toit", roof_type), 1, 0)]                   # no roof
  dt[, natroof := ifelse(grepl("thatch|sod|chaume|herbe", roof_type), 1, 0)]              # thatch/palm leaf, sod
  dt[, mudroof := ifelse(grepl("mud brick", roof_type), 1, 0)]                            # mud
  dt[, matroof := ifelse(grepl("rustic mat|natte", roof_type), 1, 0)]                     # rustic mat
  dt[, bambroof := ifelse(grepl("bamboo|bambou", roof_type), 1, 0)]                       # palm/bamboo
  dt[, wproof := ifelse(grepl("wood plank|planche", roof_type), 1, 0)]                    # wood planks
  dt[, cardroof := ifelse(grepl("cardboard", roof_type), 1, 0)]                           # cardboard/polythene sheet
  
  dt[, metalroof := ifelse(grepl("metal|tôle", roof_type), 1, 0)]                         # metal/tin/iron sheet
  dt[, woodroof := ifelse(grepl("^wood$|parquet|^bois$", roof_type), 1, 0)]               # wood
  dt[, asbroof:= ifelse(grepl("asbestos", roof_type), 1, 0)]                              # slate/asbestos
  dt[, tileroof := ifelse(grepl("roofing tile|ceramic tile|tuiles", roof_type), 1, 0)]    # ceramic tiles
  dt[, cmtroof := ifelse(grepl("c(e|i)ment", roof_type), 1, 0)]                           # cement
  dt[, shngroof := ifelse(grepl("shingles", roof_type), 1, 0)]                            # roofing shingles
  dt[, othroof := ifelse(grepl("other|autre", roof_type), 1, 0)]                          # other
  
  # return responses that did not get coded into any 1's
  roof_cols <- names(dt)[grepl("roof$", names(dt))]
  message(paste("Unproccessed roof responses:", paste(unique(dt[rowSums(dt[,c(roof_cols),with=F])==0]$roof_type), collapse = ", ")))
  
  # if missing, set all roof columns to NA
  dt[grepl("missing|manquant|no response", roof_type) | is.na(roof_type), (roof_cols) := NA]
  
  
  # Improved roofing
  dt[!is.na(natroof), roof_improve := 1]
  dt[noroof == 1 | natroof == 1 | mudroof == 1 | matroof == 1 | bambroof == 1 | wproof == 1 | cardroof == 1 | othroof == 1, roof_improve := 0]
  
  
  
  ## Cooking Fuel/Energy ----------------------------------------
  message("||---Cooking fuel/energy")
  
  # Type of cooking fuel/energy
  # for MICS must use combined responses from cookstove type and cookstove fuel/energy
  if (grepl("MICS6", survey)) {
    dt[, cook_type := as.character(as_factor(eu1))]
    dt[, cook_type := tolower(cook_type)]                     # lowercase
    dt[, cook_type :=  gsub('[[:punct:]]+', '', cook_type)]   # remove punctuation
  }
  dt[, cook_fuel := ifelse(grepl("MICS[3-5]", survey), as.character(as_factor(hc6)), as.character(as_factor(eu4)))]
  dt[, cook_fuel := tolower(cook_fuel)]                     # lowercase
  dt[, cook_fuel :=  gsub('[[:punct:]]+', '', cook_fuel)]   # remove punctuation
  
  dt[, cookelec := ifelse(grepl("electricit", cook_fuel), 1, 0)]                                    # electricity
  dt[, cooklpg := ifelse(grepl("lpg|gpl", cook_fuel), 1, 0)]                                        # liquefied petroleum gas (lpg)
  dt[, cookgas := ifelse(grepl("natural gas|diesel|alcohol|gaz natur", cook_fuel), 1, 0)]           # cooking gas, gasoline/diesel, alcohol/ethanol, piped natural gas
  dt[, cookbio := ifelse(grepl("biogas|biogaz", cook_fuel), 1, 0)]                                  # biogas
  dt[, cookkero := ifelse(grepl("kerosene|trole lampant", cook_fuel), 1, 0)]                        # kerosene
  dt[, cookcoal := ifelse(grepl("coal|charbon", cook_fuel), 1, 0)]                                  # coal, lignite
  dt[, cookchar := ifelse(grepl("charcoal|charbon de bois", cook_fuel), 1, 0)]                      # charcoal
  dt[, cookwood := ifelse(grepl("wood|sawdust|\\bbois\\b|scuire", cook_fuel), 1, 0)]                # wood, woodchips, sawdust
  dt[, cookstraw := ifelse(grepl("straw|paille", cook_fuel), 1, 0)]                                 # straw/grass/shrubs
  dt[, cookcrop := ifelse(grepl("crop residue|residus|sidus agricoles", cook_fuel), 1, 0)]          # agricultural crop   
  dt[, cookdung := ifelse(grepl("dung|animal waste|bouse", cook_fuel), 1, 0)]                       # dung, animal dung/waste
  dt[, cooknone := ifelse(grepl("none|no food cooked|non concern|pas de repas", cook_fuel), 1, 0)]  # no food cooked in house
  dt[, cookoth := ifelse(grepl("other|garbage|autre", cook_fuel), 1, 0)]                            # other, garbage/trash
  if (grepl("MICS6", survey)) {
    # MICS6 filters out these responses to cook_type, they do not answer cook_fuel
    dt[, cookelec := ifelse(grepl("electric", cook_type), 1, cookelec)]                  # electricity
    dt[, cooksolar := ifelse(grepl("solar", cook_type), 1, 0)]                           # solar cooker
    dt[, cookgas := ifelse(grepl("natural gas|diesel|alcohol", cook_type), 1, cookgas)]  # piped natural gas
    dt[, cooklpg := ifelse(grepl("lpg", cook_type), 1, cooklpg)]                         # liquefied petroleum gas (lpg)
    dt[, cookbio := ifelse(grepl("biogas", cook_type), 1, cookbio)]                      # biogas
    dt[, cooknone := ifelse(grepl("none|no food cooked", cook_type), 1, cooknone)]       # no food cooked in house
  }
  
  # return responses that did not get coded into any 1's
  cook_cols <- names(dt)[grepl("cook", names(dt)) & !grepl("_", names(dt))]
  message(paste("Unproccessed cooking responses:", paste(unique(dt[rowSums(dt[,c(cook_cols),with=F])==0]$cook_fuel), collapse = ", ")))
  
  # if missing, set all cook columns to NA
  dt[grepl("missing|manquant|no response", cook_fuel), (cook_cols) := NA]
  if (grepl("MICS6", survey)) dt[is.na(cook_type), (cook_cols) := NA]
  if (!grepl("MICS6", survey)) dt[is.na(cook_fuel), (cook_cols) := NA]
  
  # Modern fuel
  dt[!is.na(cookelec), mod_fuel := 1]
  dt[cookkero == 1 | cookcoal == 1 | cookchar == 1 | cookwood == 1 | cookstraw == 1 | cookcrop == 1 | cookdung == 1 | cookoth == 1 | cooknone == 1, mod_fuel := 0]
  
  
  
  ## Land Ownership and Area ----------------------------------------
  message("||---Land ownership/area")
  
  # owns any land for agricultural use
  dt[, any_land := ifelse(grepl("MICS[3-5]", survey), hc11, hc15)]
  dt[, any_land := ifelse(any_land == 1, 1, ifelse(any_land == 2, 0, NA))]

  # need to convert land area to hectares to match DHS
  
  # Nepal
  if (grepl("NPL", survey)) {
    
    # NPL 2010, ropani and bigha asked directly in survey, prioritizing those units first
    # https://en.wikipedia.org/wiki/Nepalese_customary_units_of_measurement 
    if (grepl("MICS4", survey)) {
      dt[, landarea := ifelse(hc12rr != 0, hc12rr * (508.737/10000),                                                   # ropani (1 = 508.757 sq m)
                              ifelse(hc12bb != 0, hc12bb * (6772.632/10000),                                           # bigha (1 = 6772.632 sq m)
                                     ifelse(hc12ra != 0, hc12ra * (31.796/10000),                                      # anna (1 = 31.796 sq m)
                                            ifelse(hc12rp %ni% c(0,8), hc12rp * (7.949/10000),                         # paisa (1 = 7.949 sq m), 8 is coded as dk
                                                   ifelse(hc12bk != 0, hc12bk * (338.632/10000),                       # katha (1 = 338.632 sq m)
                                                          ifelse(hc12bd != 0, hc12bd * (16.932/10000), hc12rr))))))]   # dhur (1 = 16.932 sq m)
    }
    
    # NPL 2014 already has a hectare conversion
    if (grepl("MICS5", survey)) dt[, landarea := hc12a] 
    
    # NPL 2019 already in hectares
    if (grepl("MICS6", survey)) dt[, landarea := hc16]
  }
  
  # Ghana
  if (grepl("GHA", survey)) {
    
    # pole and rope conversions found in GHA Living Standards 1998-99 User Guide
    
    # GHA 2006
    if (grepl("MICS3", survey)) {
      dt[, landarea := ifelse(hc12u == 1, hc12n,                                            # hectares
                              ifelse(hc12u == 2, hc12n/2.471,                               # acres to hectares
                                     ifelse(hc12u == 3, hc12n/15,                           # plots to hectares
                                            ifelse(hc12u == 9, NA,                          # dk unit
                                                   ifelse(hc12n %in% c(98,99), NA, NA)))))] # dk or missing amount
    }
    
    # GHA 2011
    if (grepl("MICS4", survey)) {
      dt[, landarea := ifelse(hc12u == 1, hc12n,                                                    # hectares
                              ifelse(hc12u == 3, hc12n/2.471,                                       # acres to hectares
                                     ifelse(hc12u == 4, (hc12n/4)/2.471,                            # plots to hectares
                                            ifelse(hc12u == 2, hc12n/2.471,                         # poles to hectares
                                                   ifelse(hc12u == 5, (hc12n/9)/2.471,              # ropes to hectares
                                                          ifelse(hc12u == 9, NA,                    # missing unit
                                                                 ifelse(hc12n == 98, NA, NA)))))))] # dk or missing amount
    }
    
    # GHA 2017-18
    if (grepl("MICS6", survey)) {
      dt[, landarea := ifelse(hc16u == 1, hc16,                                              # hectares
                              ifelse(hc16u == 2, hc16/2.471,                                 # acres to hectares
                                     ifelse(hc16u == 3, hc16/2.471,                          # poles to hectares
                                            ifelse(hc16u == 4, (hc16/4)/2.471,               # plots to hectares
                                                   ifelse(hc16u == 8, NA,                    # dk unit
                                                          ifelse(hc16 == 98, NA, NA))))))]   # dk amount
    }
  }
  
  # Malawi
  if (grepl("MWI", survey)) {
    
    # MWI 2006
    if (grepl("MICS3", survey)) {
      dt[, landarea := ifelse(hc12_unit == 1, hc12/2.471,                        # acres to hectares
                              ifelse(hc12_unit == 2, hc12,                       # hectacres
                                     ifelse(hc12 %in% c(98,99), NA, NA)))]       # dk/missing amount
    }
    
    # MWI 2013-14
    if (grepl("MICS5", survey)) {
      dt[, landarea := ifelse(hc12a == 1, hc12b/2.471,                                       # acres to hectares
                              ifelse(hc12a == 2, hc12b,                                      # hectares
                                     ifelse(hc12a == 3, hc12b*.714,                          # football pitches to hectares
                                            ifelse(hc12a == 9, NA,                           # special unit, cannot process
                                                   ifelse(hc12b %in% c(98,99), NA, NA)))))]  # dk/missing amount
    }
    
    # MWI 2019-2020
    if (grepl("MICS6", survey)) {
      dt[, landarea := ifelse(hc16 == 1, hc16a/2.471,                                       # acres to hectares
                              ifelse(hc16 == 2, hc16a,                                      # hectares
                                     ifelse(hc16 == 3, hc16a*.714,                          # football pitches to hectares
                                            ifelse(hc16 == 98, NA,                          # dk unit
                                                   ifelse(hc16a == 98, NA, NA)))))]         # dk amount
    }
  }
  
  # Cameroon
  if (grepl("CMR", survey)) {
    
    # CMR 2006, already in hectares
    if (grepl("MICS3", survey)) {
      dt[, landarea := ifelse(hc12 <= 97, hc12, NA)]
    }
    
    # CMR 2014, already in hectares
    if (grepl("MICS5", survey)) {
      dt[, landarea := ifelse(hc12 <= 97, hc12, NA)]
    }
  }
  
  # if own land but landarea is 0, set landarea to 0.005
  dt[any_land == 1 & landarea == 0, landarea := 0.005]
  
  # if no land ownership, set landarea to 0
  dt[any_land == 0, landarea := 0]
  
  
  
  # EXPORT CLEANED DATA --------------------------------------------
  message("||---Export cleaned data")
  
  # extract original wealth quintiles and scores for comparisons
  dt[, windex5 := ifelse(grepl("MICS3", survey), wlthind5, windex5)]
  dt[, wscore := ifelse(grepl("MICS3", survey), wlthscor, wscore)]
  
  # filter to variables of interest
  vars_interest <- c("survey","cluster","hh_id","area_unit","country","region","strata","hhweight","hhid_unique",
                     "n_rooms_sleeping","n_hh_members","memsleep","electricity",asset_cols,livestock_cols,"bank_acc",
                     "drink_water_supp",h2o_cols,"h2o_improve","toilet_type",toilet_cols,"toilet_improve",
                     "shared_toilet","latshare","sflushs","sflusht","sflushp","sflushe","slatvip","slatpits",
                     "slatpit","slathang","slatoth","floor_type",floor_cols,"floor_improve","wall_type",wall_cols,
                     "wall_improve","roof_type",roof_cols,"roof_improve","cook_type","cook_fuel",cook_cols,"mod_fuel",
                     "any_land","landarea","windex5","wscore")
  cur_vars <- names(dt)[names(dt) %in% vars_interest]
  dt_filter <- dt[, ..cur_vars]
  
  # save the cleaned data
  file_name <- gsub("\\.DTA|\\.SAV", "", basename(survey))
  output_file_path <- file.path(out.dir, paste0(file_name, "_wealth_quintile_prep.csv"))
  write.csv(dt_filter, output_file_path, row.names = FALSE)
  
  message(paste0("Successfully saved ", file_name))
}


# RUN EXTRACTIONS ---------------------------------------------------

# Cameroon
extract_data("/FILEPATH/CMR_MICS3_2006_HH.DTA", "cm")
extract_data("/FILEPATH/CMR_MICS5_2014_HH.DTA", "cm")

# Nepal
extract_data("/FILEPATH/NPL_MICS4_2010_HH.DTA", "np")
extract_data("/FILEPATH/NPL_MICS5_2014_HH.DTA", "np")
extract_data("/FILEPATH/NPL_MICS6_2019_HH.SAV", "np")

# Malawi
extract_data("/FILEPATH/MWI_MICS3_2006_HH.DTA", "mw")
extract_data("/FILEPATH/MWI_MICS5_2013_2014_HH.DTA", "mw")
extract_data("/FILEPATH/MWI_MICS6_2019_2020_HH.DTA", "mw")

# Ghana
extract_data("/FILEPATH/GHA_MICS3_2006_HH.DTA", "gh")
extract_data("/FILEPATH/GHA_MICS4_2011_HH.DTA", "gh")
extract_data("/FILEPATH/GHA_MICS6_2017_2018_HH.DTA", "gh")
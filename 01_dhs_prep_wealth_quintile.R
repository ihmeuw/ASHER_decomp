#-------------------Header------------------------------------------------
# Project: IHME ASHER Decomposition
# Purpose: Extract components of the wealth index from DHS surveys
# Date: 10/20/2023
# Notes:
#***************************************************************************

# Sourced from 2016 time stamp page: https://dhsprogram.com/topics/wealth-index/Wealth-Index-Construction.cfm
# Rustein article with SPSS code: https://dhsprogram.com/programming/wealth%20index/Steps_to_constructing_the_new_DHS_Wealth_Index.pdf

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

# create function to convert CMC date to year and month
get_cmc_year <- function(x) 1900 + as.integer((as.integer(x) - 1) / 12)
get_cmc_month <- function(x) as.integer(x) - 12 * (get_cmc_year(x) - 1900)


# EXTRACT DATA FUNCTION -----------------------------------------------

extract_data <- function(survey, cur_country) {
  
  # read in data 
  dt <- data.table(read_dta(survey))
  
  
  ## Survey Characteristics ----------------------------------
  message("||---Survey characteristics")
  
  # convert variable names to lowercase
  names(dt) <- tolower(names(dt))
  
  # extract survey name and add to dt
  survey_name <- str_extract(survey, "[A-Z]{3}_DHS[0-9]_[0-9]{4}(_[0-9]{4})*")
  dt[, survey := survey_name]
  
  # identifiers
  dt[, cluster := hv001]
  dt[, hh_id := hv002]
  dt[, area_unit := hv004]
  dt[, country := cur_country]
  dt[, region := as.character(as_factor(hv024))]
  dt[, strata := hv022]
  
  # household's sample weight
  dt[, hhweight := hv005]
  
  # unique hh id for each household
  dt[, hhid_unique := paste(hh_id, cluster)]
  
  
  ## Household Assets ----------------------------------------
  message("||---Household assets")
  
  # Number of members per sleeping room
  # MISSING FROM GHA 2003, MWI 2000, NPL 2001, RWA 2000 and 2005
  if (!grepl("GHA_DHS4|MWI_DHS4_2000|NPL_DHS4|RWA_DHS4|CMR_DHS4", survey)) {
    if (grepl("MWI_DHS4_2004", survey)) {
      dt[, n_rooms_sleeping := as.numeric(sh29a)]
    } else {
      dt[, n_rooms_sleeping := as.numeric(hv216)]
    }
    dt[, n_hh_members := ifelse(hv012 > 0, hv012, ifelse(hv013 > 0, hv013, NA))]
    
    dt[, memsleep := n_hh_members / n_rooms_sleeping]
    dt[, memsleep := ifelse(memsleep  >= 95, NA, memsleep)]
  }
  
  # Electricity
  dt[, electricity := ifelse(hv206 == 1, 1, 0)]
  
  ## Assets
  dt[, radio := ifelse(hv207 == 1, 1, ifelse(hv207 == 0, 0, NA))]
  dt[, tv := ifelse(hv208 == 1, 1, ifelse(hv208 == 0, 0, NA))]
  dt[, fridge := ifelse(hv209 == 1, 1, ifelse(hv209 == 0, 0, NA))]
  dt[, bike := ifelse(hv210 == 1, 1, ifelse(hv210 == 0, 0, NA))]
  dt[, moto := ifelse(hv211 == 1, 1, ifelse(hv211 == 0, 0, NA))]    # motorcycle or scooter
  dt[, car := ifelse(hv212 == 1, 1, ifelse(hv212 == 0, 0, NA))]
  dt[, land_ph := ifelse(hv221 == 1, 1, ifelse(hv221 == 0, 0, NA))]
  
  if ("hv243a" %in% names(dt)) dt[, mobile_ph := ifelse(hv243a == 1, 1, ifelse(hv243a == 0, 0, NA))]
  if ("hv243b" %in% names(dt)) dt[, watch := ifelse(hv243b == 1, 1, ifelse(hv243b == 0, 0, NA))]
  if ("hv243c" %in% names(dt)) dt[, animal_cart := ifelse(hv243c == 1, 1, ifelse(hv243c == 0, 0, NA))]
  if ("hv243d" %in% names(dt)) dt[, motor_boat := ifelse(hv243d == 1, 1, ifelse(hv243d == 0, 0, NA))]
  if ("sh110s" %in% names(dt)) dt[, internet := ifelse(sh110s == 1, 1, ifelse(sh110s == 0, 0, NA))] 
  if ("sh110l" %in% names(dt)) dt[, comp := ifelse(sh110l == 1, 1, ifelse(sh110l == 0, 0, NA))]    # or tablet
  if ("sh110k" %in% names(dt)) dt[, wash_mach := ifelse(sh110k == 1, 1, ifelse(sh110k == 0, 0, NA))]

  
  ## Livestock ----------------------------------------
  message("||---Livestock")
  
  # owns any livestock
  if ("hv246" %in% names(df)) dt[, any_livestock := ifelse(hv246 == 1, 1, ifelse(hv246 == 0, 0, NA))]
  
  # generate continuous counts
  # these are the standardized livestock variables, beyond 'f' can vary
  if ("hv246a" %in% names(dt)) dt[, cattle_only := as.integer(hv246a > 0)]
  if ("hv246b" %in% names(dt)) dt[, cows_only := as.integer(hv246b > 0)]
  if ("hv246c" %in% names(dt)) dt[, horse_donk := as.integer(hv246c > 0)]
  if ("hv246d" %in% names(dt)) dt[, goat := as.integer(hv246d > 0)]
  if ("hv246e" %in% names(dt)) dt[, sheep := as.integer(hv246e > 0)]
  if ("hv246f" %in% names(dt)) dt[, chickens := as.integer(hv246f > 0)]
  
  # replace "missing" with NA's
  livestock_cols <- c("cows_only","cattle_only","horse_donk","goat","sheep","chickens")
  cols <- names(dt)[names(dt) %in% livestock_cols]
  if (length(cols) > 0) dt[, (cols) := lapply(.SD, function(x) ifelse(x > 97, NA, x)), .SDcols = cols]
  
  # if answered "no" to any_livestock, set counts to 0
  if ("hv246" %in% names(df))  dt[, (cols) := lapply(.SD, function(x) ifelse(any_livestock == 0, 0, x)), .SDcols = cols]
  
  
  ## Bank Account ----------------------------------------
  message("||---Bank account")
  
  # Bank account
  if ("hv247" %in% names(dt)) dt[, bank_acc := ifelse(hv247 == 1, 1, ifelse(hv247 == 0, 0, NA))]
  
  
  ## Water Supply ----------------------------------------
  message("||---Water supply")
  
  # convert to character
  dt[, hv201 := as.character(as_factor(hv201))]
  dt[, hv201 := hv201 %>%
       gsub("[[:punct:]]+", "", .) %>%     # remove punctuation
       gsub("\\s+", " ", .) %>%            # remove extra spaces
       tolower]                            # lowercase
  
  # Drinking water supply
  dt[, h2oires := ifelse(grepl("piped into (dwel|comp)|robinet dans le logement|piped into res|in the house", hv201), 1, 0)]                    # piped into dwelling 
  dt[, h2oyrd := ifelse(grepl("piped .*yard|dans la .*cour|in the courtyard", hv201), 1, 0)]                                                    # piped into yard/plot
  dt[, h2opub := ifelse(grepl("public.* tap|piped to neigh|kiosk|along the road|community stand|dhara|of a neighbor", hv201), 1, 0)]            # public tap/standpipe, piped into neighbors, water kiosk
  dt[, h2obwell := ifelse(grepl("borehole|tubewell|tube well|puits a pompe|forage|well with pump", hv201), 1, 0)]                               # tube well or borehole
  dt[, h2opwell := ifelse(grepl("^protected .*well|puits prot| protected .*well|^protected without pump", hv201), 1, 0)]                        # protected dug well
  dt[, h2oowell := ifelse(grepl("unprotected well|puits non prot|open .*well|public.*well|well in (res|hou)|not protected well", hv201), 1, 0)] # unprotected dug well, unspecified wells
  dt[, h2ospg := ifelse(grepl("spring|kuwa", hv201), 1, 0)]                                                                                     # spring, kuwa = stagnant spring
  dt[, h2opspg := ifelse(grepl("protected spring|source prot|protected source", hv201), 1, 0)]                                                  # protected spring
  dt[, h2ouspg := ifelse(grepl("unprotected spring|source non prot", hv201), 1, 0)]                                                             # unprotected spring
  dt[, h2orain := ifelse(grepl("rainwater|eau de pluie", hv201), 1, 0)]                                                                         # water from rain
  dt[, h2otruck := ifelse(grepl("tanker|camion citerne|camion-citerne", hv201), 1, 0)]                                                          # water from tanker truck
  dt[, h2ocart := ifelse(grepl("cart with small tank|charrette avec petite citerne", hv201), 1, 0)]                                             # water from cart with small tank
  dt[, h2osurf := ifelse(grepl("river|lake|surface|dugout|\\bdam\\b", hv201), 1, 0)]                                                            # surface water - river, lake, dam, etc.
  dt[, h2obot := ifelse(grepl("sachet|bottle|packaged|eau minerale|bouteille|satchel", hv201), 1, 0)]                                           # water from bottle, satchel water
  dt[, h2ooth := ifelse(grepl("other|autre", hv201), 1, 0)]                                                                                     # other water source
  
  # return responses that did not get coded into any 1's
  h2o_cols <- names(dt)[grepl("^h2o", names(dt))]
  message(paste("Unproccessed h2o responses:", paste(unique(dt[rowSums(dt[,c(h2o_cols),with=F])==0]$hv201), collapse = ", ")))
  
  # if missing, set all toilet columns to NA
  dt[grepl("missing|manquant|no response|99", hv201), (h2o_cols) := NA]
  
  
  # Improved water supply
  dt[!is.na(h2oires), h2o_home := 0]
  dt[h2oires == 1, h2o_home := 1]
  
  dt[!is.na(h2oires), h2o_improve := 0]
  dt[h2oires == 1 | h2oyrd == 1 | h2opub == 1 | h2obwell == 1 | h2opwell == 1 | h2opspg == 1 | 
       h2orain == 1 | h2otruck == 1 | h2ocart == 1 | h2obot == 1, h2o_improve := 1]
  
  
  ## Toilet Facilities ----------------------------------------
  message("||---Toilet facilities")
  
  # Toilet Facility
  if ("hv205" %in% names(dt)){
    # convert to character
    dt[, hv205 := as.character(as_factor(hv205))]
    dt[, hv205 := hv205 %>%
         gsub("[[:punct:]]+", "", .) %>%     # remove punctuation
         gsub("\\s+", " ", .) %>%            # remove extra spaces
         tolower]                            # lowercase
  
    # type of toilet facility
    dt[, flushs := ifelse(grepl("piped sewer system|flus to piped|branchee a egout|système dégouts", hv205), 1, 0)]                         # flush to piped sewer system
    dt[, flusht := ifelse(grepl("septic tank|flush to sept|fosse septique", hv205), 1, 0)]                                                  # flush to septic tank
    dt[, flushp := ifelse(grepl("flush to pit|branchee aux latrines|reliée à des latrines", hv205), 1, 0)]                                  # flush to pit latrine
    dt[, flushe := ifelse(grepl("flush to somewhere|flush to open drain|branchee a autre|reliée à autre|flush bio", hv205), 1, 0)]          # flush to somewhere else
    dt[, flushdk := ifelse(grepl("flush .*(unk|dk|dont)|endroit inconnupas|own flush|shared flush|^flush toilet$", hv205), 1, 0)]           # flush, don't know where, flush toilet
    dt[, latvip := ifelse(grepl("ventilated improved|ameliorees auto aerees|\\blav\\b|improved pit|vip lat", hv205), 1, 0)]                 # ventilated improved pit latrine (vip)
    dt[, latpits := ifelse(grepl("pit latrine (with|w) slab|pit latrine with seat|latrines couvertes|fosses avec dalle", hv205), 1, 0)]     # pit latrine with slab, improve pit toilet latrine
    dt[, latpit := ifelse(grepl("without slab|latrines a fosses|fosse sans dalle|traditional pit|^latrine$|rudimentary pit", hv205), 1, 0)] # pit latrine without slab/open pit, traditional pit toilet
    dt[, latpail := ifelse(grepl("bucket|seaux", hv205), 1, 0)]                                                                             # bucket toilet, pan
    dt[, lathang := ifelse(grepl("hanging|latrines suspendues", hv205), 1, 0)]                                                              # hanging toilet/latrine
    dt[, latbush := ifelse(grepl("bush|field|brousse|nature", hv205), 1, 0)]                                                                # no facility, bush, field
    dt[, latcompost := ifelse(grepl("composting|compostage|biogas", hv205), 1, 0)]                                                          # composting toilet
    dt[, latoth := ifelse(grepl("other|autre|mobile", hv205), 1, 0)]    # other
    
    # return responses that did not get coded into any 1's
    toilet_cols <- c("flushs","flusht","flushp","flushe","flushdk","latvip","latpits",
                     "latpit","latpail","lathang","latbush","latcompost","latoth")
    cols <- names(dt)[names(dt) %in% toilet_cols]
    message(paste("Unproccessed toilet responses:", paste(unique(dt[rowSums(dt[,c(cols),with=F])==0]$hv205), collapse = ", ")))
    
    # if missing, set all toilet columns to NA
    dt[grepl("missing|manquant|no response|99", hv205) | is.na(hv205), (cols) := NA]
    
    
    # Improved toilet facilities
    # any toilet that flushes, ventilated improved pit, composting toilet, pit with slab
    dt[!is.na(flushs), toilet_improve := 0]
    dt[flushs == 1 | flusht == 1 | flushp == 1 | flushe == 1 | flushdk == 1 | latvip == 1 | latpits == 1 | latcompost == 1, toilet_improve := 1]
  } else {
    toilet_cols <- c()
  }
  
  
  # Toilet share
  
  # Create or modify the `shared_toilet` column
  if ("hv225" %in% names(dt)){
    # convert to character
    dt[, hv225 := as.character(as_factor(hv225))]
    dt[, hv225 := tolower(hv225)]
    
    # if toilet is shared
    dt[, shared_toilet := as.integer(hv225 %in% c("yes, other household only", "yes, public", "yes"))]
    
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
  } 
  
  
  ## Floor/Wall/Roof Types ----------------------------------------
  message("||---Floor/wall/roof types")
  
  # convert to character
  dt[, hv213 := as.character(as_factor(hv213))]
  dt[, hv213 := tolower(hv213)]
  
  # Type of floor
  dt[, dirtfloo := ifelse(grepl("earth|mud|dung|terre|bouse", hv213), 1, 0)]                               # earth/sand, dung, mud bricks
  dt[, woodfloo := ifelse(grepl("wood|bamboo|planche .*bois", hv213), 1, 0)]                               # wood planks
  dt[, prqfloo := ifelse(grepl("parquet", hv213), 1, 0)]                                                   # parquet or polished wood
  dt[, vinlfloo := ifelse(grepl("vinyl|linoleum|asphalt", hv213), 1, 0)]                                   # vinyl or asphalt strips
  dt[, tilefloo := ifelse(grepl("terraz+o|marble tiles|ceramic tiles|carreaux|carrelage", hv213), 1, 0)]   # ceramic tiles, terrazzo
  dt[, cemtfloo := ifelse(grepl("c(e|i)ment", hv213), 1, 0)]                                               # cement
  dt[, stonfloo := ifelse(grepl("stone|burnt brick|brick", hv213), 1, 0)]                                  # stone, bricks
  dt[, rugfloo := ifelse(grepl("carpet|taat|moquette", hv213), 1, 0)]                                      # carpet
  dt[, othfloo := ifelse(grepl("other|autre", hv213), 1, 0)]                                               # other
  
  # return responses that did not get coded into any 1's
  floor_cols <- names(dt)[grepl("floo$", names(dt))]
  message(paste("Unproccessed floor responses:", paste(unique(dt[rowSums(dt[,c(floor_cols),with=F])==0]$hv213), collapse = ", ")))
  
  # if missing, set all toilet columns to NA
  dt[grepl("missing|manquant|no response|99", hv213), (floor_cols) := NA]
  
  # Improved floor
  dt[!is.na(dirtfloo), floor_improve := 1]
  dt[dirtfloo == 1 | othfloo == 1, floor_improve := 0]
  
  
  # Type of wall
  ## all na for CMR 2004 & MWI 2000 & RWA 2000 & NPL 2001
  if ("hv214" %in% names(dt)) {
    # convert to character
    dt[, hv214 := as.character(as_factor(hv214))]
    dt[, hv214 := tolower(hv214)]
    
    dt[, nowall := ifelse(grepl("no walls", hv214), 1, 0)]# no walls
    dt[, natwall := ifelse(grepl("cane|dirt|palm|trunks with mud|sand|mud", hv214), 1, 0)]  # cane/palm/trunks, dirt/earth/mud
    dt[, mudwall := ifelse(grepl("bamboo .*with mud|pole with mud", hv214), 1, 0)]          # mud bricks, bamboo with mud
    dt[, stonwall := ifelse(grepl("stone with mud", hv214), 1, 0)]                          # stone with mud
    dt[, adobwall := ifelse(grepl("uncovered adobe", hv214), 1, 0)]                         # uncovered adobe
    dt[, plasticwall := ifelse(grepl("plastic sheeting", hv214), 1, 0)]                     # plastic
    dt[, plywall := ifelse(grepl("plywood", hv214), 1, 0)]                                  # plywood
    dt[, cardwall := ifelse(grepl("cardboard", hv214), 1, 0)]                               # cardboard
    dt[, rwoodwall := ifelse(grepl("reused wood", hv214), 1, 0)]                            # reused wood
    dt[, ubrkwall := ifelse(grepl("unburnt brick|broken brick", hv214), 1, 0)]              # unburnt bricks, broken bricks
    
    dt[, metalwall := ifelse(grepl("metal|galvanized sheet|iron sheet", hv214), 1, 0)]      # metal sheets
    dt[, cmtwall := ifelse(grepl("cement|stone with lime", hv214), 1, 0)]                   # cement, stone with lime
    dt[, brkwall := ifelse(grepl("^bricks*$|oven fired brick|burnt brick", hv214), 1, 0)]   # burnt bricks
    dt[, cmtbwall := ifelse(grepl("cement block", hv214), 1, 0)]                            # cement blocks
    dt[, cadobwall := ifelse(grepl("covered adobe", hv214), 1, 0)]                          # covered adobe
    dt[, woodwall := ifelse(grepl("wood planks", hv214), 1, 0)]                             # wood planks/shingles
    dt[, othwall := ifelse(grepl("other", hv214), 1, 0)]                                    # other
    
    # return responses that did not get coded into any 1's
    wall_cols <- names(dt)[grepl("wall$", names(dt))]
    message(paste("Unproccessed wall responses:", paste(unique(dt[rowSums(dt[,c(wall_cols),with=F])==0]$hv214), collapse = ", ")))
    
    # if missing, set all toilet columns to NA
    dt[grepl("missing|manquant|no response|99", hv214) | is.na(hv214), (wall_cols) := NA]
    
    # Improved walls
    # all na for CMR
    dt[!is.na(natwall), wall_improve := 1]
    dt[nowall == 1 | natwall == 1 | mudwall == 1 | stonwall == 1 | adobwall == 1 | plasticwall==1 | 
         plywall == 1 | cardwall == 1 | rwoodwall == 1 | ubrkwall == 1 | othwall == 1, wall_improve := 0]
  } else {
    wall_cols <- c()
  }
  
  
  # Type of roof
  ## all na for CMR 2004 and RWA 2000
  if ("hv215" %in% names(dt)) {
    # convert to character
    dt[, hv215 := as.character(as_factor(hv215))]
    dt[, hv215 := tolower(hv215)]
    
    dt[, noroof := ifelse(grepl("no roof|pas de toit", hv215), 1, 0)]                       # no roof
    dt[, natroof := ifelse(grepl("thatch|sod|chaume|herbe|straw", hv215), 1, 0)]            # thatch/palm leaf, sod
    dt[, mudroof := ifelse(grepl("mud brick|mud", hv215), 1, 0)]                            # mud
    dt[, matroof := ifelse(grepl("rustic mat|natte", hv215), 1, 0)]                         # rustic mat
    dt[, bambroof := ifelse(grepl("bamboo|bambou", hv215), 1, 0)]                           # palm/bamboo
    dt[, wproof := ifelse(grepl("wood plank|planche", hv215), 1, 0)]                        # wood planks
    dt[, cardroof := ifelse(grepl("cardboard", hv215), 1, 0)]                               # cardboard/polythene sheet
    
    dt[, metalroof := ifelse(grepl("metal|tôle|aluminum|iron sheet", hv215), 1, 0)]         # metal/tin/iron sheet
    dt[, woodroof := ifelse(grepl("^wood$|parquet|^bois$", hv215), 1, 0)]                   # wood
    dt[, asbroof:= ifelse(grepl("asbestos", hv215), 1, 0)]                                  # slate/asbestos
    dt[, tileroof := ifelse(grepl("roofing tile|ceramic tile|tuiles|tiles", hv215), 1, 0)]  # ceramic tiles
    dt[, cmtroof := ifelse(grepl("c(e|i)ment", hv215), 1, 0)]                               # cement
    dt[, shngroof := ifelse(grepl("shingles", hv215), 1, 0)]                                # roofing shingles
    dt[, othroof := ifelse(grepl("other|autre", hv215), 1, 0)]                              # other
    
    # return responses that did not get coded into any 1's
    roof_cols <- names(dt)[grepl("roof$", names(dt))]
    message(paste("Unproccessed roof responses:", paste(unique(dt[rowSums(dt[,c(roof_cols),with=F])==0]$hv215), collapse = ", ")))
    
    # if missing, set all toilet columns to NA
    dt[grepl("missing|manquant|no response|99", hv215) | is.na(hv215), (roof_cols) := NA]
    
    # Improved roofing
    dt[!is.na(natroof), roof_improve := 1]
    dt[noroof == 1 | natroof == 1 | mudroof == 1 | matroof == 1 | bambroof == 1 | wproof == 1 | cardroof == 1 | othroof == 1, roof_improve := 0]
  } else {
    roof_cols <- c()
  }
  
  
  ## Cooking Fuel/Energy ----------------------------------------
  message("||---Cooking fuel/energy")
  
  # Type of cooking fuel/energy
  if ("hv226" %in% names(dt)){
    # convert to character
    dt[, hv226 := as.character(as_factor(hv226))]
    dt[, hv226 := tolower(hv226)]
    
    dt[, cookelec := ifelse(grepl("electricit", hv226), 1, 0)]                            # electricity
    dt[, cookchulo := ifelse(grepl("improved smokeless chulo", hv226), 1, 0)]             # improved smokeless chulo
    dt[, cooklpg := ifelse(grepl("lpg|gpl", hv226), 1, 0)]                                # liquefied petroleum gas (lpg)
    dt[, cookgas := ifelse(grepl("cooking gas|diesel|natural gas|alcohol", hv226), 1, 0)] # cooking gas, gasoline/diesel, alcohol/ethanol, piped natural gas
    dt[, cookbio := ifelse(grepl("biogas|biogaz", hv226), 1, 0)]                          # biogas
    dt[, cookkero := ifelse(grepl("kerosene", hv226), 1, 0)]                              # kerosene
    dt[, cookcoal := ifelse(grepl("coal|lignite", hv226), 1, 0)]                          # coal, lignite
    dt[, cookchar := ifelse(grepl("briquette|charcoal", hv226), 1, 0)]                    # charcoal
    dt[, cookwood := ifelse(grepl("wood|sawdust|saw dust|woodchip", hv226), 1, 0)]        # wood, saw dust
    dt[, cookstraw := ifelse(grepl("straw|shrubs|grass", hv226), 1, 0)]                   # straw/grass/shrubs
    dt[, cookcrop := ifelse(grepl("agricultural crop", hv226), 1, 0)]                     # agricultural crop
    dt[, cooksolar := ifelse(grepl("solar", hv226), 1, 0)]                                # solar cooker
    dt[, cookdung := ifelse(grepl("dung|animal waste", hv226), 1, 0)]                     # animal waste, dung
    dt[, cooknone := ifelse(grepl("no food cooked", hv226), 1, 0)]                        # no food cooked in house
    dt[, cookoth := ifelse(grepl("other|garbage|autre", hv226), 1, 0)]                    # other, garbage
    
    # return responses that did not get coded into any 1's
    cook_cols <- names(dt)[grepl("cook", names(dt)) & !grepl("_", names(dt))]
    message(paste("Unproccessed cooking responses:", paste(unique(dt[rowSums(dt[,c(cook_cols),with=F])==0]$hv226), collapse = ", ")))
    
    # if missing, set all toilet columns to NA
    dt[grepl("missing|manquant|no response|99", hv226) | is.na(hv226), (cook_cols) := NA]
    
    # Modern fuel
    # following WHO IAQ guidelines
    dt[!is.na(cookelec), mod_fuel := 1]
    dt[cookkero == 1 | cookcoal == 1 | cookchar == 1 | cookwood == 1 | cookstraw == 1 | cookcrop == 1 | cookdung == 1 | cookoth == 1 | cooknone == 1, mod_fuel := 0]
  } else {
    cook_cols <- c()
  }
  
  
  ## Land Ownership and Area ----------------------------------------
  message("||---Land ownership/area")
  
  # owns any land and land area (in hectares)
  if (all(c("hv244", "hv245") %in% names(dt))){
    dt$hv245 <- as.numeric(dt$hv245)
    dt[, any_land := ifelse(hv244 == 1, 1, 0)]
    dt[, landarea := ifelse(!is.na(hv245), hv245/10, NA)]
    dt[any_land == 1 & hv245 == 0, landarea := 0.005]
    dt[is.na(hv245) | hv245 == 998, landarea := NA]
    dt[any_land == 0, landarea := 0]
  }
  
  
  
  # EXPORT CLEANED DATA --------------------------------------------
  message("||---Export cleaned data")
  
  # extract original wealth quintiles and scores for comparisons
  if ("hv270" %in% names(dt)) dt[, windex5 := hv270]
  if ("hv271" %in% names(dt)) dt[, wscore := hv271]
  
  # filter to variables of interest
  vars_interest <-  c("survey","cluster","hh_id","area_unit","country","region","strata","hhweight","hhid_unique",
                      "n_rooms_sleeping","n_hh_members","memsleep","electricity", "radio", "tv", "fridge", 
                      "bike", "moto", "car","land_ph", "mobile_ph", "watch", "animal_cart", "motor_boat",
                      livestock_cols,"bank_acc",h2o_cols,"h2o_improve",toilet_cols,"toilet_improve",
                      "shared_toilet","latshare","sflushs","sflusht","sflushp","sflushe","slatvip","slatpits",
                      "slatpit","slathang","slatoth",floor_cols,"floor_improve",wall_cols,
                      "wall_improve",roof_cols,"roof_improve","cook_type","cook_fuel",cook_cols,"mod_fuel",
                      "any_land","landarea","windex5","wscore")
  cur_vars <- names(dt)[names(dt) %in% vars_interest]
  dt_filter <- dt[, ..cur_vars]
  
  # save the cleaned data
  file_name <- gsub("\\.DTA", "", basename(survey))
  output_file_path <- file.path(out.dir, paste0(file_name, "_wealth_quintile_prep.csv"))
  write.csv(dt_filter, output_file_path, row.names = FALSE)
  
  message(paste0("Successfully saved ", file_name))
}


# RUN EXTRACTIONS ---------------------------------------------------

# Cameroon
extract_data("/FILEPATH/CMR_DHS4_2004_HH.DTA", "cm")
extract_data("/FILEPATH/CMR_DHS5_2011_HH.DTA", "cm")
extract_data("/FILEPATH/CMR_DHS7_2018_2019_HH.DTA", "cm")

# Ghana
extract_data("/FILEPATH/GHA_DHS4_2003_HH.DTA", "gh")
extract_data("/FILEPATH/GHA_DHS5_2008_HH.DTA", "gh")
extract_data("/FILEPATH/GHA_DHS6_2014_HH.DTA", "gh")
extract_data("/FILEPATH/GHA_DHS8_2022_2023_HH.DTA", "gh")

# Malawi
extract_data("/FILEPATH/MWI_DHS4_2000_HH.DTA", "mw")
extract_data("/FILEPATH/MWI_DHS4_2004_2005_HH.DTA", "mw")
extract_data("/FILEPATH/MWI_DHS6_2010_HH.DTA", "mw")
extract_data("/FILEPATH/MWI_DHS7_2015_2016_HH.DTA", "mw")

# Nepal
extract_data("/FILEPATH/NPL_DHS4_2001_HH.DTA", "np")
extract_data("/FILEPATH/NPL_DHS5_2006_HH.DTA", "np")
extract_data("/FILEPATH/NPL_DHS6_2011_HH.DTA", "np")
extract_data("/FILEPATH/NPL_DHS7_2016_2017_HH.DTA", "np")
extract_data("/FILEPATH/NPL_DHS8_2022_HH.DTA", "np")

# Rwanda
extract_data("/FILEPATH/RWA_DHS4_2000_HH.DTA", "rw")
extract_data("/FILEPATH/RWA_DHS4_2005_HH.DTA", "rw")
extract_data("/FILEPATH/RWA_DHS6_2010_2011_HH.DTA", "rw")
extract_data("/FILEPATH/RWA_DHS7_2014_2015_HH.DTA", "rw")
extract_data("/FILEPATH/RWA_DHS8_2019_2020_HH.DTA", "rw")

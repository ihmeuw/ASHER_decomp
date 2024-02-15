## Created by: Corinne Bintz
## Creation date: 12/6/2023
## MICS wealth quintile code: .sps code translated to R with help from Chat IHME
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
pacman::p_load(magrittr,tidyverse,parallel,plyr,viridis,scales,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source)) # IHME-specific line. comment out if running outside IHME

out.dir <- '/share/scratch/projects/hssa/asher/processed/'


extract_data <- function(survey, cur_country){
df <- read_dta(file.path(l, "IDENT/PROJECT_FOLDERS/UNICEF_MICS/", survey))

extract_df <- data.table()

extract_df$cluster <- df$HH1
extract_df$hh_id <- df$HH2
extract_df$area_unit <- df$HH6
extract_df$hhweight <- df$hhweight

extract_df$psu <- df$psu
extract_df$strata <- df$stratum
extract_df$region <- df$HH7
extract_df$year <- df$HH5Y

#unique cluster id for each nid
extract_df[, cluster_unique := paste(year, cluster)]

# unique psu for each nid
extract_df[, psu_unique := paste(year, psu)]

# unique hh id for each nid
extract_df[, hhid_unique := paste(year, hh_id, cluster)]

# Persons per sleeping room.
extract_df$persroom <- ifelse(df$HC3 < 98, df$HH48 / df$HC3, 99)

# Type of floor
extract_df$hc4_11 <- ifelse(df$HC4 == 11, 1, 0)    # earth/sand
extract_df$hc4_12 <- ifelse(df$HC4 == 12, 1, 0)    # dung
extract_df$hc4_21 <- ifelse(df$HC4 == 21, 1, 0)    # wood planks
extract_df$hc4_22 <- ifelse(df$HC4 == 22, 1, 0)    # palm/bamboo
extract_df$hc4_23 <- ifelse(df$HC4 == 23, 1, 0)    # stone
extract_df$hc4_31 <- ifelse(df$HC4 == 31, 1, 0)    # parquet or polished wood
extract_df$hc4_32 <- ifelse(df$HC4 == 32, 1, 0)    # vinyl or asphalt strips
extract_df$hc4_33 <- ifelse(df$HC4 == 33, 1, 0)    # ceramic tiles
extract_df$hc4_34 <- ifelse(df$HC4 == 34, 1, 0)    # cement
extract_df$hc4_35 <- ifelse(df$HC4 == 35, 1, 0)    # carpet
extract_df$hc4_36 <- ifelse(df$HC4 == 36, 1, 0)    # terrazzo

# Type of roof
extract_df$hc5_11 <- ifelse(df$HC5 == 11, 1, 0)    # no roof
extract_df$hc5_12 <- ifelse(df$HC5 == 12, 1, 0)    # thatch/palm leaf/rafia
extract_df$hc5_13 <- ifelse(df$HC5 == 13, 1, 0)    # sod
extract_df$hc5_21 <- ifelse(df$HC5 == 21, 1, 0)    # rustic mat
extract_df$hc5_22 <- ifelse(df$HC5 == 22, 1, 0)    # palm/bamboo
extract_df$hc5_23 <- ifelse(df$HC5 == 23, 1, 0)    # wood planks
extract_df$hc5_24 <- ifelse(df$HC5 == 24, 1, 0)    # cardboard/polythene sheet
extract_df$hc5_31 <- ifelse(df$HC5 == 31, 1, 0)    # metal/tin/corrugated iron sheet
extract_df$hc5_32 <- ifelse(df$HC5 == 32, 1, 0)    # wood
extract_df$hc5_33 <- ifelse(df$HC5 == 33, 1, 0)    # calamine/cement fibre
extract_df$hc5_34 <- ifelse(df$HC5 == 34, 1, 0)    # ceramic tiles
extract_df$hc5_35 <- ifelse(df$HC5 == 35, 1, 0)    # cement
extract_df$hc5_36 <- ifelse(df$HC5 == 36, 1, 0)    # roofing shingles
extract_df$hc5_37 <- ifelse(df$HC5 == 37, 1, 0)    # slate/asbestos

## Type of wall
extract_df$hc6_11 <- ifelse(df$HC6 == 11, 1, 0)    # no walls
extract_df$hc6_12 <- ifelse(df$HC6 == 12, 1, 0)    # cane/palm/trunks
extract_df$hc6_13 <- ifelse(df$HC6 == 13, 1, 0)    # dirt/earth/mud
extract_df$hc6_21 <- ifelse(df$HC6 == 21, 1, 0)    # bamboo with mud
extract_df$hc6_22 <- ifelse(df$HC6 == 22, 1, 0)    # stone with mud
extract_df$hc6_23 <- ifelse(df$HC6 == 23, 1, 0)    # uncovered adobe
extract_df$hc6_24 <- ifelse(df$HC6 == 24, 1, 0)    # plywood
extract_df$hc6_25 <- ifelse(df$HC6 == 25, 1, 0)    # cardboard
extract_df$hc6_26 <- ifelse(df$HC6 == 26, 1, 0)    # reused wood
extract_df$hc6_31 <- ifelse(df$HC6 == 31, 1, 0)    # cement
extract_df$hc6_32 <- ifelse(df$HC6 == 32, 1, 0)    # stone with lime/cement
extract_df$hc6_33 <- ifelse(df$HC6 == 33, 1, 0)    # bricks
extract_df$hc6_34 <- ifelse(df$HC6 == 34, 1, 0)    # cement blocks
extract_df$hc6_35 <- ifelse(df$HC6 == 35, 1, 0)    # covered adobe
extract_df$hc6_36 <- ifelse(df$HC6 == 36, 1, 0)    # wood planks/shingles
extract_df$hc6_37 <- ifelse(df$HC6 == 37, 1, 0)    # slate/asbestos

# Household assets - 1
extract_df$hc7a <- ifelse(df$HC7A == 1, 1, 0)      # fixed telephone line
extract_df$hc7b <- ifelse(df$HC7B == 1, 1, 0)      # radio
extract_df$hc7c <- ifelse(df$HC7C == 1, 1, 0)      # country-specific item

# Electricity
extract_df$hc8_1 <- ifelse(df$HC8 == 1, 1, 0)      # yes, interconnected grid
extract_df$hc8_2 <- ifelse(df$HC8 == 2, 1, 0)      # yes, off-grid (generator/isolated system)
extract_df$hc8_3 <- ifelse(df$HC8 == 3, 1, 0)      # no electricity

# Household assets - 2
extract_df$hc9a <- ifelse(df$HC9A == 1, 1, 0)      # television
extract_df$hc9b <- ifelse(df$HC9B == 1, 1, 0)      # refrigerator
extract_df$hc9c <- ifelse(df$HC9C == 1, 1, 0)      # country-specific item

# Household members assets
extract_df$hc10a <- ifelse(df$HC10A == 1, 1, 0)    # wristwatch
extract_df$hc10b <- ifelse(df$HC10B == 1, 1, 0)    # bicycle
extract_df$hc10c <- ifelse(df$HC10C == 1, 1, 0)    # motorcycle/scooter
extract_df$hc10d <- ifelse(df$HC10D == 1, 1, 0)    # animal drawn cart
extract_df$hc10e <- ifelse(df$HC10E == 1, 1, 0)    # car/truck/van
extract_df$hc10f <- ifelse(df$HC10F == 1, 1, 0)    # boat with motor
extract_df$hc10g <- ifelse(df$HC10G == 1, 1, 0)    # country-specific item

# Computer/mobile/internet
extract_df$hc11 <- ifelse(df$HC11 == 1, 1, 0)      # computer/tablet
extract_df$hc12 <- ifelse(df$HC12 == 1, 1, 0)      # mobile telephone
extract_df$hc13 <- ifelse(df$HC13 == 1, 1, 0)      # internet at home

#extract_df$hc15 <- ifelse(df$HC15 == 1, 1, 0)

# Recode HC16
extract_df$hc16r <- ifelse(df$HC16 == 0, 1, ifelse(df$HC16 >= 96, 99, df$HC16))
extract_df$hc16r[df$HC15 == 2] <- 0

# HC17
extract_df$hc17 <- ifelse(df$HC17 == 1, 1, 0)

# Recode HC18A HC18B HC18C HC18D HC18E HC18F HC18G HC18H (sysmis=0).
extract_df$HC18A[is.na(df$HC18A)] <- 0
# Do this for the rest of your HC18 variables...
extract_df$HC18B[is.na(df$HC18B)] <- 0
extract_df$HC18C[is.na(df$HC18C)] <- 0
extract_df$HC18D[is.na(df$HC18D)] <- 0
extract_df$HC18E[is.na(df$HC18E)] <- 0
extract_df$HC18F[is.na(df$HC18F)] <- 0
extract_df$HC18G[is.na(df$HC18G)] <- 0
extract_df$HC18H[is.na(df$HC18H)] <- 0

# HC19
extract_df$hc19 <- ifelse(df$HC19 == 1, 1, 0)

# EU1
extract_df$EU1_01 <- ifelse(df$EU1 == 1, 1, 0)
extract_df$EU1_02 <- ifelse(df$EU1 == 2, 1, 0)
extract_df$EU1_03 <- ifelse(df$EU1 == 3, 1, 0)
extract_df$EU1_04 <- ifelse(df$EU1 == 4, 1, 0)
extract_df$EU1_05 <- ifelse(df$EU1 == 5, 1, 0)
extract_df$EU1_06 <- ifelse(df$EU1 == 6, 1, 0)
extract_df$EU1_07 <- ifelse(df$EU1 == 7, 1, 0)
extract_df$EU1_08 <- ifelse(df$EU1 == 8, 1, 0)
extract_df$EU1_09 <- ifelse(df$EU1 == 9, 1, 0)
extract_df$EU1_97 <- ifelse(df$EU1 == 97, 1, 0)
# df$EU1_070101 <- ifelse(df$EU1 == "070101", 1, 0)
# df$EU1_070102 <- ifelse(df$EU1 == "070102", 1, 0)
# df$EU1_070108 <- ifelse(df$EU1 == "070108", 1, 0)
# df$EU1_070201 <- ifelse(df$EU1 == "070201", 1, 0)
# df$EU1_070202 <- ifelse(df$EU1 == "070202", 1, 0)
# df$EU1_070208 <- ifelse(df$EU1 == "070208", 1, 0)
# df$EU1_070801 <- ifelse(df$EU1 == "070801", 1, 0)
# df$EU1_070802 <- ifelse(df$EU1 == "070802", 1, 0)
# df$EU1_070808 <- ifelse(df$EU1 == "070808", 1, 0)
# df$EU1_080101 <- ifelse(df$EU1 == "080101", 1, 0)
# df$EU1_080102 <- ifelse(df$EU1 == "080102", 1, 0)
# df$EU1_080108 <- ifelse(df$EU1 == "080108", 1, 0)
# df$EU1_080201 <- ifelse(df$EU1 == "080201", 1, 0)
# df$EU1_080202 <- ifelse(df$EU1 == "080202", 1, 0)
# df$EU1_080208 <- ifelse(df$EU1 == "080208", 1, 0)
# df$EU1_080801 <- ifelse(df$EU1 == "080801", 1, 0)
# df$EU1_080802 <- ifelse(df$EU1 == "080802", 1, 0)
# df$EU1_080808 <- ifelse(df$EU1 == "070808", 1, 0)

# EU2, EU3
extract_df$eu2 <- ifelse(df$EU2 == 1, 1, 0)
extract_df$eu3 <- ifelse(df$EU2 == 1, 1, 0)

# EU4
extract_df$EU4_01 <- ifelse(df$EU4 == 1, 1, 0)
extract_df$EU4_02 <- ifelse(df$EU4 == 2, 1, 0)
extract_df$EU4_03 <- ifelse(df$EU4 == 3, 1, 0)
extract_df$EU4_04 <- ifelse(df$EU4 == 4, 1, 0)
extract_df$EU4_05 <- ifelse(df$EU4 == 5, 1, 0)
extract_df$EU4_06 <- ifelse(df$EU4 == 6, 1, 0)
extract_df$EU4_07 <- ifelse(df$EU4 == 7, 1, 0)
extract_df$EU4_08 <- ifelse(df$EU4 == 8, 1, 0)
extract_df$EU4_09 <- ifelse(df$EU4 == 9, 1, 0)
extract_df$EU4_10 <- ifelse(df$EU4 == 10, 1, 0)
extract_df$EU4_11 <- ifelse(df$EU4 == 11, 1, 0)

# EU5
extract_df$EU5_1 <- ifelse(df$EU5 == 1, 1, 0)
extract_df$EU5_2 <- ifelse(df$HC8 == 2, 1, 0)
extract_df$EU5_3 <- ifelse(df$HC8 == 3, 1, 0)
extract_df$EU5_4 <- ifelse(df$HC8 == 4, 1, 0)
extract_df$EU5_5 <- ifelse(df$HC8 == 5, 1, 0)

# EU6
extract_df$EU6_01 <- ifelse(df$EU6 == 1, 1, 0)
extract_df$EU6_02 <- ifelse(df$EU6 == 2, 1, 0)
extract_df$EU6_03 <- ifelse(df$EU6 == 3, 1, 0)
extract_df$EU6_04 <- ifelse(df$EU6 == 4, 1, 0)
extract_df$EU6_05 <- ifelse(df$EU6 == 5, 1, 0)
extract_df$EU6_06 <- ifelse(df$EU6 == 6, 1, 0)
extract_df$EU6_97 <- ifelse(df$EU6 == 97, 1, 0)
# df$EU6_0201 <- ifelse(df$EU6 == "0201", 1, 0)
# df$EU6_0202 <- ifelse(df$EU6 == "0202", 1, 0)
# df$EU6_0208 <- ifelse(df$EU6 == "0208", 1, 0)
# df$EU6_0301 <- ifelse(df$EU6 == "0301", 1, 0)
# df$EU6_0302 <- ifelse(df$EU6 == "0302", 1, 0)
# df$EU6_0308 <- ifelse(df$EU6 == "0308", 1, 0)
# df$EU6_0401 <- ifelse(df$EU6 == "0401", 1, 0)
# df$EU6_0402 <- ifelse(df$EU6 == "0402", 1, 0)
# df$EU6_0408 <- ifelse(df$EU6 == "0408", 1, 0)
# df$EU6_0501 <- ifelse(df$EU6 == "0501", 1, 0)
# df$EU6_0502 <- ifelse(df$EU6 == "0502", 1, 0)
# df$EU6_0508 <- ifelse(df$EU6 == "0508", 1, 0)

# EU7
extract_df$eu7 <- ifelse(df$EU7 == 1, 1, 0)

# EU8
extract_df$EU8_01 <- ifelse(df$EU8 ==1, 1, 0)
extract_df$EU8_02 <- ifelse(df$EU8 == 2, 1, 0)
extract_df$EU8_03 <- ifelse(df$EU8 == 3, 1, 0)
extract_df$EU8_04 <- ifelse(df$EU8 == 4, 1, 0)
extract_df$EU8_05 <- ifelse(df$EU8 == 5, 1, 0)
extract_df$EU8_06 <- ifelse(df$EU8 == 6, 1, 0)
extract_df$EU8_07 <- ifelse(df$EU8 == 7, 1, 0)
extract_df$EU8_08 <- ifelse(df$EU8 == 8, 1, 0)
extract_df$EU8_09 <- ifelse(df$EU8 == 9, 1, 0)
extract_df$EU8_10 <- ifelse(df$EU8 == 10, 1, 0)
extract_df$EU8_11 <- ifelse(df$EU8 == 11, 1, 0)
extract_df$EU8_12 <- ifelse(df$EU8 == 12, 1, 0)
extract_df$EU8_13 <- ifelse(df$EU8 == 13, 1, 0)
extract_df$EU8_14 <- ifelse(df$EU8 == 14, 1, 0)
extract_df$EU8_15 <- ifelse(df$EU8 == 15, 1, 0)
extract_df$EU8_16 <- ifelse(df$EU8 == 16, 1, 0)

# EU9
extract_df$EU9_01 <- ifelse(df$EU9 == 1, 1, 0)
extract_df$EU9_02 <- ifelse(df$EU9 == 2, 1, 0)
extract_df$EU9_03 <- ifelse(df$EU9 == 3, 1, 0)
extract_df$EU9_04 <- ifelse(df$EU9 == 4, 1, 0)
extract_df$EU9_05 <- ifelse(df$EU9 == 5, 1, 0)
extract_df$EU9_06 <- ifelse(df$EU9 == 6, 1, 0)
extract_df$EU9_07 <- ifelse(df$EU9 == 7, 1, 0)
extract_df$EU9_08 <- ifelse(df$EU9 == 8, 1, 0)
extract_df$EU9_09 <- ifelse(df$EU9 == 9, 1, 0)
extract_df$EU9_10 <- ifelse(df$EU9 == 10, 1, 0)
extract_df$EU9_11 <- ifelse(df$EU9 == 11, 1, 0)
extract_df$EU9_12 <- ifelse(df$EU9 == 12, 1, 0)
extract_df$EU9_13 <- ifelse(df$EU9 == 13, 1, 0)
extract_df$EU9_97 <- ifelse(df$EU9 == 97, 1, 0)

# WS1
extract_df$WS1_11 <- ifelse(df$WS1 == 11, 1, 0)
extract_df$WS1_12 <- ifelse(df$WS1 == 12, 1, 0)
extract_df$WS1_13 <- ifelse(df$WS1 == 13, 1, 0)
extract_df$WS1_14 <- ifelse(df$WS1 == 14, 1, 0)
extract_df$WS1_21 <- ifelse(df$WS1 == 21, 1, 0)
extract_df$WS1_31 <- ifelse(df$WS1 == 31, 1, 0)
extract_df$WS1_32 <- ifelse(df$WS1 == 32, 1, 0)
extract_df$WS1_41 <- ifelse(df$WS1 == 41, 1, 0)
extract_df$WS1_42 <- ifelse(df$WS1 == 42, 1, 0)
extract_df$WS1_51 <- ifelse(df$WS1 == 51, 1, 0)
extract_df$WS1_61 <- ifelse(df$WS1 == 61, 1, 0)
extract_df$WS1_71 <- ifelse(df$WS1 == 71, 1, 0)
extract_df$WS1_72 <- ifelse(df$WS1 == 72, 1, 0)
extract_df$WS1_81 <- ifelse(df$WS1 == 81, 1, 0)
extract_df$WS1_91 <- ifelse(df$WS1 == 91, 1, 0)
extract_df$WS1_92 <- ifelse(df$WS1 == 92, 1, 0)

# WS3 (WS3 in W2.sps, watloc in W3.sps)
extract_df$WS3_1 <- ifelse(df$WS3 == 1, 1, 0)
extract_df$WS3_2 <- ifelse(df$WS3 == 2, 1, 0)
extract_df$WS3_3 <- ifelse(df$WS3 == 3, 1, 0)
extract_df$WS3_4 <- ifelse(df$WS3 == 4, 1, 0)
extract_df$WS3_5 <- ifelse(df$WS3 == 5, 1, 0)

# WS4 (Three string variables, "MEMBERS DO NOT COLLECT", "dk", "NO RESPONSE"
# have all been converted to NAs)
extract_df$WS4_numeric <- as.numeric(as.character(df$WS4))

# WS7
extract_df$WS7_1 <- ifelse(df$WS7 == 1, 1, 0)
extract_df$WS7_2 <- ifelse(df$WS7 == 2, 1, 0)

# WS11
extract_df$WS11_11 <- ifelse(df$WS11 == 11, 1, 0)
extract_df$WS11_12 <- ifelse(df$WS11 == 12, 1, 0)
extract_df$WS11_13 <- ifelse(df$WS11 == 13, 1, 0)
extract_df$WS11_14 <- ifelse(df$WS11 == 14, 1, 0)
extract_df$WS11_18 <- ifelse(df$WS11 == 18, 1, 0)
extract_df$WS11_21 <- ifelse(df$WS11 == 21, 1, 0)
extract_df$WS11_22 <- ifelse(df$WS11 == 22, 1, 0)
extract_df$WS11_23 <- ifelse(df$WS11 == 23, 1, 0)
extract_df$WS11_31 <- ifelse(df$WS11 == 31, 1, 0)
extract_df$WS11_41 <- ifelse(df$WS11 == 41, 1, 0)
extract_df$WS11_51 <- ifelse(df$WS11 == 51, 1, 0)
extract_df$WS11_95 <- ifelse(df$WS11 == 95, 1, 0)

# WS14
extract_df$WS14_1 <- ifelse(df$WS14 == 1, 1, 0)
extract_df$WS14_2 <- ifelse(df$WS14 == 2, 1, 0)
extract_df$WS14_3 <- ifelse(df$WS14 == 3, 1, 0)

# WS15 (latshare in W3), WS16 (lathh1 in W3)
extract_df$WS15 <- ifelse(df$WS15 == 1, 1, 0)
extract_df$WS16_1 <- ifelse(df$WS16 == 1, 1, 0)
extract_df$WS16_2 <- ifelse(df$WS16 == 2, 1, 0)
extract_df$WS16_3 <- ifelse(df$WS16 == 3, 1, 0)

# WS17 (lathh2 in W3)
# Define the unique text values as they appear in your dataset. Make sure the text matches exactly.
levels_WS17 <- c("two", "three", "four", "five", "six", "seven", "eight", "nine", "TEN OR MORE HOUSEHOLDS", "dk", "NO RESPONSE")
extract_df$WS17<- factor(df$WS17, levels = levels_WS17, ordered = TRUE)



# latshare, lathh1, lathh2 (unsure what variables in the survey? -Paul)

# HW1, HW2, HW3 (soap in W3.sps), HW4
extract_df$HW1_1 <- ifelse(df$HW1 == 1, 1, 0)
extract_df$HW1_2 <- ifelse(df$HW1 == 2, 1, 0)
extract_df$HW1_3 <- ifelse(df$HW1 == 3, 1, 0)
extract_df$HW1_4 <- ifelse(df$HW1 == 4, 1, 0)
extract_df$HW1_5 <- ifelse(df$HW1 == 5, 1, 0)
extract_df$HW1_6 <- ifelse(df$HW1 == 6, 1, 0)
extract_df$HW2 <- ifelse(df$HW2 == 1, 1, 0)
extract_df$HW3 <- ifelse(df$HW3 == 1, 1, 0)
extract_df$HW4_1 <- ifelse(df$HW4 == 1, 1, 0)
extract_df$HW4_2 <- ifelse(df$HW4 == 2, 1, 0)
extract_df$HW4_3 <- ifelse(df$HW4 == 3, 1, 0)
extract_df$HW4_4 <- ifelse(df$HW4 == 4, 1, 0)

# servant (unsure what variables in the survey? -Paul)

extract_df[, country := cur_country]

## extract original wealth quintiles and scores for comparisons
extract_df$windex5 <- df$windex5
extract_df$wscore <- df$wscore

## Save the cleaned data
file_name <- file_path_sans_ext(basename(survey))
output_file_path <- file.path(out.dir, paste0(file_name, "_wealth_quintile_prep.csv"))
write.csv(data.table(extract_df), output_file_path, row.names = FALSE)
}

## enter survey and country here here 
extract_data("GHA/2017_2018/GHA_MICS6_2017_2018_HH_Y2020M04D10.DTA", "gha")
extract_data("MWI/2019_2020/MWI_MICS6_2019_2020_HH_Y2022M01D31.DTA", "mwi")


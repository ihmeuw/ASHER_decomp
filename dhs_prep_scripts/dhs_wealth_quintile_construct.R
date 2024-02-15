### ASHER DECOMP 
## WEALTH QUINTILE CREATION

## Created by: Corinne Bintz 
## Creation date: 11/16/2023
##  Create "wealth quintile"  for all households 
# clear memory
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
pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey,readstata13,zoo )

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source)) # IHME-specific line. comment out if running outside IHME
in.dir <- '/share/scratch/projects/hssa/asher/processed/'
out.dir <- '/share/scratch/projects/hssa/asher/processed/'

countries <- c("cm", "gh", "mw", "np", "rw")

process_wealth_quintile <- function(cur_country){
  ## read in hh processed dt's
  
  files<-list.files(in.dir)
  files <-files[grepl(cur_country, tolower(files))]
  files <-files[grepl("hh_wealth_quintile_dt", tolower(files)) | grepl("_wealth_quintile_prep", tolower(files))]
  
  possible_vars <- c( "hhid_unique", 'year', "survey", "electricity", "radio", "tv_color", "fridge", "bike", "moto", "car",
                      "land_ph", "mobile_ph", "watch", "animal_cart", "motor_boat", "bank_acc",
                      "internet", "comp", "wash_mach", "any_land", "landarea",
                      "memsleep", "h2oimprove","toilet_improve",
                      "floor_improve", "wall_improve", "roof_improve", "mod_fuel", "hv271","hv270","windex5", "wscore")
  cur_data <- rbindlist(lapply(file.path(in.dir, files), fread), fill = TRUE)
  
  current_data <- as.data.table(cur_data)

  ## for now, filter to first and last years
  first_year <- ifelse(cur_country == "cm", 2004,
                       ifelse(cur_country == "gh", 2003, 
                              ifelse(cur_country== 'mw',2000,
                                     ifelse(cur_country == "np",2006,2000))))
  
  last_year <- ifelse(cur_country == "cm", 2018,
                      ifelse(cur_country == "gh", 2022, 
                             ifelse(cur_country== 'mw',2019,
                                    ifelse(cur_country == "np",2021,2019))))
  
  first_last_years <- c(first_year,last_year)
  
  ## adjust years to just be one 
  if (cur_country == "cm"){
    current_data[, year := ifelse(year %in% c(2018,2019), 2018, year)]
  }
  if (cur_country == "mw"){
    current_data[, year := ifelse(year %in% c(2019,2020), 2019, year)]
  }
  
  if (cur_country == "np"){
    current_data[, year := ifelse(year %in% c(2005,2006), 2006,
                                  ifelse(year %in% c(2021,2022), 2021,year))]
  }
  if (cur_country == "rw"){
    current_data[, year := ifelse(year %in% c(2020,2019), 2019, year)]
    
  }
  if (cur_country == "gh"){
    current_data[, year := ifelse(year %in% c(2022,2023), 2022, year)]
  }
  current_data <- current_data[year %in% first_last_years]
  indicators <- names(current_data)[names(current_data) %in% possible_vars]
  test_vars <- indicators[ !indicators %in% c('hhid_unique', 'year', "survey", "hv271","hv270","windex5", "wscore")]
  test <- current_data[, ..test_vars]
  
  ## find variables that have a standard deviation of 0 r that is missing 
  sd_values <- data.table(sapply(test, sd, na.rm = TRUE))
  dt_sd <- data.table(names = names(test), sd = sapply(test, sd, na.rm = TRUE))
  dt_sd_wide <- melt(dt_sd, id.vars = "names", value.name = "sd")
  dt_sd_wide <- dt_sd_wide[!is.na(sd) & sd !=0]
  
  filter_names <- c("hhid_unique","year", dt_sd_wide$names)
  analysis_vars <- current_data[, filter_names, with = FALSE]
  # function to calculate missing data
  missing_percentage <- function(data, year_col, var_col) {
    # subset data by year
    data_by_year <- data[year == year_col]
    missing_count <- sum(is.na(data_by_year[[var_col]]))
    total_count <- nrow(data_by_year)
    missing_percentage <- (missing_count / total_count) * 100
    return(missing_percentage)
  }
  
  # function to calculate missing data and returns vars with 100% missing for any given year
  missing_percentage_all_vars <- function(data, year_col) {
    # get all variable columns
    var_cols <- setdiff(names(data), year_col)
    
    # initialize an empty vector to store variables with 100% missing values
    vars_with_100_percent_missing <- c()
    
    # calculate missing percentage per year for each variable
    for(var_col in var_cols) {
      missing_percentages <- missing_percentage(data, year_col, var_col)
      if(any(missing_percentages == 100)) {
        vars_with_100_percent_missing <- c(vars_with_100_percent_missing, var_col)
      }
    }
    
    return(vars_with_100_percent_missing)
  }
  missing_vars_all <- as.character()
  for (cur_year in unique(analysis_vars$year)){
    tmp_missing <- missing_percentage_all_vars(data = analysis_vars, year_col = cur_year)
    missing_vars_all <- c(missing_vars_all, tmp_missing)
  }
  
  ## remove variables that are completly missing in any given year
  filter_names <- filter_names[!filter_names %in% missing_vars_all]
  analysis_vars <- analysis_vars[, ..filter_names]
  
  ## replace missing values with mean of column
  
  # Define the function
  replace_na_with_median <- function(column) {
    median_value <- median(column, na.rm = TRUE)
    column[is.na(column)] <- median_value
    return(column)
  }
  
  # Apply the function to all columns
  id_df <- analysis_vars[, c('hhid_unique','year')]
  df <- sapply(analysis_vars[, -c('hhid_unique','year')], replace_na_with_median)
  
  analysis_vars <- cbind(id_df,df)
  
  summary(analysis_vars[,-1])
  av_corr <- cor(analysis_vars[,-1])
  summary(av_corr)
  x <- data.table(av_corr)
  #ggcorrplot(av_corr)
  
  ## check if any variables are constant in data  
  check_same_values <- function(current_dt, column_name) {
    # Get the unique values in the column
    unique_values <- unique(current_dt[[column_name]])
    
    # Check if the length of unique_values is 1, which means all values are the same
    if (length(unique_values) == 1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  # Apply the function to all columns of 'dt'
  same_values_all_columns <- lapply(names(analysis_vars), function(column_name) check_same_values(analysis_vars, column_name))
  
  # Convert the result to a named vector
  same_values_all_columns <- unlist(same_values_all_columns)
  names(same_values_all_columns) <- names(analysis_vars)
  
  ## check here for high colliniearity which won't work in the PCA
  factors <- princomp(analysis_vars[, -c('hhid_unique', 'year')], scores = TRUE, fix_sign =TRUE, cor = TRUE) 
  indicators <-  names(analysis_vars)[!names(analysis_vars) %in% c('hhid_unique', 'year')]

  # ## Function to weight and combine PCA loadings by p proportion of variance explained
  getWeights <- function(pcaobj, var_names, p = 1, method = "A", nfactors = NA) {
    # Keep each factor until we get to p variance explained
    vars <- c()
    loadingsdf <- data.table(var_names= var_names)
    if (is.na(nfactors)) nfactors <- nrow(pcaobj$loadings)
    n <- 1
    for (n in 1:nfactors) {
      loadingsdf <- loadingsdf[, eval(paste0("PC", n)) := pcaobj$loadings[, n]]
      vars <- c(vars, ((pcaobj$sdev) ^ 2 / sum(pcaobj$sdev ^ 2))[n])
      varexp <- cumsum((pcaobj$sdev) ^ 2 / sum(pcaobj$sdev ^ 2))[n]
      if (varexp > p) break
    }
    ## Rescale variance weighting 
    vars_rescaled <- vars / sum(vars)
    
    ## Flip signs of loadings so that it points in "majority" direction
    #   Method A: relies on sum. Variables must be same scale
    #   Method B: relies on number. Sum is used as tie breaker
    if(method == "A") {
      for(fact in setdiff(names(loadingsdf), c("var_names"))){
        if (abs(sum(loadingsdf[get(fact) < 0, get(fact)])) > sum(loadingsdf[get(fact) > 0, get(fact)])) {
          loadingsdf[, eval(fact) := get(fact)*(-1)]
        } 
      }
    }else if(method == "B"){
      for(fact in setdiff(names(loadingsdf), c("var_names"))){
        if (nrow(loadingsdf[get(fact) < 0]) > nrow(loadingsdf[get(fact) > 0])) {
          loadingsdf[, eval(fact) := get(fact)*(-1)]
        }else if(nrow(loadingsdf[get(fact) < 0]) == nrow(loadingsdf[get(fact) > 0])){
          if (abs(sum(loadingsdf[get(fact) < 0, get(fact)])) > sum(loadingsdf[get(fact) > 0, get(fact)])) {
            loadingsdf[, eval(fact) := get(fact)*(-1)]
          }
        }
      }
    }else{
      error("Must define method A or B in function")
    }
    
    # Get variable weight (weighted sum of loadings by proportion of variance explained)
    loadingsdf$comp_loading <- apply(as.matrix(loadingsdf[, paste0("PC", 1:length(vars)), with = FALSE]) %*% diag(vars_rescaled),
                                     1, sum, na.rm = T)
    
    # Add row to loadings data with explained variance
    suppdf <- data.table(var_names = "variance explained", comp_loading = NA)
    suppdf <- suppdf[, paste0("PC", 1:length(vars)) := as.list(vars)]
    loadingsdf <- rbind(loadingsdf[, c("var_names", "comp_loading", paste0("PC", 1:length(vars))), with = FALSE],
                        suppdf, fill=T)
    
    # Only include positive weights, rescale to 1
    #  loadingsdf <- loadingsdf[comp_loading > 0, var_weight := comp_loading / sum(comp_loading)]
    loadingsdf[var_names != "variance explained", var_weight := comp_loading / sum(comp_loading)]
    
    return(loadingsdf)
  }
  
  # run PCA
  pca <- princomp(analysis_vars[, ..indicators], scores = TRUE, fix_sign =TRUE, cor = TRUE)
  
  # get weights using all principal components
  test <- getWeights(pca, var_names = indicators, p = 1)
  
  # map values onto first principal component
  analysis_vars[, all_indics := predict(pca)[,1]]
  
  ## melt analysis vars dt from wide to long
  analysis_vars_long <- melt(analysis_vars, id.vars = c("hhid_unique", "year"))
  
  # merge on weights and create weighted PCA index using all principal components
  dt_weights <- merge(na.omit(analysis_vars_long[variable != "all_indics"]), test[var_names != "variance explained" & var_names != "all_indics", c("var_names", "var_weight")], by.x = "variable", by.y = "var_names", all = T)
  
  setnames(dt_weights, "var_weight", "all_weight")
  check_na <- dt_weights[is.na(all_weight)]
  
  wealth_dt <- dt_weights[, .(wealth_scores = sum(value * all_weight)), by = c("hhid_unique", "year")]
  nrow(wealth_dt) # these should be the same 
  nrow(analysis_vars)
  
  ## compare to previous wealth score
  if(cur_country == "mw"){ ## include MICS wealth quintile constructed values 
    for_comp <- current_data[, c("hhid_unique", "hv271","hv270", "year", "survey","windex5", "wscore" )]
  } else{
    for_comp <- current_data[, c("hhid_unique", "hv271","hv270", "year", "survey")]
  }
  for_comp <- merge(for_comp,wealth_dt, by = c("hhid_unique", "year"))
  
  ## compute wealth index for all years at once
  cuts <- quantile(wealth_dt$wealth_scores, probs = c(.2, .4, .6, .8, 1))
  cuts <- as.data.frame(cuts)
  
  wealth_dt$wealth_index[wealth_dt$wealth_scores <= cuts[1,]] <- 1 
  wealth_dt$wealth_index[wealth_dt$wealth_scores > cuts[1,] & wealth_dt$wealth_scores <= cuts[2,]] <- 2
  wealth_dt$wealth_index[wealth_dt$wealth_scores > cuts[2,] & wealth_dt$wealth_scores <= cuts[3,]] <- 3
  wealth_dt$wealth_index[wealth_dt$wealth_scores > cuts[3,] & wealth_dt$wealth_scores <= cuts[4,]] <- 4
  wealth_dt$wealth_index[wealth_dt$wealth_scores > cuts[4,]  ] <- 5
  
  
  for (cur_year in unique(for_comp$year)){
    file_name <- unique(for_comp[year == cur_year]$survey)
    print(cur_country)
    print(cur_year)
    tmp_dt <- for_comp[year == cur_year]
    print(cor(as.numeric(tmp_dt$hv271), tmp_dt$wealth_scores)) 
    if(cur_country == "mw" & cur_year == 2019){
    print(cor(as.numeric(tmp_dt$wscore), tmp_dt$wealth_scores)) ## for MICS
    }
    
    # # correlation with existing weights
    tmp_dt$old_wi[tmp_dt$hv270 == "poorest"] <- 1 ## poorest
    tmp_dt$old_wi[tmp_dt$hv270 == "poorer"] <- 2 ##poorer
    tmp_dt$old_wi[tmp_dt$hv270 == "middle"] <- 3 ## middle
    tmp_dt$old_wi[tmp_dt$hv270 == "richer"] <- 4 ## richer
    tmp_dt$old_wi[tmp_dt$hv270 == "richest"] <- 5 ## richest
    
    wealth_dt_current <- wealth_dt[year == cur_year]
    check <- merge(tmp_dt, wealth_dt_current, by = c("hhid_unique", "year", "wealth_scores"))
    
    print(cor(check$old_wi, check$wealth_index)) # if falls below .8 may want to adjust
    if(cur_country == "mw" & cur_year == 2019){
    print(cor(check$windex5, check$wealth_index)) # if falls below .8 may want to adjust
    }
     
    
    # # Export to merge with the individual recode
   write.csv(check, file.path(out.dir, paste(file_name,"wealth_quintile_dt_summary.csv", sep ="_")), row.names = FALSE)
  }
  
}
for (cur_country in countries){
  process_wealth_quintile(cur_country)
}
print(factors)
plot(factors) #
#########################
## CHECK TIME TRENDS ####
#########################

### When have multiple years of data loaded
## will want to see how wealth changes over time
# we would mostly expect wealth to increase in the countries where we are working
# which would mean fewer people in quintile 1 and more in the other, higher quintiles


in.dir <- '/share/scratch/projects/hssa/asher/processed/'
out.dir <- '/share/scratch/projects/hssa/asher/processed/'

countries <- c("cm", "gh", "mw", "np", "rw")

cur_country <- "rw"

country_title <- ifelse(cur_country == "cm", "Cameroon",
                        ifelse(cur_country == "gh", "Ghana", 
                               ifelse(cur_country== 'mw', "Malawi",
                                      ifelse(cur_country == "np", "Nepal", "Rwanda"))))


files<-list.files(in.dir)
files <-files[grepl(cur_country, tolower(files))]
files <-files[grepl("wealth_quintile_dt_summary.csv", tolower(files))]
cur_data <- rbindlist(lapply(file.path(in.dir, files), fread), fill = TRUE)

unique(cur_data$year)

cur_data$wealth_index <- factor(cur_data$wealth_index, levels=c(1,2,3,4,5), labels = c(1,2,3,4,5))
gg <- ggplot(cur_data, aes(as.factor(year), group=wealth_index)) + geom_bar(aes(fill = as.numeric(wealth_index)), position = "fill",width = 0.4) +
  xlab("Year") + scale_fill_viridis(name = "Wealth Quintile") +
  scale_y_reverse()+
  ggtitle(country_title)+
  ylab("Percentage") + theme_bw()


ggsave(file.path(out.dir, paste0(cur_country, "_wealth_quintile_time_series.png")), width = 5, height = 5)

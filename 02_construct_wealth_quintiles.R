#-------------------Header------------------------------------------------
# Project: IHME ASHER Decomposition
# Purpose: Create country-specific wealth index quintiles through PCA
# Date: 11/16/23
# Notes:
#***************************************************************************

# SET-UP ----------------------------------------------------------

# clear memory
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
pacman::p_load(data.table,magrittr,tidyverse,parallel,plyr,scales,ggridges,dplyr,haven,survey,readstata13,zoo,corrplot)

# in/out
in.dir <- "FILEPATH"
out.dir <- "FILEPATH"

# create function for the opposite of %in%
'%ni%' <- Negate('%in%')

# countries to create wealth index for
countries <- c("cm", "gh", "mw", "np", "rw")


# PROCESS WEALTH QUINTILE FUNCTION ---------------------------------

process_wealth_quintile <- function(cur_country, baseline_endline = F, cohort = F) {
  
  
  # PREP DATA -------------------------------------------------
  
  print(cur_country)
  
  # list files in directory of interest and subset to country files
  files <- list.files(in.dir, pattern = ".csv")
  files <- files[grepl(cur_country, tolower(files))]
  
  # if baseline_endline, update out_dir and subset files
  if (baseline_endline == T) {
    
    # update out_dir
    out.dir <- "FILEPATH"
    
    # subset files
    if (cur_country == "rw") files <- files[grepl("DHS4_2000|DHS8_2019", files)]
    if (cur_country == "gh") files <- files[grepl("DHS4_2003|DHS8_2022", files)]
    if (cur_country == "np") files <- files[grepl("DHS5_2006|DHS8_2022", files)]
    if (cur_country == "mw") files <- files[grepl("DHS4_2000|MICS6_2019", files)]
    if (cur_country == "cm") files <- files[grepl("DHS4_2004|DHS7_2018", files)]
  }
  
  # if cohort, update out_dir and subset files
  if (cohort == T) {
    
    # update out_dir
    out.dir <- "FILEPATH"
    
    # subset files
    if (cur_country == "rw") files <- files[grepl("DHS6_2010|DHS7_2014|DHS8_2019", files)]
    if (cur_country == "gh") files <- files[grepl("DHS5_2008|DHS6_2014|DHS8_2022", files)]
    if (cur_country == "np") files <- files[grepl("DHS5_2006|DHS6_2011|DHS7_2016|DHS8_2022", files)]
    if (cur_country == "mw") files <- files[grepl("DHS4_2004|DHS6_2010|DHS7_2015", files)]
    if (cur_country == "cm") return()
  }
  
  # read in extracted wealth quintile data
  files <- files[grepl("wealth_quintile_prep", files)]
  data <- rbindlist(lapply(file.path(in.dir, files), fread), fill = TRUE)
  
  # drop if hhweight == 0 (MICS entries that did not complete survey)
  data <- data[hhweight != 0]
  
  
  ## DATA DIAGNOSTICS ------------------------------------------
  
  # check data availability
  data_avail_all <- data[, lapply(.SD, function(x) sum(!is.na(x))/.N), by = "survey"]
  data_avail_all <- dcast(melt(data_avail_all, id.vars = "survey"), formula = variable ~ survey)
  write.csv(data_avail_all, file.path(out.dir, paste0(cur_country, "_data_availability_all_vars.csv")), row.names = F)
  
  # list of variables to test PCA
  possible_vars <- c("survey", "hhid_unique", "memsleep", "electricity", "radio", "fridge", "bike", "moto", "car",
                     "land_ph", "mobile_ph", "watch", "animal_cart", "motor_boat", "internet",
                     "comp", "tv", "tv_color", "wash_mach", "bank_acc", "h2o_home", "h2o_improve",
                     "toilet_improve", "latshare", "floor_improve", "wall_improve", "roof_improve",
                     "mod_fuel", "any_land", "landarea", "windex5", "wscore")
  
  # subset data to test set
  indicators <- names(data)[names(data) %in% possible_vars]
  data_test <- data[, ..indicators]
  
  test_vars <- indicators[!indicators %in% c("survey","windex5","wscore","hhid_unique")]
  test <- data[, ..test_vars]
  
  # check data availability
  data_avail_test <- data_test[, lapply(.SD, function(x) sum(!is.na(x))/.N), by = "survey"]
  data_avail_test <- dcast(melt(data_avail_test, id.vars = "survey"), formula = variable ~ survey)
  write.csv(data_avail_test, file.path(out.dir, paste0(cur_country, "_data_availability_test_vars.csv")), row.names = F)
  
  # visualize data availability heat map
  plot_dt <- melt(data_avail_test, id.vars = "variable", variable.name = "survey")
  plot_dt <- plot_dt[variable %ni% c("wscore","windex5","hhid_unique")]
  plot_dt[, variable := factor(variable, levels = possible_vars[plot_dt$variable %in% possible_vars])]
  
  gg <- ggplot(data = plot_dt, aes(x = variable, y = survey, fill = value)) +
    geom_tile(color = "white", lwd = .4) +
    scale_fill_distiller(palette = "RdYlGn", labels = percent, direction = 1) +
    scale_x_discrete(position = "top", labels = wrap_format(20)) +
    scale_y_discrete(limits = rev, labels = wrap_format(15)) +
    labs(fill = "Availability") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 0, size = 11), axis.title = element_blank(),
                            axis.text.y = element_text(hjust = 1, size = 11), 
                            axis.ticks = element_blank(), panel.grid.major = element_blank()) +
    coord_fixed()
  gg
  ggsave(file.path(out.dir, paste0(cur_country, "_heatmap_data_availability.png")), w=12, h=5)
  ggsave(file.path(out.dir, paste0(cur_country, "_heatmap_data_availability.pdf")), w=12, h=5)
  
  
  # visualize associations of predictors with survey-assigned wealth quintiles
  plot_dt <- data_test[, lapply(.SD, mean, na.rm = T), by = c("survey","windex5"), .SDcols = test_vars]
  plot_dt <- melt(plot_dt, id.vars = c("survey", "windex5"))
  
  gg <- ggplot(data = plot_dt[windex5 %in% 1:5], aes(x = as.factor(windex5), y = value)) +
    geom_point() +
    labs(x = "Wealth Index Quintile (1 = Lowest, 5 = Highest)", y = "Mean of Variable") +
    theme_bw() + theme(strip.text.x = element_text(size = 6)) +
    facet_grid(survey~variable, scales = "free")
  gg
  ggsave(file.path(out.dir, paste0(cur_country, "_visualize_predictors_windex5.pdf")), w=18, h=9)
  
  
  ## HANDLE MISSINGNESS ----------------------------------------
  
  # identify variables 100% missing for any given survey
  vars_missing_data <- data_avail_test[rowSums(data_avail_test==0)>0]
  vars_missing <- vars_missing_data$variable
  
  # remove variables that are completely missing in any given year
  analysis_vars <- test_vars[!test_vars %in% vars_missing]
  data_analysis <- test[, ..analysis_vars]
  
  # ensure all variables are numeric
  data_analysis[, (analysis_vars) := lapply(.SD, as.numeric), .SDcols = c(analysis_vars)]
  
  # replace continuous missing values with mean of column
  continuous_vars <- analysis_vars[analysis_vars %in% c("memsleep","landarea")]
  if (!is_empty(continuous_vars)) {
    data_analysis[, (continuous_vars) := lapply(.SD, function(x) ifelse(is.na(x), mean(x, na.rm = T), x)), .SDcols = continuous_vars]
  }
  
  # replace dichotomous missing values with 0
  dichotomous_vars <- analysis_vars[analysis_vars %ni% c("memsleep","landarea")]
  if (!is_empty(dichotomous_vars)) {
    data_analysis[, (dichotomous_vars) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = dichotomous_vars]
  }
  
  
  # RUN PCA -------------------------------------------------
  
  # check correlation matrix and plot correlations
  corr <- cor(data_analysis)
  pdf(file.path(out.dir, paste0(cur_country, "_pca_indicators_corr_matrix.pdf")), width = 9, height = 9)
  plot_corr <- corrplot(corr, type = "upper", order = "original")
  dev.off()
  
  # function to weight and combine PCA loadings by p proportion of variance explained
  getWeights <- function(pcaobj, var_names, p = 1, method = "A", nfactors = NA) {
    # keep each factor until we get to p variance explained
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
    # rescale variance weighting 
    vars_rescaled <- vars / sum(vars)
    
    # flip signs of loadings so that it points in "majority" direction
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
    
    # get variable weight (weighted sum of loadings by proportion of variance explained)
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
  pca <- princomp(data_analysis, scores = TRUE, fix_sign =TRUE, cor = TRUE)
  
  # get weights using all principal components
  test <- getWeights(pca, var_names = analysis_vars, p = 1)
  
  # save pca results for future reference
  write.csv(test, file.path(out.dir, paste0(cur_country, "_pca_loadings.csv")), row.names = F)
  
  # map values onto first principal component
  data_analysis[, all_indics := predict(pca)[,1]]
  
  # melt analysis data from wide to long
  data_analysis$survey <- data_test$survey
  data_analysis$hhid_unique <- data_test$hhid_unique
  data_analysis_long <- melt(data_analysis, id.vars = c("survey","hhid_unique"))
  
  # merge on weights and create weighted PCA index using all principal components
  dt_weights <- merge(na.omit(data_analysis_long[variable != "all_indics"]), test[var_names != "variance explained" & var_names != "all_indics", c("var_names", "var_weight")], by.x = "variable", by.y = "var_names", all = T)
  
  # calculate wealth scores by summing variable value * pca weight
  wealth_dt <- dt_weights[, .(wealth_scores = sum(value * var_weight)), by = c("survey", "hhid_unique")]
  nrow(wealth_dt) == nrow(data_analysis)  # these should be the same
  
  # calculate wealth quintile cutoffs 
  cuts <- quantile(wealth_dt$wealth_scores, probs = c(.2, .4, .6, .8, 1))
  
  # assign quintiles
  wealth_dt[wealth_scores <= cuts[1], wealth_quintiles := 1]
  wealth_dt[wealth_scores > cuts[1] & wealth_scores <= cuts[2], wealth_quintiles := 2]
  wealth_dt[wealth_scores > cuts[2] & wealth_scores <= cuts[3], wealth_quintiles := 3]
  wealth_dt[wealth_scores > cuts[3] & wealth_scores <= cuts[4], wealth_quintiles := 4]
  wealth_dt[wealth_scores > cuts[4] & wealth_scores <= cuts[5], wealth_quintiles := 5]
  
  # add on original scores and quintiles for comparison
  wealth_dt$wscore <- data$wscore
  wealth_dt$windex5 <- data$windex5
  
  
  # VISUALIZE RESULTS ---------------------------------------------
  
  # plot distributions of quintiles by survey
  plot_dt <- wealth_dt[, .(count = .N), by = c("survey", "wealth_quintiles")]
  plot_dt[, year := as.numeric(str_extract(survey, "[0-9]{4}"))]
  plot_dt[, wealth_quintiles := factor(wealth_quintiles, levels = 1:5, labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))]
  
  gg <- ggplot(data = plot_dt, aes(x = reorder(survey, year), y = count, fill = fct_rev(as.factor(wealth_quintiles)))) +
    geom_bar(stat = "identity", position = "fill") +
    labs(x = "Survey", y = "Proportion of Respondents", fill = "Wealth Quintile") +
    theme_bw() + theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 9))
  gg
  if (!baseline_endline) {
    ggsave(file.path(out.dir, paste0(cur_country, "_wealth_quintiles_barchart.png")), w=12, h=5)
    ggsave(file.path(out.dir, paste0(cur_country, "_wealth_quintiles_barchart.pdf")), w=12, h=5)
  } else {
    ggsave(file.path(out.dir, paste0(cur_country, "_wealth_quintiles_barchart.png")), w=7, h=5)
    ggsave(file.path(out.dir, paste0(cur_country, "_wealth_quintiles_barchart.pdf")), w=7, h=5)
  }
 
  
  # EXPORT RESULTS ---------------------------------------------
  
  # save constructed wealth scores and quintiles
  write.csv(wealth_dt, file.path(out.dir, paste0(cur_country, "_wealth_quintile_estimates.csv")), row.names = FALSE)
}


# CONSTRUCT WEALTH QUINTILES ------------------------------------------

# Ghana
process_wealth_quintile(cur_country = "gh")
process_wealth_quintile(cur_country = "gh", baseline_endline = T)
process_wealth_quintile(cur_country = "gh", cohort = T)

# Cameroon
process_wealth_quintile(cur_country = "cm")
process_wealth_quintile(cur_country = "cm", baseline_endline = T)

# Nepal
process_wealth_quintile(cur_country = "np")
process_wealth_quintile(cur_country = "np", baseline_endline = T)
process_wealth_quintile(cur_country = "np", cohort = T)

# Rwanda
process_wealth_quintile(cur_country = "rw")
process_wealth_quintile(cur_country = "rw", baseline_endline = T)
process_wealth_quintile(cur_country = "rw", cohort = T)

# Malawi
process_wealth_quintile(cur_country = "mw")
process_wealth_quintile(cur_country = "mw", baseline_endline = T)
process_wealth_quintile(cur_country = "mw", cohort = T)

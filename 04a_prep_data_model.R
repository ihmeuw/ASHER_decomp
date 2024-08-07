#-------------------Header------------------------------------------------
# Project: IHME ASHER Decomposition
# Purpose: Prep merged data for Oaxaca Blinder modeling, create data diagnostics
# Date: 2/16/2024
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
pacman::p_load(data.table,magrittr,tidyverse,parallel,plyr,dplyr,haven,survey,tools,devtools)

# in/out
in.dir <- "FILEPATH"
out.dir <- "FILEPATH"
out.dir_diagnostics <- "FILEPATH"

# create function for the opposite of %in%
'%ni%' <- Negate('%in%')


# PREP DATA -------------------------------------------------------

# countries to prep files for
countries <- c("gh", "mw", "np", "rw", "cm")

# loop through each country to merge
for (cur_country in countries) {
  
  print(cur_country)
  
  # list csv files in merged directory
  files <- list.files(in.dir, pattern = ".csv")
  
  # subset to files for country of interest
  files <- files[grepl(cur_country, tolower(files))]
  
  # read in and combine all surveys for a country
  dt <- rbindlist(lapply(file.path(in.dir, files), fread), fill = T)
  
  
  # RECODE/CREATE ADDITIONAL VARIABLES ---------------------------
  
  # fix attend_school for MICS
  # only asked to individuals 3-24 who have ever attended school
  if (any(grepl("MICS", unique(dt$survey)))) dt[grepl("MICS", survey) & age %in% 15:24 & ever_attend_school == 0 & is.na(attend_school), attend_school := 0]
  
  # finish creation of educ_single_yrs for NPL 2010 and NPL 2014, 8-4 system
  if (cur_country == "np") {
    dt[grepl("NPL_MICS4", survey) & grepl("Prim|Secon", edu_level_categ_wn), educ_single_yrs := edu_years_in_level]
    dt[grepl("NPL_MICS4", survey) & edu_years_in_level == 11, educ_single_yrs := 10]     # SLC exam given at end of year 10 in 2010
    dt[grepl("NPL_MICS4", survey) & edu_years_in_level == 12, educ_single_yrs := 12]     # plus 2 are two years of education after year 10
    dt[grepl("NPL_MICS4", survey) & edu_years_in_level == 13, educ_single_yrs := 16]     # bachelor
    dt[grepl("NPL_MICS4", survey) & edu_years_in_level == 14, educ_single_yrs := 18]     # master
    
    dt[grepl("NPL_MICS5", survey) & edu_years_in_level %in% 0:10, educ_single_yrs := edu_years_in_level]
    dt[grepl("NPL_MICS5", survey) & edu_years_in_level == 11, educ_single_yrs := 10]     # SLC exam given at end of year 10 in 2010
    dt[grepl("NPL_MICS5", survey) & edu_years_in_level == 12, educ_single_yrs := 12]     # plus 2 are two years of education after year 10
    dt[grepl("NPL_MICS5", survey) & edu_years_in_level == 13, educ_single_yrs := 16]     # bachelor
    dt[grepl("NPL_MICS5", survey) & edu_years_in_level == 14, educ_single_yrs := 18]     # master
    dt[grepl("NPL_MICS5", survey) & edu_years_in_level == 94, educ_single_yrs := 0]      # preschool
  }
  
  # create highest ed level to be separated by years instead of country specific levels 
  dt[, highest_ed_level := ifelse(educ_single_yrs >=15, 'tertiary',
                                  ifelse(educ_single_yrs >=12, 'secondary',
                                         ifelse(educ_single_yrs >=6, 'primary', 
                                                ifelse(educ_single_yrs <6, 'less_than_primary', NA))))]
  
  # if ethnicity is NA, replace with ethnicity_hh
  if(all(c("ethnicity","ethnicity_hh") %in% names(dt))) dt[is.na(ethnicity), ethnicity := ethnicity_hh]
  
  # additional variables
  dt[, had_intercourse := ifelse(never_had_intercourse == 1, 0, ifelse(never_had_intercourse == 0, 1, NA))]
  dt[, method_information_index := ifelse(fp_se == 1 & fp_dealse == 1 & fp_othermethod == 1, 1, 0)]
  dt[, cpr := any_contra]
  dt[, sex_activity_last_4_weeks := ifelse(rec_sex_activity == "Active in last 4 weeks", 1, 0)]
  
  # save dataset
  write.csv(dt, file.path(out.dir, paste0(cur_country, "_all_yrs_prepped.csv")), row.names = F)
  
  
  # BASELINE/ENDLINE FILES ----------------------------------------
  
  # output separate CSV containing just the baseline and endline surveys
  baseline_endline <- T
  
  # if baseline_endline, update out_dir and subset files
  if (baseline_endline == T) {
    
    # ONLY DHS SURVEYS
    
    # subset surveys
    if (cur_country == "rw") dt_phase1 <- dt[grepl("DHS4_2000|DHS8_2019", survey)]
    if (cur_country == "gh") dt_phase1 <- dt[grepl("DHS4_2003|DHS8_2022", survey)]
    if (cur_country == "np") dt_phase1 <- dt[grepl("DHS5_2006|DHS8_2022", survey)]
    if (cur_country == "mw") dt_phase1 <- dt[grepl("DHS4_2000|DHS7_2015", survey)]
    if (cur_country == "cm") dt_phase1 <- dt[grepl("DHS4_2004|DHS7_2018", survey)]
    
    # replace wealth_quintiles and wealth_scores with baseline/endline calculated values
    dt_phase1 <- dt_phase1[, -c("wealth_quintiles", "wealth_scores"), with = F]
    
    # read in calculated wealth quintile values based only on baseline/endline surveys
    wq_files <- list.files("/mnt/share/scratch/projects/hssa/asher/data/01_processed/wealth_quintile_construction/baseline_endline", full.names = T)
    wq_file <- wq_files[grepl(cur_country, wq_files) & grepl("estimates\\.csv", wq_files)]
    dt_wq <- fread(wq_file)
    
    # merge onto main dataset
    dt_phase1 <- merge(dt_phase1, dt_wq[,-c("wscore","windex5"),with=F], by = c("survey","hhid_unique"), all.x = T)
    
    # prep variables and rename
    dt_phase1[, had_intercourse := ifelse(never_had_intercourse == 1, 0, ifelse(never_had_intercourse == 0, 1, NA))]
    dt_phase1[, method_information_index := ifelse(fp_se == 1 & fp_dealse == 1 & fp_othermethod == 1, 1, 0)]
    dt_phase1[, cpr := any_contra]
    dt_phase1[, sex_activity_last_4_weeks := ifelse(rec_sex_activity == "Active in last 4 weeks", 1, 0)]
    
    # output
    write.csv(dt_phase1, file.path(out.dir, "baseline_endline", paste0(cur_country, "_baseline_endline.csv")), row.names = F)
    
    # DHS AND MICS SURVEYS
    
    # subset surveys
    if (cur_country == "mw") {
      dt_phase1 <- dt[grepl("DHS4_2000|MICS6_2019", survey)]
      
      # replace wealth_quintiles and wealth_scores with baseline/endline calculated values
      dt_phase1 <- dt_phase1[, -c("wealth_quintiles", "wealth_scores"), with = F]
      
      # read in calculated wealth quintile values based only on baseline/endline surveys
      wq_files <- list.files("/mnt/share/scratch/projects/hssa/asher/data/01_processed/wealth_quintile_construction/baseline_endline", full.names = T)
      wq_file <- wq_files[grepl(cur_country, wq_files) & grepl("estimates\\.csv", wq_files)]
      dt_wq <- fread(wq_file)
      
      # merge onto main dataset
      dt_phase1 <- merge(dt_phase1, dt_wq[,-c("wscore","windex5"),with=F], by = c("survey","hhid_unique"), all.x = T)
      
      # prep variables and rename
      dt_phase1[, had_intercourse := ifelse(never_had_intercourse == 1, 0, ifelse(never_had_intercourse == 0, 1, NA))]
      dt_phase1[, method_information_index := ifelse(fp_se == 1 & fp_dealse == 1 & fp_othermethod == 1, 1, 0)]
      dt_phase1[, cpr := any_contra]
      dt_phase1[, sex_activity_last_4_weeks := ifelse(rec_sex_activity == "Active in last 4 weeks", 1, 0)]
      
      # output
      write.csv(dt_phase1, file.path(out.dir, "baseline_endline", paste0(cur_country, "_baseline_endline_mics.csv")), row.names = F)
    } 
  }
  
  
  # DATA AVAILABILITY ---------------------------------------------
  
  # variables of interest for modeling
  possible_vars <- c("survey","wpweight","urban","region","religion_recode","ethnicity","ethnicity_recode","wealth_quintiles",
                     "attend_school","educ_single_yrs","highest_ed_level","mean_yrs_schooling_head",
                     "curr_preg","ever_birth","any_births_2_yr","births_3_yrs","births_5_yrs","ever_term","number_births",
                     "any_birth_preg","ever_preg","age","age_1st_birth","curr_cohabit","ever_cohabit","age_first_cohabit",
                     "no_read","age_1st_sex_imp","never_had_intercourse","rec_sex_activity","gap_sex_mar","desire_child_teen",
                     "desire_spacing","desire_limiting","desire_later","any_contra","mod_contra","trad_contra",
                     "current_method","never_used_contra","mcpr","need_contra","unmet_need","unmet_need_mod","demand_satisfied",
                     "beating_just","ideal_child","ov_cycle_correct","method_information_index",
                     "employed","fp_exp_media","decision_use_joint_respondent","condom_ed","knowledge_mod",
                     "contra_source_public","contra_source_priv","contra_source_other","fp_exp_media",
                     "fp_hcw12m","fp_facility12m","get_condom","sex_cash","refuse_sex","ask_condom","sexual_violence","sex_partners"
                     )
  
  # subset data to columns of interest
  indicators <- names(dt)[names(dt) %in% possible_vars]
  data_test <- dt[, ..indicators]
  
  # check data availability (all ages)
  data_avail_test <- data_test[, lapply(.SD, function(x) sum(!is.na(x))/.N), by = "survey"]
  data_avail_test <- dcast(melt(data_avail_test, id.vars = "survey"), formula = variable ~ survey)
  write.csv(data_avail_test, file.path(out.dir_diagnostics, paste0(cur_country, "_data_availability_model_vars.csv")), row.names = F)
  
  pdf(file.path(out.dir_diagnostics, paste0(cur_country, "_heatmap_data_availability.pdf")), w=14, h=4, onefile = T)
  
  # visualize data availability heat map
  plot_dt <- melt(data_avail_test, id.vars = "variable", variable.name = "survey") %>% as.data.table
  plot_dt <- plot_dt[variable %ni% c("wpweight","age")]
  plot_dt[, variable := factor(variable, levels = indicators[plot_dt$variable %in% indicators])]
  
  gg <- ggplot(data = plot_dt, aes(x = variable, y = survey, fill = value)) +
    geom_tile(color = "white", lwd = .4) +
    scale_fill_distiller(palette = "RdYlGn", labels = percent, direction = 1) +
    scale_x_discrete(position = "top", labels = wrap_format(20)) +
    scale_y_discrete(limits = rev, labels = wrap_format(15)) +
    labs(fill = "Availability", title = "15-49") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 0, size = 11), axis.title = element_blank(),
                            axis.text.y = element_text(hjust = 1, size = 11), 
                            axis.ticks = element_blank(), panel.grid.major = element_blank()) +
    coord_fixed()
  print(gg)
  ggsave(file.path(out.dir_diagnostics, paste0(cur_country, "_heatmap_data_availability_15_49.png")), w=14, h=4)
 
  
  # check data availability (15-19)
  data_avail_test <- data_test[age %in% 15:19, lapply(.SD, function(x) sum(!is.na(x))/.N), by = "survey"]
  data_avail_test <- dcast(melt(data_avail_test, id.vars = "survey"), formula = variable ~ survey)
  write.csv(data_avail_test, file.path(out.dir_diagnostics, paste0(cur_country, "_data_availability_model_vars_15_19.csv")), row.names = F)
  
  # visualize data availability heat map
  plot_dt <- melt(data_avail_test, id.vars = "variable", variable.name = "survey") %>% as.data.table
  plot_dt <- plot_dt[variable %ni% c("wpweight","age")]
  plot_dt[, variable := factor(variable, levels = indicators[plot_dt$variable %in% indicators])]
  
  gg <- ggplot(data = plot_dt, aes(x = variable, y = survey, fill = value)) +
    geom_tile(color = "white", lwd = .4) +
    scale_fill_distiller(palette = "RdYlGn", labels = percent, direction = 1) +
    scale_x_discrete(position = "top", labels = wrap_format(20)) +
    scale_y_discrete(limits = rev, labels = wrap_format(15)) +
    labs(fill = "Availability", title = "15-19") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 0, size = 11), axis.title = element_blank(),
                            axis.text.y = element_text(hjust = 1, size = 11), 
                            axis.ticks = element_blank(), panel.grid.major = element_blank()) +
    coord_fixed()
  print(gg)
  ggsave(file.path(out.dir_diagnostics, paste0(cur_country, "_heatmap_data_availability_15_19.png")), w=14, h=4)
  
  
  # check data availability (15-24)
  data_avail_test <- data_test[age %in% 15:24, lapply(.SD, function(x) sum(!is.na(x))/.N), by = "survey"]
  data_avail_test <- dcast(melt(data_avail_test, id.vars = "survey"), formula = variable ~ survey)
  write.csv(data_avail_test, file.path(out.dir_diagnostics, paste0(cur_country, "_data_availability_model_vars_15_24.csv")), row.names = F)
  
  # visualize data availability heat map
  plot_dt <- melt(data_avail_test, id.vars = "variable", variable.name = "survey") %>% as.data.table
  plot_dt <- plot_dt[variable %ni% c("wpweight","age")]
  plot_dt[, variable := factor(variable, levels = indicators[plot_dt$variable %in% indicators])]
  
  gg <- ggplot(data = plot_dt, aes(x = variable, y = survey, fill = value)) +
    geom_tile(color = "white", lwd = .4) +
    scale_fill_distiller(palette = "RdYlGn", labels = percent, direction = 1) +
    scale_x_discrete(position = "top", labels = wrap_format(20)) +
    scale_y_discrete(limits = rev, labels = wrap_format(15)) +
    labs(fill = "Availability", title = "15-24") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 0, size = 11), axis.title = element_blank(),
                            axis.text.y = element_text(hjust = 1, size = 11), 
                            axis.ticks = element_blank(), panel.grid.major = element_blank()) +
    coord_fixed()
  print(gg)
  ggsave(file.path(out.dir_diagnostics, paste0(cur_country, "_heatmap_data_availability_15_24.png")), w=14, h=4)
  
  dev.off()
}

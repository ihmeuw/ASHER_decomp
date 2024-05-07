#-------------------Header------------------------------------------------
# Author: Olivia Angelino
# Team: ASHER Decomp
# Purpose: Prep merged data for modeling, create data diagnostics
# Date: 2/16/2024
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
pacman::p_load(data.table,magrittr,tidyverse,parallel,plyr,dplyr,haven,survey,tools,devtools)

# in/out
in.dir <- '/share/scratch/projects/hssa/asher/data/02_merged/'
out.dir <- '/share/scratch/projects/hssa/asher/data/03_prepped/'
out.dir_diagnostics <- '/share/scratch/projects/hssa/asher/data/data_diagnostics/'

# create function for the opposite of %in%
'%ni%' <- Negate('%in%')


# PREP DATA -------------------------------------------------------

# countries to prep files for
countries <- c("gh", "mw", "np", "rw", "cm")

cur_country <- "cm"

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
  
  # process religion
  # want to capture main groups, put smaller groups into other/none
  dt[, religion := tolower(religion)]
  
  if (cur_country == "gh") {
    dt[grepl("catholic", religion), religion_recode := "Christian: Catholic"]
    dt[grepl("protestant|methodist|deeper life|pentecostal|presby|jehovah|sda|other christ|anglican", religion), religion_recode := "Christian: Non-Catholic"]
    dt[grepl("islam|moslem", religion), religion_recode := "Muslim"]
    dt[grepl("traditional", religion), religion_recode := "Traditional"]
    dt[grepl("no religion|other rel|^other$|spiritualist", religion), religion_recode := "Other/None"]
    dt[grepl("no response|99", religion) | is.na(religion), religion_recode := "No response"]
    message(paste0("religion responses not captured: ", paste(unique(dt[is.na(religion_recode)]$religion), collapse = ", ")))
    
    dt[religion_recode == "No response", religion_recode := NA]
  } else if (cur_country == "np") {
    dt[grepl("christian", religion), religion_recode := "Christian"]
    dt[grepl("hindu|prakriti", religion), religion_recode := "Hindu"]
    dt[grepl("buddhist", religion), religion_recode := "Buddhist"]
    dt[grepl("islam|mulsim|muslim", religion), religion_recode := "Muslim"]
    dt[grepl("kirat|bon", religion), religion_recode := "Traditional"]
    dt[grepl("other relig|no relig|others|sikh|jain|^other$", religion), religion_recode := "Other/None"]
    dt[is.na(religion), religion_recode := "No response"]
    message(paste0("religion responses not captured: ", paste(unique(dt[is.na(religion_recode)]$religion), collapse = ", ")))
    
    dt[religion_recode == "No response", religion_recode := NA]
  } else if (cur_country == "cm") {
    dt[grepl("catholi", religion), religion_recode := "Christian: Catholic"]
    dt[grepl("pros*testant|other christ|autre chr", religion), religion_recode := "Christian: Non-Catholic"]
    dt[grepl("moslem|muslim|musulman", religion), religion_recode := "Muslim"]
    dt[grepl("animist", religion), religion_recode := "Traditional"]
    dt[grepl("^other$|no relig|autre r|pas de relig|none|new relig", religion), religion_recode := "Other/None"]
    dt[grepl("manquant", religion) | is.na(religion), religion_recode := "No response"]
    message(paste0("religion responses not captured: ", paste(unique(dt[is.na(religion_recode)]$religion), collapse = ", ")))
    
    dt[religion_recode == "No response", religion_recode := NA]
  } else if (cur_country == "mw") {
    dt[grepl("catholic|seventh day|anglican|ccap|other christ|christianity", religion), religion_recode := "Christian"]  # can't split bc MICS 2019-2020
    dt[grepl("islam|muslim", religion), religion_recode := "Muslim"]
    dt[grepl("traditional", religion), religion_recode := "Traditional"]
    dt[grepl("no religion|^other$|other relig|hindu|buddhism", religion), religion_recode := "Other/None"]
    dt[grepl("no response|missing|99", religion) | is.na(religion), religion_recode := "No response"]
    message(paste0("religion responses not captured: ", paste(unique(dt[is.na(religion_recode)]$religion), collapse = ", ")))
    
    dt[religion_recode == "No response", religion_recode := NA]
  } else if (cur_country == "rw") {
    dt[grepl("catholic", religion), religion_recode := "Christian: Catholic"]
    dt[grepl("adventist|jehovah|protestant|7e jour", religion), religion_recode := "Christian: Non-Catholic"]
    dt[grepl("muslim", religion), religion_recode := "Muslim"]
    dt[grepl("no religion|^other$|none|traditional", religion), religion_recode := "Other/None"]
    dt[grepl("99", religion) | is.na(religion), religion_recode := "No response"]
    message(paste0("religion responses not captured: ", paste(unique(dt[is.na(religion_recode)]$religion), collapse = ", ")))
    
    dt[religion_recode == "No response", religion_recode := NA]
  }
  
  # process ethnicity
  # want to capture main groups, put smaller groups into other/none
  dt[, ethnicity := tolower(ethnicity)]
  
  if (cur_country == "gh") {
    dt[grepl("akan", ethnicity), ethnicity_recode := "Akan"]
    dt[grepl("damgme|dangme", ethnicity), ethnicity_recode := "Ga/Dangme"]
    dt[grepl("ewe", ethnicity), ethnicity_recode := "Ewe"]
    dt[grepl("guan", ethnicity), ethnicity_recode := "Guan"]
    dt[grepl("mole.dagbani", ethnicity), ethnicity_recode := "Mole-Dagbani"]
    dt[grepl("grus+i", ethnicity), ethnicity_recode := "Grusi"]
    dt[grepl("gurma|gruma", ethnicity), ethnicity_recode := "Gurma"]
    dt[grepl("mande", ethnicity), ethnicity_recode := "Mande"]
    dt[grepl("other eth|others|^dk$|non-ghana|^other$|hausa", ethnicity), ethnicity_recode := "Other"]
    dt[grepl("no response|99|missing", ethnicity) | is.na(ethnicity), ethnicity_recode := "No response"]
    message(paste0("ethnicity responses not captured: ", paste(unique(dt[is.na(ethnicity_recode)]$ethnicity), collapse = ", ")))
    
    dt[ethnicity_recode == "No response", ethnicity_recode := NA]
  } else if (cur_country == "np") {
    
    # Nepal has a caste/ethnicity system, TONS of options, definitely need collaborator input
    dt[, ethnicity_recode := ethnicity]
    
  } else if (cur_country == "cm") {
    # TONS of options, definitely need collaborator input
    dt[grepl("arabe.choa|peulh|haoussa|kanuri", ethnicity), ethnicity_recode := "Arabes-Choa/Peulh/Haoussa/Kanuri"]
    dt[grepl("b(iu|ui).mandara", ethnicity), ethnicity_recode := "Biu-Mandara"]
    dt[grepl("adamaoua.oubangui", ethnicity), ethnicity_recode := "Adamaoua-Oubangui"]
    dt[grepl("bantoide|south-west", ethnicity), ethnicity_recode := "Bantoide Sud-Ouest"]
    dt[grepl("grassfield", ethnicity), ethnicity_recode := "Grassfields"]
    dt[grepl("bamil(e|i)ke|bamoun", ethnicity), ethnicity_recode := "Bamilike/Bamoun"]
    dt[grepl("oroko", ethnicity), ethnicity_recode := "Cotier/Ngoe/Oroko"]
    dt[grepl("beti|bassa|mbam", ethnicity), ethnicity_recode := "Beti/Bassa/Mbam"]
    dt[grepl("kako|meka|pygme", ethnicity), ethnicity_recode := "Kako/Meka/Pygme"]
    dt[grepl("stranger|other|^dk$|autre|etranger", ethnicity), ethnicity_recode := "Foreign/Other"]
    dt[grepl("no response|99|manquant", ethnicity) | is.na(ethnicity), ethnicity_recode := "No response"]
    message(paste0("ethnicity responses not captured: ", paste(unique(dt[is.na(ethnicity_recode)]$ethnicity), collapse = ", ")))
    
    dt[ethnicity_recode == "No response", ethnicity_recode := NA]
  } else if (cur_country == "mw") {
    dt[grepl("", ethnicity), ethnicity_recode := ""]
    dt[grepl("", ethnicity), ethnicity_recode := ""]
    dt[grepl("no ethnicity", ethnicity), ethnicity_recode := "Other"]
    dt[grepl("no response|99", ethnicity) | is.na(ethnicity), ethnicity_recode := "No response"]
    message(paste0("ethnicity responses not captured: ", paste(unique(dt[is.na(ethnicity_recode)]$ethnicity), collapse = ", ")))
    
    dt[ethnicity_recode == "No response", ethnicity_recode := NA]
  } else if (cur_country == "rw") {
    dt[grepl("", ethnicity), ethnicity_recode := ""]
    dt[grepl("", ethnicity), ethnicity_recode := ""]
    dt[grepl("no ethnicity", ethnicity), ethnicity_recode := "Other"]
    dt[grepl("no response|99", ethnicity) | is.na(ethnicity), ethnicity_recode := "No response"]
    message(paste0("ethnicity responses not captured: ", paste(unique(dt[is.na(ethnicity_recode)]$ethnicity), collapse = ", ")))
    
    dt[ethnicity_recode == "No response", ethnicity_recode := NA]
  }
  
  dt[, had_intercourse := ifelse(never_had_intercourse == 1, 0, ifelse(never_had_intercourse == 0, 1, NA))]
  dt[, method_information_index := ifelse(fp_se == 1 & fp_dealse == 1 & fp_othermethod == 1, 1, 0)]
  dt[, cpr := any_contra]
  dt[, sex_activity_last_4_weeks := ifelse(rec_sex_activity == "Active in last 4 weeks", 1, 0)]
  
  # save dataset
  write.csv(dt, file.path(out.dir, paste0(cur_country, "_all_yrs_prepped.csv")), row.names = F)
  
  
  # BASELINE/ENDLINE FILES ----------------------------------------
  
  # output separate CSV containing just the baseline and endline surveys for Corinne
  baseline_endline <- T
  
  # if baseline_endline, update out_dir and subset files
  if (baseline_endline == T) {
    
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
  
  
  # DATA TRENDS -----------------------------------------------------
  
  # create survey_year for plotting purposes
  dt[, survey_year := as.numeric(str_extract(survey, "[0-9]{4}"))]
  
  pdf(file.path(out.dir_diagnostics, paste0(cur_country, "_indicator_trends_15_49.pdf")), width = 10, height = 8, onefile = T)
  
  # loop through each available indicator and plot
  for (indic in indicators) {
    
    if (indic %in% c("survey","wpweight","age")) next()
    
    if (indic %in% c("region","religion_recode","ethnicity","ethnicity_recode","rec_sex_activity","highest_ed_level","current_method")) {
      # categorical
      
      dt[, (indic) := as.character(get(indic))]
      
      # get weighted proportion of each category
      plot_dt <- dt %>%
        group_by(survey, survey_year, !!sym(indic)) %>%                # Group by the columns of interest
        dplyr::summarise(count = sum(pweight), .groups = "keep") %>%   # Count the number of occurrences in each group
        group_by(survey, survey_year) %>%
        dplyr::mutate(total = sum(count),                              # Calculate the total count for each group
                      proportion = count / total) %>%                  # Calculate the proportion
        as.data.table
      plot_dt[, survey_group := str_extract(survey, "DHS|MICS")]
      
      # plot
      gg <- ggplot(data = plot_dt, aes(x = get(indic), y = proportion, group = survey_year, fill = reorder(survey, survey_year))) +
        geom_bar(position = "dodge", stat = "identity") +
        scale_y_continuous(labels = percent) +
        labs(title = indic,
             x = "Response",
             y = "Proportion",
             fill = "Survey Series") +
        theme_bw() + facet_wrap(~get(indic), scales = "free")
      print(gg) 
      
    } else if (indic %in% c("number_births","age_1st_birth","age_first_cohabit","age_1st_sex_imp","gap_sex_mar",
                            "ideal_child","knowledge_mod","mean_yrs_schooling_head","educ_single_yrs","mean_yrs_schooling_head",
                            "wealth_quintiles","sex_partners")) {
      # continuous
      
      # take weighted mean by survey
      plot_dt <- dt[, .(indicator=indic,
                        value=weighted.mean(get(indic), w = pweight, na.rm = T),
                        N = .N),
                    .(survey_year,survey)]
      plot_dt[, survey_group := str_extract(survey, "DHS|MICS")]
      
      # plot
      gg <- ggplot(data = plot_dt, aes(x = reorder(survey, survey_year), y = value, fill = survey_group)) +
        geom_bar(stat = "identity") +
        labs(title = indic,
             x = "Survey",
             y = "Value",
             fill = "Survey Series") +
        theme_bw()
      print(gg) 
      
    } else {
      # proportion
      
      # take weighted mean by survey
      plot_dt <- dt[, .(indicator=indic,
                        value=weighted.mean(get(indic), w = pweight, na.rm = T),
                        N = .N),
                    .(survey_year,survey)]
      plot_dt[, survey_group := str_extract(survey, "DHS|MICS")]
      
      # plot
      gg <- ggplot(data = plot_dt, aes(x = reorder(survey, survey_year), y = value, fill = survey_group)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(labels = percent) +
        labs(title = indic,
             x = "Survey",
             y = "Value",
             fill = "Survey Series") +
        theme_bw()
      print(gg) 
    }
  }
  
  dev.off()
  
  
  
  
  pdf(file.path(out.dir_diagnostics, paste0(cur_country, "_indicator_trends_15_19.pdf")), width = 10, height = 8, onefile = T)
  
  # loop through each available indicator and plot
  for (indic in indicators) {
    
    if (indic %in% c("survey","wpweight","age")) next()
    
    if (indic %in% c("region","religion_recode","ethnicity","ethnicity_recode","rec_sex_activity","highest_ed_level","current_method")) {
      # categorical
      
      dt[, (indic) := as.character(get(indic))]
      
      # get weighted proportion of each category
      plot_dt <- dt[age %in% 15:19] %>%
        group_by(survey, survey_year, !!sym(indic)) %>%                # Group by the columns of interest
        dplyr::summarise(count = sum(pweight), .groups = "keep") %>%   # Count the number of occurrences in each group
        group_by(survey, survey_year) %>%
        dplyr::mutate(total = sum(count),                              # Calculate the total count for each group
                      proportion = count / total) %>%                  # Calculate the proportion
        as.data.table
      plot_dt[, survey_group := str_extract(survey, "DHS|MICS")]
      
      # plot
      gg <- ggplot(data = plot_dt, aes(x = get(indic), y = proportion, group = survey_year, fill = reorder(survey, survey_year))) +
        geom_bar(position = "dodge", stat = "identity") +
        scale_y_continuous(labels = percent) +
        labs(title = indic,
             x = "Response",
             y = "Proportion",
             fill = "Survey Series") +
        theme_bw() + facet_wrap(~get(indic), scales = "free")
      print(gg) 
      
    } else if (indic %in% c("number_births","age_1st_birth","age_first_cohabit","age_1st_sex_imp","gap_sex_mar",
                            "ideal_child","knowledge_mod","educ_single_yrs","mean_yrs_schooling_head",
                            "wealth_quintiles","sex_partners")) {
      # continuous
      
      # take weighted mean by survey
      plot_dt <- dt[age %in% 15:19, .(indicator=indic,
                        value=weighted.mean(get(indic), w = pweight, na.rm = T),
                        N = .N),
                    .(survey_year,survey)]
      plot_dt[, survey_group := str_extract(survey, "DHS|MICS")]
      
      # plot
      gg <- ggplot(data = plot_dt, aes(x = reorder(survey, survey_year), y = value, fill = survey_group)) +
        geom_bar(stat = "identity") +
        labs(title = indic,
             x = "Survey",
             y = "Value",
             fill = "Survey Series") +
        theme_bw()
      print(gg) 
      
    } else {
      # proportion
      
      # take weighted mean by survey
      plot_dt <- dt[age %in% 15:19, .(indicator=indic,
                        value=weighted.mean(get(indic), w = pweight, na.rm = T),
                        N = .N),
                    .(survey_year,survey)]
      plot_dt[, survey_group := str_extract(survey, "DHS|MICS")]
      
      # plot
      gg <- ggplot(data = plot_dt, aes(x = reorder(survey, survey_year), y = value, fill = survey_group)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(labels = percent) +
        labs(title = indic,
             x = "Survey",
             y = "Value",
             fill = "Survey Series") +
        theme_bw()
      print(gg) 
    }
  }
  
  dev.off()
}



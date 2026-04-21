#-------------------Header------------------------------------------------
# Author: NAME
# Project: ASHER Decomp
# Purpose: Run crude decomp using log hazard ratios from cox ph model 
# Notes:
#***************************************************************************

# load packages
pacman::p_load(data.table,tidyverse,dplyr)

# in/out
in.date <- 'DATE'
in.date.cohort <- 'DATE'
cohort.dir <- 'FILEPATH'
cohort.dir <- file.path(cohort.dir, in.date.cohort)
data_dir <- file.path('FILEPATH', in.date)

contra_type <- 'current' # pre_partum or current

# cleaned variable names
var_names <- c("urban" = "Urban",
               "as_factor(windex5)2" = "Wealth 2nd quintile",
               "as_factor(windex5)3" = "Wealth 3rd quintile",
               "as_factor(windex5)4" = "Wealth 4th quintile",
               "as_factor(windex5)5" = "Wealth 5th quintile",
               "as_factor(wealth_quintiles)2" = "Wealth",
               "as_factor(wealth_quintiles)3" = "Wealth",
               "as_factor(wealth_quintiles)4" = "Wealth",
               "as_factor(wealth_quintiles)5" = "Wealth",
               "mean_yrs_schooling_head" = "HH head educ yrs",
               "knowledge_mod" = "# modern methods known",
               "curr_cohabit_timevary" = "Marital status",
               "ever_had_intercourse_timevary" = "Has had intercourse",
               "ever_had_intercourse_timevary_unmarried" = "Has had intercourse (unmarried)",
               "ed_levelCurrently attending" = "Attending school",
               "ed_levelNot attending: completed primary" = "Not attending: completed primary",            
               "ed_levelNot attending: completed secondary" = "Not attending: completed secondary", 
               "ed_levelNot attending: some primary" = "Not attending: some primary",
               "ed_levelNot attending: some secondary" = "Not attending: some secondary",
               "ed_levelNo education" = "No education",
               "ed_level_customCurrently attending" = "Attending school",
               "ed_level_customNot attending: completed primary" = "Not attending: completed primary",            
               "ed_level_customNot attending: completed secondary" = "Not attending: completed secondary", 
               "ed_level_customNot attending: some primary" = "Not attending: some primary",
               "ed_level_customNot attending: some secondary" = "Not attending: some secondary",
               "educ_yrs_timevary" = "Educ yrs completed",
               "attend_school_timevary" = "Attending school",
               "mod_contra" = "Modern contraceptive prevalence",
               "mod_contra_lag_1m" = "Mod contra, lag 1 mo",
               "mod_contra_past_2m" = "Mod contra, past 2 months",
               "year" = "Calendar Year",
               "beating_just" = "Believes beating is justified",
               "wants_children" = "Wants children",
               "age_1st_sex_imp" = "Age at first sex",
               "decision_use_joint_respondent_timevary_np_2022" = "Self/joint decision re:contra",
               "mod_contra_inc_fail" = "Pre-partum modern contraceptive use",
               "as.factor(current_contra_type)Modern method" = "Modern method",     
               "as.factor(current_contra_type)Traditional method" = "Trad method",
               "as.factor(current_contra_subtype)Modern Long-acting" = "Modern Long-acting",    
               "as.factor(current_contra_subtype)Modern Short-acting" = "Modern Short-acting",
               "as.factor(current_contra_subtype)Traditional" = "Traditional")

# function to run decomp and plot based on contraception type (constructed pre-partum or current) -------
run_decomp <- function(contra_type){
   # pretty country labels 
  pretty_country_labs <- data.table(country = c( "cm", "rw" , "gh" , "mw" , "np"),
                                    pretty_country_lab = c("Cameroon", "Rwanda","Ghana","Malawi", "Nepal"))
  
  # PREP DATA ----------------------------------------------------------------
  
  # final models for country-specific presentations
  
  # list files in 04_model_cohort
  files <- list.files(cohort.dir)
  
  # subset to files with model coefficients
  files <- files[grepl("_models_coefs", files)]
  
  # read in and combine for all the countries
  model_coefs_final <- rbindlist(lapply(file.path(cohort.dir, files), fread), fill = T)
  model_coefs_final <- model_coefs_final[grepl(", categorical education", model)]
  
  # rename columns
  setnames(model_coefs_final, c("rn", "coef", "Pr(>|z|)"), c("var", "val", "p_val"))
  
  # T/F if significant at the 0.05 level
  model_coefs_final[, signif := ifelse(p_val < .05, T, F)]
  
  # identify direction of coefficient
  model_coefs_final[val < 0, group := "Negative"]
  model_coefs_final[val > 0, group := "Positive"]
  
  # construct confidence intervals
  model_coefs_final[, conf_high := val + (1.96 * `robust se`)]
  model_coefs_final[, conf_low := val - (1.96 * `robust se`)]
  
  ## BASE MODEL -----------------------------------------------------------
  
  # base model
  model_coefs_base <- model_coefs_final[model == "no contra lag, inc method failure, categorical education"]
  model_coefs_base[, country := str_sub(data, 1, 3)]
  model_coefs_base[, direction := ifelse(val > 0, "Positive", "Negative")]
  
  ## PREP DATA FOR CRUDE DECOMP ------------------------------------------
  # subset to columns of interest
  decomp_dt <- model_coefs_base[, c("country", "var", "exp(coef)", "signif", "direction")]
  
  # convert hazard ratios to probability: HR = P/(1-P); P = HR/(1+HR)
  decomp_dt[, prob := `exp(coef)` / (1 + `exp(coef)`)]
  decomp_dt[, prob := ifelse(direction == "Negative", prob * -1, prob)]
  
  # merge on change in values over time
  all_plot_dt <- data.table()
  outcome_dt <- data.table()
  age_group <- "15_19"
  
  for (cur_country_data in c("mw", "np", "rw", "gh")){
    diff_df <- fread(file.path(data_dir,  paste0(paste("diff_means", cur_country_data, "dhs", age_group,"outcome", "c", 'mod_contra_imputed', sep = "_"),".csv") ))
    
    if (contra_type == "pre_partum"){
      mod_contra_dt <- fread(file.path(data_dir,  paste0(paste("diff_means", cur_country_data, "dhs", age_group,"outcome", "c", 'mod_contra_imputed', sep = "_"),".csv") ))
      mod_contra_dt <- mod_contra_dt[variable == 'mod_contra_outcome_c']
      diff_df <- rbind(diff_df, mod_contra_dt)
    }
    # need to use outcome c because outcome b slightly increases in ghana
    outcome_tmp <- fread(file.path(data_dir,  paste0(paste("diff_means", cur_country_data, "dhs", age_group,"outcome_c", 'mod_contra_imputed', sep = "_"),".csv") ))
    outcome_tmp <- outcome_tmp[variable == 'outcome_c_dhs', c("variable", "country", "mean_diff")]
    outcome_tmp[, mean_diff := mean_diff * 100]
    outcome_dt <- rbind(outcome_dt, outcome_tmp)
    if (contra_type == "pre_partum"){
      
      diff_df <- diff_df[variable %in% c("urban", "mean_yrs_schooling_head", "beating_just", "wealth_dummies1",
                                         "wealth_dummies2","wealth_dummies3","wealth_dummies4","wealth_dummies5",
                                         "curr_cohabit", "had_intercourse_unmarried", "mod_contra_outcome_c",
                                         "not_attend_less_secondary", "not_attend_completed_secondary", "currently_attending")]
    } else{
      diff_df <- diff_df[variable %in% c("urban", "mean_yrs_schooling_head", "beating_just", "wealth_dummies1",
                                         "wealth_dummies2","wealth_dummies3","wealth_dummies4","wealth_dummies5",
                                         "curr_cohabit", "had_intercourse_unmarried",
                                         "not_attend_less_secondary", "not_attend_completed_secondary", "currently_attending", "mod_contra")]
      
    }
    
    
    all_plot_dt <- rbind(all_plot_dt, diff_df[, c("variable", "country", "mean_diff")])
  }
  
  if (contra_type == "pre_partum"){
    # rename variables to match
    var_names_merge <- list("curr_cohabit" = "curr_cohabit_timevary",
                            "not_attend_less_secondary"="not_attend_less_secondary",
                            "wealth_dummies1"="wealth_dummies1",
                            "wealth_dummies2"="as_factor(wealth_quintiles)2", 
                            "wealth_dummies3" = "as_factor(wealth_quintiles)3",
                            "wealth_dummies4" = "as_factor(wealth_quintiles)4",
                            "wealth_dummies5" = "as_factor(wealth_quintiles)5",
                            "had_intercourse_unmarried" = "ever_had_intercourse_timevary_unmarried",
                            "not_attend_completed_secondary" = "ed_levelNot attending: completed secondary",
                            "currently_attending" = "ed_levelCurrently attending",
                            "not_attend_less_secondary"="not_attend_less_secondary",
                            "mod_contra_outcome_a" = "mod_contra_inc_fail",
                            "mod_contra_outcome_b" = "mod_contra_inc_fail",
                            "mod_contra_outcome_c" = "mod_contra_inc_fail",
                            "urban" = "urban",
                            "beating_just" = "beating_just",
                            "mean_yrs_schooling_head" = "mean_yrs_schooling_head")
  } else{
    # rename variables to match
    var_names_merge <- list("curr_cohabit" = "curr_cohabit_timevary",
                            "not_attend_less_secondary"="not_attend_less_secondary",
                            "wealth_dummies1"="wealth_dummies1",
                            "wealth_dummies2"="as_factor(wealth_quintiles)2", 
                            "wealth_dummies3" = "as_factor(wealth_quintiles)3",
                            "wealth_dummies4" = "as_factor(wealth_quintiles)4",
                            "wealth_dummies5" = "as_factor(wealth_quintiles)5",
                            "had_intercourse_unmarried" = "ever_had_intercourse_timevary_unmarried",
                            "not_attend_completed_secondary" = "ed_levelNot attending: completed secondary",
                            "currently_attending" = "ed_levelCurrently attending",
                            "not_attend_less_secondary"="not_attend_less_secondary",
                            "mod_contra" = "mod_contra_inc_fail",
                            "urban" = "urban",
                            "beating_just" = "beating_just",
                            "mean_yrs_schooling_head" = "mean_yrs_schooling_head")
  }
  
  all_plot_dt[, lab := unlist(var_names_merge[variable])]
  
  all_plot_dt <- merge(all_plot_dt, pretty_country_labs, by = c("country"))
  
  decomp_dt[, country := tolower(str_sub(country, 1, 2))]
  decomp_dt_plot <- merge(decomp_dt, pretty_country_labs, by = c("country"))
  
  ## RUN CRUDE DECOMP ------------------------------------------
  # merge on change in values over time
  setnames(decomp_dt_plot, "var", "lab")
  decomp_dt_plot <- merge(decomp_dt_plot, all_plot_dt, by = c("pretty_country_lab", "country", "lab"))
  decomp_dt_plot[, attr := prob * mean_diff]
  decomp_dt_plot[, x_lab := 1]
  
  decomp_dt_plot[, pretty_lab := unlist(var_names[lab])]
  # sum wealth (sum attr by label)
  decomp_dt_plot <- decomp_dt_plot[, .(attr = sum(attr)), by = .(pretty_country_lab, country, pretty_lab, x_lab, signif)]
  # multiply attr by 100, unless continous var ( "mean_yrs_schooling_head")
  decomp_dt_plot[pretty_lab != "HH head educ yrs", attr := attr * 100]
  
  # calculate unexplained: sum significant attributes and subtract from overall change in outcome
  explained_dt <- decomp_dt_plot[signif == T, .(explained = sum(attr)), by = .(pretty_country_lab, country)]
  # merge on outcome
  explained_dt <- merge(explained_dt, outcome_dt, by = c("pretty_country_lab" = "country"))
  # calc unexplained
  explained_dt[, unexplained := mean_diff - explained]
  unexplained_dt <- explained_dt[, .(pretty_country_lab, country, unexplained)]
  setnames(unexplained_dt, "unexplained", "attr")
  unexplained_dt[, signif := "TRUE"]
  unexplained_dt[, pretty_lab := "Unexplained"]
  unexplained_dt[, x_lab := 1]
  # add onto plot df
  decomp_dt_plot <- rbind(decomp_dt_plot, unexplained_dt)
  # order so explained is at the top 
  decomp_dt_plot[, pretty_lab := factor(pretty_lab, levels = c("Unexplained",setdiff(unique(decomp_dt_plot[signif == T]$pretty_lab), "Unexplained")))]
  
  # update pretty country lab to have overall difference in outcome below it
  explained_dt[, outcome_diff := round(mean_diff, 2)]
  decomp_dt_plot <- merge(decomp_dt_plot, explained_dt[, .(pretty_country_lab, outcome_diff)], by = c("pretty_country_lab"))
  decomp_dt_plot[, pretty_country_lab := paste0(pretty_country_lab, "\n", outcome_diff, "%")]
  ## PLOT ------------------------------------------
  # stacked bar 
  if (contra_type == "pre_partum"){
    ggplot(decomp_dt_plot[signif=="TRUE"], aes(fill=pretty_lab, x= x_lab)) + 
      facet_wrap(~pretty_country_lab, ncol=4)+
      geom_bar(position="stack", stat="identity", aes(y=attr))+
      labs(title ="Crude decomposition, outcome c",
           x = "Variable", y = "Attribution (probability * change)", fill = "Variable")+
      theme_bw()+
      scale_fill_manual(values = c("#BEBADA" ,"#FB8072", "#80B1D3", "#386CB0","#66C2A5", "#FDB462", "#B3DE69","#FFFFB3", "#F0027F", "#6A3D9A", "#D3D3D3"),
                        breaks = c("Believes beating is justified", "Marital status","Attending school", "Not attending: completed secondary", 
                                   "No education", "Has had intercourse (unmarried)", "Pre-partum modern contraceptive use", "Wealth", "Urban", 
                                   "HH head educ yrs", "Unexplained"))+
      geom_text(aes(label = ifelse(abs(attr) >=0.7, round(attr, 2), ""), y = attr, color = 'black'), position = position_stack(vjust = .5),  size = 4.5) +
      scale_color_manual(values = c(  "Black","White"))+
      scale_y_continuous(breaks = c(-20, -10,0,10), labels = c("-20%", "-10%", "0%", "10%")) +
      geom_hline(yintercept = 0, col = "black", size=1)+
      theme(plot.title = element_text(hjust = 0.5), 
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            text = element_text(size = 24),
            strip.background = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.title = element_blank(),
            plot.caption = element_text(size = 14,hjust = 0))+
      guides(color ="none")
    
    ggsave(file.path(cohort.dir, "Fairlie crude decomp pre partum mod contra.png"), width = 14, height=9.5)
    
    
  } else{
    decomp_dt_plot[pretty_lab == "Pre-partum contraceptive use", pretty_lab := "Pre-partum contraceptive use*"]
    ggplot(decomp_dt_plot[signif=="TRUE"], aes(fill=pretty_lab, x= x_lab)) +
      facet_wrap(~pretty_country_lab, ncol=4)+
      geom_bar(position="stack", stat="identity", aes(y=attr))+
      labs(x = "Variable", y = "Effect on proportion of women with any birth \n or pregancy in the last two years", fill = "Variable",
           caption = "*Using pre-partum modern contraceptive use coefficient and current modern contraceptive use change in means") +
      theme_bw()+
      scale_fill_manual(values = c("#BEBADA" ,"#FB8072", "#80B1D3", "#386CB0","#66C2A5", "#FDB462", "#B3DE69","#FFFFB3", "#F0027F", "#6A3D9A", "#D3D3D3"),
                        breaks = c("Believes beating is justified", "Marital status","Attending school", "Not attending: completed secondary",
                                   "No education", "Has had intercourse (unmarried)", "Pre-partum modern contraceptive use", "Wealth", "Urban",
                                   "HH head educ yrs", "Unexplained"))+
      geom_text(aes(label = ifelse(abs(attr) >= 0.7, round(attr, 2), ""), y = attr, color = 'black'), position = position_stack(vjust = .5),  size = 4.5) +
      scale_color_manual(values = c(  "Black","White"))+
      scale_y_continuous(breaks = c(-20,-10,0,10,20), labels = c("-20%", "-10%", "0%", "10%", "20%")) +
      geom_hline(yintercept = 0, col = "black", size=1)+
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            text = element_text(size = 28),
            strip.background = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size = 16),
            plot.caption = element_text(size = 14,hjust = 0))+
      guides(color ="none")
    
    ggsave(file.path(cohort.dir, "Fairlie crude decomp mcpr.png"), width = 15.5, height=10)

  }
  
}

# call function ------
run_decomp('pre_partum')
run_decomp('current')


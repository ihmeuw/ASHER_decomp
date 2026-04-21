#-------------------Header------------------------------------------------

## Created by: NAME
## Run oaxaca-blinder decomp for asher manually 

## set-up --------
pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey,tidyr,glmnet, ggpattern)

'%ni%' <- Negate('%in%')

# date of draw creation
in.date.draw <- 'DATE'
in.date.draw.part.2 <- 'DATE'
# date of ob run for coefficients to use 
ob.in.date <- 'DATE'
# date of data mean creation
in.date.data <- 'DATE'
in.date.coef.draw <- 'DATE'

## directories
data_dir <- file.path('FILEPATH', in.date.data)
draw_dir <-  file.path('FILEPATH', in.date.draw)
draw_dir.part.2 <-  file.path('FILEPATH', in.date.draw.part.2)
draw_dir_coef <- file.path('FILEPATH', in.date.coef.draw)
in.dir.ob <-  file.path('FILEPATH', ob.in.date)
date <- Sys.Date()
out.dir <-  file.path('FILEPATH', date)
dir.create(out.dir)
dir.create(file.path(out.dir, 'plots'))

# variable labels
variable_labs <- list("difference" = "Difference", 
                      "unexplained" = "Unexplained",
                      "explained" = "Explained",
                      "age" = "Age",
                      "age_1st_sex_imp" = "Age at first sex",
                      "beating_just" = "Believes beating is justified",
                      "curr_cohabit" = "Marital status",
                      "educ_single_yrs" = "Years of education",
                      "had_intercourse" = "Has had intercourse",
                      "curr_preg_wanted" = "Wanted pregnancy",
                      "curr_preg_unwanted" = "Unwanted pregnancy     ",
                      "had_intercourse_unmarried" = "Has had intercourse (unmarried)",
                      "unmet_need" = "Unmet need",
                      "wealth_dummies1" = "Wealth 1",
                      "wealth_dummies2" = "Wealth 2",
                      "wealth_dummies3" = "Wealth 3",
                      "wealth_dummies4" = "Wealth 4",
                      "wealth_dummies5" = "Wealth 5",
                      "wealth" = "Wealth",
                      "wealth_index" = "Wealth",
                      "beating_just" = "Believes beating is justified", 
                      'outcome_b_dhs' = "First pregnancy in the two years prior to the survey, \n including terminations, miscarriages, and stillbirths",
                      'outcome_b_mics' = "First pregnancy in the two years prior to the survey",
                      'outcome_c_dhs' = "Any pregnancy in the two years prior to the survey, \n including terminations, miscarriages, and stillbirths",
                      'outcome_c_mics' = "Any pregnancy in the two years prior to the survey",
                      "mod_contra_outcome_a" = "Prepartum modern contraception use*",
                      "mod_contra_outcome_b" = "Prepartum modern contraception use*",
                      "mod_contra_outcome_c" = "Prepartum modern contraception use*")# will put an astrix below to indicate that not available in cameroon 

pretty_country_labs <- data.table(country = c( "cm", "rw" , "gh" , "mw" , "np"),
                                  pretty_country_lab = c("Cameroon", "Rwanda","Ghana","Malawi", "Nepal"))

# create data frame to save results
run_ob_manual <- function(outcome, coef_group) {
  outcome <- tolower(outcome)
  coef_group <- tolower(coef_group)
  
  if (outcome %in% c("a", "b", "c")) {
    outcome <- paste0("outcome_", outcome)
  }
  
  if (!(outcome %in% c("outcome_a", "outcome_b", "outcome_c", "full_sensitivity_outcome_c", 'mics_outcome_c', "marriage_preg_drop_outcome_c"))) {
    stop("`outcome` must be one of: a, b, c, outcome_a, outcome_b, outcome_c, full_sensitivity_outcome_c, mics_outcome_c, marriage_preg_drop_outcome_c")
  }
  if (!(coef_group %in% c("endline", "pooled"))) {
    stop("`coef_group` must be one of: endline, pooled")
  }
  
  results_dt <- data.table()
  output_dt <- data.table()
  
  # -------- Run decomp per country ---------
  for (cur_country in c("cm", "gh", "mw", "np", "rw")){
  
    country_results <- data.table()
    share_dt <- data.table()
    
    mode <- ifelse(cur_country != "mw", "dhs", "mics")

    # read in draws
    if (outcome == 'mics_outcome_c'){ # same as outcome c because don't use outcome draws and same amount of missingness between the two outcomes 
      draws_dt <- data.table(fread(file.path(draw_dir, paste0(cur_country, '_outcome_c_draws.csv'))))
    } else{
      draws_dt <- data.table(fread(file.path(draw_dir, paste0(cur_country, '_', outcome, '_draws.csv'))))
    }
    draws_dt <- draws_dt[, .SD, .SDcols = sort(names(draws_dt))]
    
    # read in mean difference for non-outcome variables
    if (outcome == 'mics_outcome_c'){ # same as outcome c because don't use outcome draws and same amount of missingness between the two outcomes 
      diff_df <- fread(
        file.path(
          data_dir,
          paste0(cur_country, "_", mode, "_15_19_outcome_c_baseline_endline_diff_upper_lower.csv")
        )
      )
    } else{
      diff_df <- fread(
        file.path(
          data_dir,
          paste0(cur_country, "_", mode, "_15_19_", outcome, "_baseline_endline_diff_upper_lower.csv")
        )
      )
    }
    
   
    diff_df <- diff_df[, .(variable, mean = mean_diff, significance, ci_l, ci_u)]
    diff_df[, category := "Data"]
    
    # read in mean difference for outcome
    if (cur_country == 'mw' & outcome == 'mics_outcome_c'){
      # for mw mics outcome c, use same ci as outcome c because already using mics for mw
       test_ci <- fread(file.path(in.dir.ob, paste0('alt_', mode, '_oaxaca_15_19_ci_detail_', "outcome_c", "_baseline_ref_", coef_group, "_coef_", cur_country, '.csv')), fill = T)
    } else{
      test_ci <- fread(file.path(in.dir.ob, paste0('alt_', mode, '_oaxaca_15_19_ci_detail_', outcome, "_baseline_ref_", coef_group, "_coef_", cur_country, '.csv')), fill = T)
    }
    names(test_ci) <- c("variable", "ci_l", "ci_u")
    test_ci <- test_ci[rowSums(test_ci == "" | is.na(test_ci)) != ncol(test_ci)]
    
    ## extract and remove summary values
    summary_dt <-  test_ci[c(1: 14), ] 
    summary_dt <- summary_dt[c(4:12),]
    summary_dt <- summary_dt[seq(1, .N, by = 2), -c("ci_u")]
    
    summary_dt <- summary_dt[, variable := gsub('["=\\*]', '', variable)]
    names(summary_dt) <- c("variable", "mean")
    summary_dt <- summary_dt[, mean := gsub('["=\\*]', '', mean)]
    
    ## long to wide
    summary_dt <- data.table(summary_dt)
    summary_dt_wide <- dcast(summary_dt, formula = ... ~ variable, value.var = "mean")
    summary_dt_wide <- summary_dt_wide[, -c(".")]
    
    # extract difference and summary value 
    difference_val <- as.numeric(summary_dt_wide$difference)
    
    # specify endline data type
    endline <- ifelse(cur_country == 'mw', 'mics', 'dhs')

    # remove non-driver columns
    drop_cols <- c('mod_contra', 'wealth_dummies1', 'country')
    outcome_cols <- grep('^outcome_[abc]_(dhs|mics)$', names(draws_dt), value = TRUE)
    drop_cols <- unique(c(drop_cols, outcome_cols))
    drop_cols <- drop_cols[drop_cols %in% names(draws_dt)]
    draws_dt <- draws_dt[, !names(draws_dt) %in% drop_cols, with = FALSE]
    
    # read in coefficients from OB run
    if (cur_country == 'mw' & outcome == 'mics_outcome_c'){
      # for mw mics outcome c, use same ci as outcome c because already using mics for mw
      coef_dt <- fread(file.path(in.dir.ob, paste0("alt_", endline, "_oaxaca_15_19_ci_detail_", coef_group, "_outcome_c_", cur_country, ".csv")), fill = T)
      
    } else{
      coef_dt <- fread(file.path(in.dir.ob, paste0("alt_", endline, "_oaxaca_15_19_ci_detail_", coef_group, "_", outcome, "_", cur_country, ".csv")), fill = T)
    }
    
    names(coef_dt) <- c("variable", "ci_l", "ci_u")
    coef_dt <- coef_dt[rowSums(coef_dt == "" | is.na(coef_dt)) != ncol(coef_dt)]
    
    # extract mean value from data
    coef_dt_mean_only <- coef_dt[seq(1, .N, by = 2), -c("ci_u")]
    names(coef_dt_mean_only) <- c("variable", "mean")
    coef_dt_mean_only[, significance := ifelse(grepl("\\*+", mean), "significant", "insignificant")]
    coef_dt_mean_only[, p_value := ifelse(grepl("^[^*]*\\*[^*]*$", mean), "<0.05",
                        ifelse(grepl("^[^*]*\\*[^*]*\\*[^*]*$", mean), "<0.01",
                           ifelse(grepl("^[^*]*\\*[^*]*\\*[^*]*\\*[^*]*$", mean), "<0.001", "Insignificant")))]
    
    coef_dt_mean_only <- coef_dt_mean_only[, variable := gsub('["=\\*]', '', variable)]
    coef_dt_mean_only <- coef_dt_mean_only[, mean := gsub('["=\\*]', '', mean)]
    
    coef_dt_mean_only <- coef_dt_mean_only[2:11]
    
    coef_dt_ci_only <- coef_dt[seq(2, .N, by = 2)]
    coef_dt_ci_only <- coef_dt_ci_only[, ci_l := gsub('["=\\*\\]', '', ci_l)]
    coef_dt_ci_only <- coef_dt_ci_only[, ci_l := gsub('\\[', '', ci_l)]
    
    coef_dt_ci_only <- coef_dt_ci_only[, ci_u := gsub('["=\\*\\]', '', ci_u)]
    coef_dt_ci_only <- coef_dt_ci_only[, ci_u := gsub('\\]', '', ci_u)]
    
    coef_dt_ci_only <- coef_dt_ci_only[2:11]
    
    coef_dt_ci_only[, variable := unique(coef_dt_mean_only$variable)]
    
    coef_dt <- merge(coef_dt_mean_only, coef_dt_ci_only, by = "variable")
    coef_dt[, catg := 'coef']
    
    # combine with data difference values
    share_dt <- merge(coef_dt[, .(variable, mean)], diff_df[, .(variable, mean)], by = "variable", all.x = TRUE,
                      suffixes = c("_coef", "_data"))
    
    # read in coefficient draws
    if (cur_country == "mw" & outcome == "mics_outcome_c"){
      # for mw mics outcome c, use same draws as outcome c because already using mics for mw
       coef_draws_dt <- fread(file.path(draw_dir_coef, paste0(cur_country, "_coef_draws_", coef_group, '_', "outcome_c", ".csv")))
    } else if (outcome == "marriage_preg_drop_outcome_c"){
      coef_draws_dt <- fread(file.path(draw_dir_coef, paste0(cur_country, "_coef_draws_", outcome, ".csv")))
    } else{
      coef_draws_dt <- fread(file.path(draw_dir_coef, paste0(cur_country, "_coef_draws_", coef_group, '_', outcome, ".csv")))
      coef_draws_dt <- coef_draws_dt[, .SD, .SDcols = sort(names(coef_draws_dt))]
      coef_draws_dt <- coef_draws_dt[, -c("country")]
    }

    # ensure same variables/order in both draw datasets
    keep_vars <- intersect(names(draws_dt), names(coef_draws_dt))
    draws_dt <- draws_dt[, ..keep_vars]
    coef_draws_dt <- coef_draws_dt[, ..keep_vars]
    
    ## multiply data draws by coefficient draws
    result <- data.table(mapply('*', draws_dt, coef_draws_dt))
    
    ## take mean, 95th, and 5th percentile, and empirical p-value
    for (var in names(draws_dt)){
      results_dt_tmp <- data.table()
      
      # Extract the draws vector once to speed up the loop and clean up the code
      vec <- result[, get(eval(var))]
      
      mean_tmp <- mean(vec)
      upper_tmp <- quantile(vec, 0.975)
      lower_tmp <- quantile(vec, 0.025)
      
      # 1. Count draws above and below zero
      count_below <- sum(vec < 0)
      count_above <- sum(vec > 0)
      total_draws <- length(vec)
      
      # 2. Calculate two-tailed p-value (with +1 correction to avoid exactly 0)
      pval_tmp <- (2 * min(count_below, count_above) + 1) / (total_draws + 1)
      
      results_dt_tmp[, variable := var]
      results_dt_tmp[, mean := mean_tmp]
      results_dt_tmp[, upper := upper_tmp]
      results_dt_tmp[, lower := lower_tmp]
      results_dt_tmp[, p_value := pval_tmp] # Assign the p-value here
      # calc if p value is < 0.05, <0.01, or < 0.001
       results_dt_tmp[, p_value := ifelse(p_value < 0.001, "<0.001",
                                  ifelse(p_value < 0.01, "<0.01",
                                         ifelse(p_value < 0.05, "<0.05", "Insignificant")))]
      
      country_results <- rbind(country_results, results_dt_tmp)
    }
    
    country_results[, catg := 'attribution']
    country_results[, sig := 0]
    country_results[upper < 0, sig := 1]
    country_results[lower > 0, sig := 1]    
    country_results[, diff_outcome := difference_val]
    
    # calculate unexplained (total change in outcome - sum of attributions)
    country_results <- rbind(country_results, data.table(variable = "unexplained",
                                                         mean = difference_val - sum(country_results[sig == 1]$mean), sig = 1,
                                                         p_value = NA_character_, diff_outcome = difference_val), fill = T)
    
    country_results[, country := cur_country]
    results_dt <- rbind(results_dt, country_results)
    
    # add to share dt
    share_dt <- share_dt[, country := cur_country]
    share_dt <- share_dt[, diff_outcome := difference_val]
    share_dt[, crude_attr := as.numeric(mean_coef) * mean_data]
    # merge on calculated attr from draws
    share_dt <- merge(share_dt, country_results[, .(variable, mean, sig)], by = "variable", all.x = TRUE)
    setnames(share_dt, "mean", "attr_distribution")
    output_dt <- rbind(output_dt, share_dt, fill = T)
  }
  
  # plot
  plot_dt <- merge(results_dt, pretty_country_labs, by = "country")
  plot_dt[, pretty_country_lab := paste0(pretty_country_lab, ":\n", round(diff_outcome*100,2), "%")]
  # sum significant wealth
  plot_dt[, variable := ifelse(grepl("wealth", variable), "wealth", variable)]
  # sum sig values by variable, lab, and country
  plot_dt <- plot_dt[sig==1, .(mean = sum(mean)), by = .(variable, country, pretty_country_lab, sig)]
  # label variables
  plot_dt[, lab := variable_labs[variable], by = variable]
  plot_dt[, lab := factor(lab)]
  plot_dt$lab <- relevel(plot_dt$lab, 'Unexplained')
  
  y_lab <- ifelse(outcome == "outcome_b",
                  "Effect on change in proportion of women with\nfirst birth or pregnancy in the last two years",
                  "Effect on proportion of women with any birth \n or pregancy in the last two years")
  
  gg <- ggplot(data = plot_dt[sig == 1], aes(x = outcome, fill = lab)) +
    geom_col(aes(y = mean)) +
    geom_hline(yintercept = 0, col = "black", size=1) +
    geom_text(aes(label = ifelse(abs(mean*100) > 0.1, round(mean*100, 2), ""), y = mean), position = position_stack(vjust = .5),  size=4.5) +
    scale_y_continuous(label = percent, breaks = c(.025,0,-.025,-.05,-.075)) +
    scale_fill_manual(values = c( "#FCCDE5","#BEBADA" ,"#FB8072", "#80B1D3", "#FDB462", "#B3DE69","#FFFFB3" ,"#D3D3D3"),
                      breaks = c("Age", "Believes beating is justified", "Marital status", "Years of education",
                                 "Has had intercourse (unmarried)", "Unmet need", "Wealth", "Unexplained")) +
    labs(y = y_lab, x = "") +
    theme_bw() + theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                       strip.background = element_blank(),
                       text = element_text(size = 24),
                       plot.caption = element_text(size = 12,hjust = 0)) +
    facet_wrap(~pretty_country_lab, nrow = 1)
  
  ggsave(file.path(out.dir, 'plots', paste0("OB ", outcome, "_", coef_group, " manual.png")), gg, width = 15, height=9.5)
  
  if (outcome == "outcome_c") {
    # save output_dt
    output_dt[, lab := variable_labs[variable], by = variable]
    write.csv(output_dt, file.path(out.dir, paste0("output_dt_", outcome, "_", coef_group, ".csv")), row.names = F)
  }

  
  # If outcome is outcome_c, save attribution table (mean, upper, lower, sig) for each variable
  if (outcome == "outcome_c") {
    attribution_table <- results_dt[, .(country, variable, mean, upper, lower, p_value)]
    attribution_table <- merge(attribution_table, pretty_country_labs, by = "country")
    attribution_table[, lab := unlist(variable_labs[variable])]
    attribution_table <- attribution_table[p_value != "Insignificant" | variable == "unexplained"]
    attribution_table[, mean := ifelse( lab == "Unexplained", round(mean, 4), paste0(round(mean, 4), " (", round(lower, 4), ", ", round(upper, 4), ")"))]
    attribution_table <- attribution_table[, c("pretty_country_lab", "lab", "mean", "p_value")]
    setnames(attribution_table, c("pretty_country_lab", "lab", "mean", "p_value"), c("Country", "Variable", "Mean (95% CI)", "P value"))
    
    fwrite(attribution_table, file.path(out.dir, paste0("attribution_table_outcome_c_", coef_group, ".csv")))
  }
  
}

# call function -------
run_ob_manual("c", "endline")
run_ob_manual("b", "endline")
run_ob_manual("c", "pooled")
run_ob_manual("full_sensitivity_outcome_c", "endline")
run_ob_manual("mics_outcome_c", "endline")
run_ob_manual("marriage_preg_drop_outcome_c", "endline")

# create data frame to save results: sexually active women only 
run_ob_manual_part_2 <- function(outcome, coef_group) {
  outcome <- tolower(outcome)
  coef_group <- tolower(coef_group)
  
  if (outcome %in% c("a", "b", "c")) {
    outcome <- paste0("outcome_", outcome)
  }
  
  if (!(outcome %in% c("outcome_a", "outcome_b", "outcome_c"))) {
    stop("`outcome` must be one of: a, b, c, outcome_a, outcome_b, outcome_c")
  }
  if (!(coef_group %in% c("endline", "pooled"))) {
    stop("`coef_group` must be one of: endline, pooled")
  }
  
  results_dt <- data.table()
  output_dt <- data.table()
  
  # -------- Run decomp per country ---------
  for (cur_country in c("cm", "gh", "mw", "np", "rw")){
    country_results <- data.table()
    share_dt <- data.table()
    
    mode <- ifelse(cur_country != "mw", "dhs", "mics")
    # read in draws
    draws_dt <- data.table(fread(file.path(draw_dir.part.2, paste0(cur_country, '_draws_part_2.csv'))))
    draws_dt <- draws_dt[, .SD, .SDcols = sort(names(draws_dt))]
    
    # read in mean difference for non-outcome variables
    diff_df <- fread(
      file.path(
        data_dir,
        paste0(cur_country, "_", mode, "_15_19_", "part_2_", outcome, "_baseline_endline_diff_upper_lower.csv")
      )
    )
    diff_df <- diff_df[, .(variable, mean = mean_diff, significance, ci_l, ci_u)]
    diff_df[, category := "Data"]
    
    # read in mean difference for outcome
    test_ci <- fread(file.path(in.dir.ob, paste0("alt_", mode, "_oaxaca_15_19_ci_detail_part_2_", outcome, '_', cur_country, ".csv")), fill = T)
    
    names(test_ci) <- c("variable", "ci_l", "ci_u")
    test_ci <- test_ci[rowSums(test_ci == "" | is.na(test_ci)) != ncol(test_ci)]
    
    ## extract and remove summary values
    summary_dt <-  test_ci[c(1: 14), ] 
    summary_dt <- summary_dt[c(4:12),]
    summary_dt <- summary_dt[seq(1, .N, by = 2), -c("ci_u")]
    
    summary_dt <- summary_dt[, variable := gsub('["=\\*]', '', variable)]
    names(summary_dt) <- c("variable", "mean")
    summary_dt <- summary_dt[, mean := gsub('["=\\*]', '', mean)]
    
    ## long to wide
    summary_dt <- data.table(summary_dt)
    summary_dt_wide <- dcast(summary_dt, formula = ... ~ variable, value.var = "mean")
    summary_dt_wide <- summary_dt_wide[, -c(".")]
    
    # extract difference and summary value 
    difference_val <- as.numeric(summary_dt_wide$difference)
    
    # specify endline data type
    endline <- ifelse(cur_country == 'mw', 'mics', 'dhs')
    
    # remove non-driver columns
    drop_cols <- c('mod_contra', 'wealth_dummies1', 'country')
    outcome_cols <- grep('^outcome_[abc]_(dhs|mics)$', names(draws_dt), value = TRUE)
    drop_cols <- unique(c(drop_cols, outcome_cols))
    drop_cols <- drop_cols[drop_cols %in% names(draws_dt)]
    draws_dt <- draws_dt[, !names(draws_dt) %in% drop_cols, with = FALSE]
    
    # read in coefficient draws
    coef_draws_dt <- fread(file.path(draw_dir_coef, paste0(cur_country, "_coef_draws_", coef_group, '_', outcome, "_part_2.csv")))
    coef_draws_dt <- coef_draws_dt[, .SD, .SDcols = sort(names(coef_draws_dt))]
    coef_draws_dt <- coef_draws_dt[, -c("country")]
    
    # ensure same variables/order in both draw datasets
    keep_vars <- intersect(names(draws_dt), names(coef_draws_dt))
    draws_dt <- draws_dt[, ..keep_vars]
    coef_draws_dt <- coef_draws_dt[, ..keep_vars]
    
    ## multiply data draws by coefficient draws
    result <- data.table(mapply('*', draws_dt, coef_draws_dt))
    
    ## take mean, 95th, and 5th percentile, and empirical p-value
    for (var in names(draws_dt)){
      results_dt_tmp <- data.table()
      
      # Extract the draws vector once to speed up the loop and clean up the code
      vec <- result[, get(eval(var))]
      
      mean_tmp <- mean(vec)
      upper_tmp <- quantile(vec, 0.975)
      lower_tmp <- quantile(vec, 0.025)
      
      # 1. Count draws above and below zero
      count_below <- sum(vec < 0)
      count_above <- sum(vec > 0)
      total_draws <- length(vec)
      
      # 2. Calculate two-tailed p-value (with +1 correction to avoid exactly 0)
      pval_tmp <- (2 * min(count_below, count_above) + 1) / (total_draws + 1)
      
      results_dt_tmp[, variable := var]
      results_dt_tmp[, mean := mean_tmp]
      results_dt_tmp[, upper := upper_tmp]
      results_dt_tmp[, lower := lower_tmp]
      results_dt_tmp[, p_value := pval_tmp] # Assign the p-value here
      # calc if p value is < 0.05, <0.01, or < 0.001
       results_dt_tmp[, p_value := ifelse(p_value < 0.001, "<0.001",
                                  ifelse(p_value < 0.01, "<0.01",
                                         ifelse(p_value < 0.05, "<0.05", "Insignificant")))]
      
      country_results <- rbind(country_results, results_dt_tmp)
    }
    
    country_results[, catg := 'attribution']
    country_results[, sig := 0]
    country_results[upper < 0, sig := 1]
    country_results[lower > 0, sig := 1]    
    country_results[, diff_outcome := difference_val]
    
    # calculate unexplained (total change in outcome - sum of attributions)
    country_results <- rbind(country_results, data.table(variable = "unexplained",
                                                         mean = difference_val - sum(country_results[sig == 1]$mean), sig = 1,
                                                         p_value = NA_character_, diff_outcome = difference_val), fill = T)
    
    country_results[, country := cur_country]
    results_dt <- rbind(results_dt, country_results)
  
  }
  
  # plot
  plot_dt <- merge(results_dt, pretty_country_labs, by = "country")
  plot_dt[, pretty_country_lab := paste0(pretty_country_lab, ":\n", round(diff_outcome*100,2), "%")]
  # sum significant wealth
  plot_dt[, variable := ifelse(grepl("wealth", variable), "wealth", variable)]
  # sum sig values by variable, lab, and country
  plot_dt <- plot_dt[sig==1, .(mean = sum(mean)), by = .(variable, country, pretty_country_lab, sig)]
  # label variables
  plot_dt[, lab := variable_labs[variable], by = variable]
  plot_dt[, lab := factor(lab)]
  plot_dt$lab <- relevel(plot_dt$lab, 'Unexplained')
  
  y_lab <- ifelse(outcome == "outcome_b",
                  "Effect on change in proportion of women with\nfirst birth or pregnancy in the last two years",
                  "Effect on proportion of women with any birth \n or pregancy in the last two years")
  
  gg <- ggplot(data = plot_dt[sig == 1], aes(x = outcome, fill = lab)) +
    geom_col(aes(y = mean)) +
    geom_hline(yintercept = 0, col = "black", size=1) +
    geom_text(aes(label = ifelse(abs(mean*100) > 0.1, round(mean*100, 2), ""), y = mean), position = position_stack(vjust = .5),  size=4.5) +
    scale_y_continuous(label = percent)+ #breaks = c(.025,0,-.025,-.05,-.075)) +
    scale_fill_manual(values = c( "#FCCDE5","#BEBADA" ,"#FB8072", "#80B1D3", "#FDB462", "#B3DE69","#FFFFB3" ,"#D3D3D3"),
                      breaks = c("Age", "Believes beating is justified", "Marital status", "Years of education",
                                 "Age at first sex", "Unmet need", "Wealth", "Unexplained")) +
    labs(y = y_lab, x = "") +
    theme_bw() + theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                       strip.background = element_blank(),
                       text = element_text(size = 24),
                       plot.caption = element_text(size = 12,hjust = 0)) +
    facet_wrap(~pretty_country_lab, nrow = 1)
  
  ggsave(file.path(out.dir, 'plots', paste0("OB ", outcome, "_", coef_group, " manual_part_2.png")), gg, width = 15, height=9.5)
  
}
run_ob_manual_part_2("outcome_c", "endline")

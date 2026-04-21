#-------------------Header------------------------------------------------

## Created by: NAME
## Run oaxaca-blinder decomp for asher manually: w pre partum contra 
## *Using pre-partum modern contraceptive use endline coefficient and current modern contraceptive use change in means

## set-up --------
pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey,tidyr,glmnet, ggpattern)

'%ni%' <- Negate('%in%')

# date of draw creation
in.date.draw <- 'DATE'
# date of ob run for coefficients to use 
ob.in.date <- 'DATE'
# date of data mean creation
in.date.data <-'DATE'
in.date.draw <- 'DATE'

## directories
data_dir <- file.path('FILEPATH', in.date.data)
draw_dir <-  file.path('FILEPATH', in.date.draw)
in.dir.ob <-  file.path('FILEPATH', ob.in.date)
out.dir <-  file.path('FILEPATH', Sys.Date())
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
                      "mod_contra_outcome_c" = "Prepartum modern contraception use*",
                      "mod_contra" = "Prepartum modern contraception use*") 

pretty_country_labs <- data.table(country = c( "cm", "rw" , "gh" , "mw" , "np"),
                                  pretty_country_lab = c("Cameroon", "Rwanda","Ghana","Malawi", "Nepal"))

# create data frame to save results
results_dt <- data.table()
exemplars_dt <- data.table()
outcome <- 'outcome_c'

# -------- Run decomp per country ---------
for (cur_country in c("gh", "mw", "np", "rw")){
  country_results <- data.table()
  share_dt <- data.table()
  
  mode <- 'dhs'
  # read in draws
  draws_dt <- data.table(fread(file.path(draw_dir, paste0(cur_country, '_draws.csv'))))
  draws_dt <- draws_dt[, .SD, .SDcols = sort(names(draws_dt))]
  
  # read in mean difference for outcome variable
  diff_means <- fread(file.path(data_dir, paste0("diff_means_", cur_country, "_dhs_15_19_", outcome, '_mod_contra_imputed', ".csv")))

  # read in mean difference for non-outcome variables (for exemplars)
  diff_df <- diff_means[, .(variable, mean_data = as.numeric(mean_diff))]
  # remove mod contra outcome c and rename mod contra to mod contra outcome c since we are using mod contra change in means and mod contra outcome c coefficient
  diff_df <- diff_df[variable != "mod_contra_outcome_c"]
  
  # extract difference and summary value 
  difference_val <- as.numeric(diff_means[variable == paste0(outcome, "_", mode)]$mean_diff)

  # specify endline data type
  if (cur_country == 'mw'){
    endline <- 'mics'
    # remove outcome
    draws_dt <- draws_dt[, -c("outcome_c_mics", 'unmet_need', 'wealth_dummies1', 'country')]
  } else {
    endline <- 'dhs'
    draws_dt <- draws_dt[, -c("outcome_c_dhs", 'unmet_need', 'wealth_dummies1', 'country')]
  }

   # read in coefficient draws 
  coef_draws_dt <- fread(file.path(draw_dir, paste0(cur_country, "_coef_draws_w_contra.csv")))
  coef_draws_dt <- coef_draws_dt[, .SD, .SDcols = sort(names(coef_draws_dt))]
  coef_draws_dt <- coef_draws_dt[, -c("country")]

  # read in coefficient means (for exemplars)
  coef_dt <- fread(file.path(in.dir.ob, paste0('outcome_c_dhs_oaxaca_15_19_ci_detail_endline_contra_', cur_country, '.csv')), fill = T)
  names(coef_dt) <- c("variable", "lower", "upper")
  coef_dt <- coef_dt[rowSums(coef_dt == "" | is.na(coef_dt)) != ncol(coef_dt)]

  coef_dt_mean_only <- coef_dt[seq(1, .N, by = 2), -c("upper")]
  names(coef_dt_mean_only) <- c("variable", "mean")
  coef_dt_mean_only <- coef_dt_mean_only[, variable := gsub('["=\\*]', '', variable)]
  coef_dt_mean_only <- coef_dt_mean_only[, mean := gsub('["=\\*]', '', mean)]
  coef_dt_mean_only <- coef_dt_mean_only[2:11]
  coef_dt_mean_only[, mean_coef := as.numeric(mean)]
  # rename mod contra outcome c to mod contra for merging with data change in means since we are using mod contra change in means and mod contra outcome c coefficient
  coef_dt_mean_only[variable == "mod_contra_outcome_c", variable := "mod_contra"]

  # set 
  share_dt <- merge(coef_dt_mean_only[, .(variable, mean_coef)], diff_df[, .(variable, mean_data)],
                    by = "variable", all.x = TRUE)

  ## multiply data draws by coefficient draws
  result <- data.table(mapply('*', draws_dt, coef_draws_dt))
  
  ## take mean, 95th, and 5th percentile 
  for (var in names(draws_dt)){
    results_dt_tmp <- data.table()
    mean_tmp <- mean(result[, get(eval(var))])
    upper_tmp <- quantile(result[, get(eval(var))], 0.975)
    lower_tmp <- quantile(result[, get(eval(var))], 0.025)
    results_dt_tmp[, variable := var]
    results_dt_tmp[, mean := mean_tmp]
    results_dt_tmp[, upper := upper_tmp]
    results_dt_tmp[, lower := lower_tmp]
    country_results <- rbind(country_results, results_dt_tmp)
  }
  country_results[, catg := 'attribution']
  country_results[, sig := 0]
  country_results[upper < 0, sig := 1]
  country_results[lower > 0, sig := 1]
  
  country_results[, diff_outcome := difference_val]
  
  # calculate unexplained (total change in outcome - sum of attributions)
  country_results <- rbind(country_results, data.table(variable = "unexplained",
                                             mean = difference_val- sum(country_results[sig == 1]$mean), sig = 1, diff_outcome = difference_val), fill = T)
  
  country_results[, country := cur_country]
  results_dt <- rbind(results_dt, country_results)

  # add to share dt for exemplars
  share_dt <- share_dt[, country := cur_country]
  share_dt <- share_dt[, diff_outcome := difference_val]
  share_dt[, crude_attr := as.numeric(mean_coef) * mean_data]
  # merge on calculated attribution from draws
  share_dt <- merge(share_dt, country_results[, .(variable, mean, sig)], by = "variable", all.x = TRUE)
  setnames(share_dt, "mean", "attr_distribution")
  exemplars_dt <- rbind(exemplars_dt, share_dt, fill = TRUE)
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
plot_dt$lab <- relevel(  plot_dt$lab, 'Unexplained')

gg_c <- ggplot(data = plot_dt[sig == 1], aes(x = outcome, fill = lab)) +
  geom_col(aes(y = mean)) +
  geom_hline(yintercept = 0, col = "black", size=1) +
  geom_text(aes(label = ifelse(abs(mean*100) > 0.1, round(mean*100, 2), ""), y = mean), position = position_stack(vjust = .5),  size=4.5) +
  scale_y_continuous(label = percent, breaks = c(.025,0,-.025,-.05,-.075)) +
  scale_fill_manual(values = c( "#FCCDE5","#BEBADA" ,"#FB8072", "#80B1D3", "#FDB462", "#B3DE69","#FFFFB3" ,"#D3D3D3"),
                    breaks = c("Age", "Believes beating is justified", "Marital status", "Years of education",
                               "Has had intercourse (unmarried)", "Prepartum modern contraception use*", "Wealth", "Unexplained")) +
  labs(y = "Effect on proportion of women with any birth \n or pregancy in the last two years", x = "",
  caption = "*Using pre-partum modern contraceptive use endline coefficient and current modern contraceptive use change in means") +
  theme_bw() + theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                     strip.background = element_blank(),text = element_text(size = 24),
                     plot.caption = element_text(size = 12,hjust = 0)) +
  facet_wrap(~pretty_country_lab, nrow = 1)

ggsave(file.path(out.dir, 'plots', paste0("OB outcome C manual with contra.png")), gg_c, width = 14, height=9.5)


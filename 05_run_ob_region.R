#-------------------Header------------------------------------------------

# Project: IHME ASHER Decomposition
# Creation date: 6/23/2024
# Run oaxaca-blinder decomp for asher by region 
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

## set-up --------
pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey,tidyr,glmnet, ggpattern)

'%ni%' <- Negate('%in%')

# date of draw creation
in.date <- '2024-06-23'
# date of ob run for coefficients to use 
ob.in.date <- '2024-06-20'

## directories
in.dir.data <-  file.path('FILEPATH', in.date)
in.dir.ob <-  file.path('FILEPATH', ob.in.date)
out.dir <-  file.path('FILEPATH', Sys.Date())
dir.create(out.dir)
dir.create(file.path(out.dir, 'plots'))

# variable labels: want wealth individually 
variable_labs <- list("difference" = "Difference", 
                      "unexplained" = "Unexplained",
                      "explained" = "Explained",
                      "age" = "Age",
                      "age_1st_sex_imp" = "Age at first sex",
                      "curr_cohabit" = "Marital status",
                      "educ_single_yrs" = "Years of education",
                      "had_intercourse" = "Has had intercourse",
                      "unmet_need" = "Unmet need",
                      "wealth_dummies1" = "Wealth quintile 1",
                      "wealth_dummies2" = "Wealth quintile 2",
                      "wealth_dummies3" = "Wealth quintile 3",
                      "wealth_dummies4" = "Wealth quintile 4",
                      "wealth_dummies5" = "Wealth quintile 5",
                      "wealth" = "Wealth",
                      "beating_just" = "Believes beating is justified")

# variable colors 
lab_cols <-  c("Age" = "#E30E0E", # dark red
               "Age at first sex" = "#FA7D7A", # light red
               "Believes beating is justified" = "#a000c8", #purple
               "Marital status" = "#FA8D20", # orange
               "Years of education" = "#FFCC12", # orange-yellow
               "Has had intercourse" = "#AEFA20", # light green
               "Unmet need" = "#48FA20", # lime green
               "Wealth" = "#09D9CE", # seafoam
               "Unexplained variation" = "snow",
               "Explained" =  	"#000068", # blue
               "Unexplained" = "#D3D3D3", # white
               "Difference" = "red") 

pretty_country_labs <- data.table(country = c( "cm", "rw" , "gh" , "mw" , "np"),
                                  pretty_country_lab = c("Cameroon", "Rwanda","Ghana","Malawi", "Nepal"))

# -------- Run decomp per country ---------
run_decomp <- function(cur_country){
  # read in regional draws
  draws_dt <- data.table(fread(file.path(in.dir.data, paste0(cur_country, '_region_draws.csv'))))
  draws_dt <- draws_dt[, .SD, .SDcols = sort(names(draws_dt))]
  
  # specify endline data type
  if (cur_country == 'mw'){
    endline <- 'mics'
    draws_dt <- draws_dt[, -c("any_birth_preg_2_yr_mics", "age_1st_sex_imp")]
  } else {
    endline <- 'dhs'
    draws_dt <- draws_dt[, -c("any_birth_preg_2_yr_dhs", "age_1st_sex_imp")]
  }

   # read in coefficients from OB run 
  coef_dt <- fread(file.path(in.dir.ob, paste0("alt_", endline, "_oaxaca_15_19_ci_detail_endline_", cur_country, ".csv")), fill = T)
  names(coef_dt) <- c("variable", "ci_l", "ci_u")
  coef_dt <- coef_dt[rowSums(coef_dt == "" | is.na(coef_dt)) != ncol(coef_dt)]
  
  # extract mean value from data
  coef_dt_mean_only <- coef_dt[seq(1, .N, by = 2), -c("ci_u")]
  names(coef_dt_mean_only) <- c("variable", "mean")
  coef_dt_mean_only[, significance := ifelse(grepl("\\*+", mean), "significant", "insignificant")]
  
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
  
  # read in coefficient draws 
  coef_draws_dt <- fread(file.path(in.dir.data, paste0(cur_country, "_coef_draws.csv")))
  coef_draws_dt <- coef_draws_dt[, .SD, .SDcols = sort(names(coef_draws_dt))]
  coef_draws_dt <- coef_draws_dt[, -c("country")]
  
  # create data frame to save results from all regions
  all_region_dt <- data.table()
  # plot reach region individually 
  for (region in unique(draws_dt$admin_1_region)) {
    # filter data to current region 
    data_draws_dt_tmp <- draws_dt[admin_1_region == eval(region)]
    data_draws_dt_tmp <- data_draws_dt_tmp[, -c("admin_1_region", "country", "wealth_dummies1", "wealth_index")]

    ## multiply data draws by coefficient draws
    result <- data.table(mapply('*', data_draws_dt_tmp, coef_draws_dt))
  
    ## take mean, 95th, and 5th percentile 
    results_dt <- data.table()
    for (var in names(data_draws_dt_tmp)){
      results_dt_tmp <- data.table()
      mean_tmp <- mean(result[, get(eval(var))])
      upper_tmp <- quantile(result[, get(eval(var))], 0.975)
      lower_tmp <- quantile(result[, get(eval(var))], 0.025)
      sig <- coef_dt[variable==eval(var)]$significant
      results_dt_tmp[, variable := var]
      results_dt_tmp[, mean := mean_tmp]
      results_dt_tmp[, upper := upper_tmp]
      results_dt_tmp[, lower := lower_tmp]
      results_dt <- rbind(results_dt, results_dt_tmp)
    }
    results_dt[, catg := 'attribution']
    results_dt[, sig := 0]
    results_dt[upper < 0, sig := 1]
    results_dt[lower > 0, sig := 1]

  # get means and confidence interval for coefficients
  coef_summary <- data.table()
  for (var in names(data_draws_dt_tmp)){
    coef_dt_tmp <- data.table()
    mean_tmp <- mean(coef_draws_dt[, get(eval(var))])
    upper_tmp <- quantile(coef_draws_dt[, get(eval(var))], 0.975)
    lower_tmp <- quantile(coef_draws_dt[, get(eval(var))], 0.025)
    sig <- coef_dt[variable==eval(var)]$significance
    coef_dt_tmp[, variable := var]
    coef_dt_tmp[, mean := mean_tmp]
    coef_dt_tmp[, upper := upper_tmp]
    coef_dt_tmp[, lower := lower_tmp]
    coef_dt_tmp[, significance := sig]
    coef_summary <- rbind(coef_summary, coef_dt_tmp, fill =T)
  }
  coef_summary[, catg := 'coef']
  coef_summary[, sig := ifelse(significance == "significant", 1, 0)]
  
  # get means and confidence interval for difference in data
  data_summary <- data.table()

  for (var in names(data_draws_dt_tmp)){
    data_dt_tmp <- data.table()
    mean_tmp <- mean(data_draws_dt_tmp[, get(eval(var))])
    upper_tmp <- quantile(data_draws_dt_tmp[, get(eval(var))], 0.975)
    lower_tmp <- quantile(data_draws_dt_tmp[, get(eval(var))], 0.025)
    
    data_dt_tmp[, variable := var]
    data_dt_tmp[, mean := mean_tmp]
    data_dt_tmp[, upper := upper_tmp]
    data_dt_tmp[, lower := lower_tmp]
    data_summary <- rbind(data_summary, data_dt_tmp)
  }
  
  data_summary[, catg := 'data_diff']
  data_summary[, sig := 0]
  data_summary[upper < 0, sig := 1]
  data_summary[lower > 0, sig := 1]
  
  plot_dt <- do.call("rbind", list(results_dt, coef_summary, data_summary, fill=T))
  plot_dt[, diff := ifelse(mean <0, "Negative", "Positive")]
  
  plot_dt[, country := cur_country]  
  plot_dt[, lab := unlist(variable_labs[plot_dt$variable])]
  plot_dt <- merge(plot_dt, pretty_country_labs, by = "country")
  # order categories
  plot_dt[, catg := factor(catg, levels = c("data_diff", "coef", "attribution"),
                           labels = c("Mean difference", "Coefficient", "OB attribution"))]

  pretty_country_lab <- unique(plot_dt$pretty_country_lab)
  
  # plot
  gg_all <- ggplot(data = plot_dt[,-c('significance')], aes(x = lab, y = mean,fill = diff,alpha =sig)) +
    facet_wrap(~catg, ncol=3, scales='free', )+
    geom_bar(stat='identity', width = 0.75, position = 'dodge') +
    geom_errorbar(aes(ymin=lower, ymax=upper), width =1)+
    coord_flip() +
    labs(y = "", x = "", fill = "Factor type", alpha = "Significance", title = paste0(pretty_country_lab, ": ", region, ", Oaxaca-Blinder decomposition")) +
    theme_bw()+
    geom_hline(aes(yintercept = 0), linetype = "dashed", size = .5) +
    scale_fill_brewer(palette = "Set1") +
    theme(plot.title = element_text(hjust = 0.5), 
          text = element_text(size = 22),
          legend.position = 'none',
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
          # strip.background = element_blank(),
          # strip.text.x = element_blank())
  ggsave(file.path(out.dir, 'plots', paste0('ob_', cur_country, "_", region, '.pdf')), width = 20, height=10)
  plot_dt[, region := region]
  all_region_dt <- rbind(all_region_dt, plot_dt)
  }
  all_region_dt[, direction := ifelse(mean >0, "pos", "neg")]
  
  # plot attribution across all regions in the current country
  gg_all <- ggplot(data = all_region_dt[catg == "OB attribution"], aes(x = lab, y = as.numeric(mean), fill = direction, alpha = sig)) +
    facet_wrap(~region, ncol=5)+
    geom_bar(stat='identity', width = 0.75, position = 'dodge') +
    geom_errorbar(aes(ymin=lower, ymax=upper))+
    coord_flip() +
    labs(y = "\n Attribution to outcome", x = "", fill = "Factor type", alpha = "Significance", title = paste0(pretty_country_lab, ": Attribution to outcome by region")) +
    theme_bw()+
    geom_hline(aes(yintercept = 0), linetype = "dashed", size = .5) +
    scale_fill_brewer(palette = "Set1") +
    #scale_y_continuous(limits=c(-0.4,0,0.4), breaks = c(-0.4,0,0.4))+
    theme(plot.title = element_text(hjust = 0.5), 
          text = element_text(size = 22),
          legend.position = 'none',
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
          strip.background = element_blank())
  ggsave(file.path(out.dir, 'plots', paste0('attr_all_regions_', cur_country, '.pdf')), width = 20, height=10)
   
  # plot data change across all regions in the current country
  gg_all <- ggplot(data = all_region_dt[catg == "Mean difference"], aes(x = lab, y = as.numeric(mean), fill = direction, alpha = sig)) +
    facet_wrap(~region, ncol=5)+
    geom_bar(stat='identity', width = 0.75, position = 'dodge') +
    geom_errorbar(aes(ymin=lower, ymax=upper))+
    coord_flip() +
    labs(y = "\n Change in data mean", x = "", fill = "Factor type", alpha = "Significance", title = paste0(pretty_country_lab, ": Change in data means by region")) +
    theme_bw()+
    geom_hline(aes(yintercept = 0), linetype = "dashed", size = .5) +
    scale_fill_brewer(palette = "Set1") +
    theme(plot.title = element_text(hjust = 0.5), 
          text = element_text(size = 22),
          legend.position = 'none',
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
          strip.background = element_blank())
  ggsave(file.path(out.dir, 'plots', paste0('data_change_all_regions_', cur_country, '.pdf')), width = 20, height=10)

}
run_decomp("cm")
run_decomp("gh")
run_decomp("mw")
run_decomp("np")
run_decomp("rw")

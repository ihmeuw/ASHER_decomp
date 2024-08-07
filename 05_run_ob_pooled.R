#-------------------Header------------------------------------------------
# Project: IHME ASHER Decomposition
# Creation date: 6/26/2024
# Run oaxaca-blinder decomp for asher using coefficients from a pooled regression (all years of data)

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
in.date <- '2024-06-26'
# date of ob run for coefficients to use 
ob.in.date <- '2024-06-26'

## directories
in.dir.data <-  file.path('FILEPATH', in.date)
in.dir.ob <-  file.path('FILEPATH', ob.in.date)
out.dir <-  file.path('FILEPATH', Sys.Date())

dir.create(out.dir)
dir.create(file.path(out.dir, 'plots'))
ob_dir_plot <- file.path(out.dir, 'plots')
  
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
  # specify endline data type
  if (cur_country == 'mw'){
    endline <- 'mics'
  } else {
    endline <- 'dhs'
  }
  
  # read in baseline endline data draws
  draws_dt <- data.table(fread(file.path(in.dir.data, paste0(cur_country, '_baseline_endline_draws.csv'))))
  draws_dt <- draws_dt[, .SD, .SDcols = sort(names(draws_dt))]

   # read in coefficients from OB run 
  coef_dt <- fread(file.path(in.dir.ob, paste0(endline, "_oaxaca_15_19_pooled_reg_", cur_country, ".csv")), fill = T)
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
  

  draws_dt <- draws_dt[, -c( "country", "wealth_dummies1", "wealth_index", 'age_1st_sex_imp')]

    ## multiply data draws by coefficient draws
    result <- data.table(mapply('*', draws_dt, coef_draws_dt))
  
    ## take mean, 95th, and 5th percentile 
    results_dt <- data.table()
    for (var in names(draws_dt)){
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
  for (var in names(draws_dt)){
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

  for (var in names(draws_dt)){
    data_dt_tmp <- data.table()
    mean_tmp <- mean(draws_dt[, get(eval(var))])
    upper_tmp <- quantile(draws_dt[, get(eval(var))], 0.975)
    lower_tmp <- quantile(draws_dt[, get(eval(var))], 0.025)
    
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
  
  ## save results for later plot
  write.csv(plot_dt, file.path(out.dir, paste0('ob_pooled_', cur_country, '.csv')), row.names = F)

  pretty_country_lab <- unique(plot_dt$pretty_country_lab)
  
  # plot
  gg_all <- ggplot(data = plot_dt[,-c('significance')], aes(x = lab, y = mean,fill = diff,alpha =sig)) +
    facet_wrap(~catg, ncol=3, scales='free', )+
    geom_bar(stat='identity', width = 0.75, position = 'dodge') +
    geom_errorbar(aes(ymin=lower, ymax=upper), width =1)+
    coord_flip() +
    labs(y = "", x = "", fill = "Factor type", alpha = "Significance", title = paste0(pretty_country_lab, ": Oaxaca-Blinder decomposition, pooled data")) +
    theme_bw()+
    geom_hline(aes(yintercept = 0), linetype = "dashed", size = .5) +
    scale_fill_brewer(palette = "Set1") +
    theme(plot.title = element_text(hjust = 0.5), 
          text = element_text(size = 22),
          legend.position = 'none',
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
          # strip.background = element_blank(),
          # strip.text.x = element_blank())
  ggsave(file.path(ob_dir_plot, paste0('ob_pooled_', cur_country, '.pdf')), width = 20, height=10)
  
}
run_decomp("cm")
run_decomp("gh")
run_decomp("mw")
run_decomp("np")
run_decomp("rw")

# read in ob results for plotting across countries -----
files <- list.files(out.dir)
files <- files[grepl('ob_pooled_', files)]
setwd(out.dir)
all_plot_dt <- do.call("rbind", lapply(files, fread))
all_plot_dt[, direction := ifelse(mean <0, "Negative", "Positive")]

# plot means across countries --------
plot_means_all_countries <- function(mode,age_group){
  all_plot_dt <- data.table()
  for (cur_country in c("cm", "gh", "mw", "np", "rw")){
    # read in baseline endline data draws
    draws_dt <- data.table(fread(file.path(in.dir.data, paste0(cur_country, '_baseline_endline_draws.csv'))))
    draws_dt <- draws_dt[, -c( "country", 'wealth_index','age_1st_sex_imp')]
    # get means and confidence interval for difference in data
    data_summary <- data.table()
    
    for (var in names(draws_dt)){
      data_dt_tmp <- data.table()
      mean_tmp <- mean(draws_dt[, get(eval(var))])
      upper_tmp <- quantile(draws_dt[, get(eval(var))], 0.975)
      lower_tmp <- quantile(draws_dt[, get(eval(var))], 0.025)
      
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
    data_summary[, country:= cur_country]
    all_plot_dt <- rbind(all_plot_dt, data_summary)
  }
  
  all_plot_dt <- merge(all_plot_dt, pretty_country_labs, by = "country") 
  all_plot_dt[, direction := ifelse(mean >0, "pos", "neg")]
  all_plot_dt[, lab := unlist(variable_labs[variable])]
  gg_all <- ggplot(data = all_plot_dt[catg == "data_diff"], aes(x = lab, y = mean, fill = direction, alpha = sig)) +
    facet_wrap(~pretty_country_lab, ncol=5)+
    geom_bar(stat='identity', width = 0.75, position = 'dodge') +
    geom_errorbar(aes(ymin=lower, ymax=upper))+
    coord_flip() +
    labs(x = "", y = "\n Change in mean", fill = "Factor type", alpha = "Significance", title = paste0("Changes in means by country, pooled data")) +
    theme_bw()+
    geom_hline(aes(yintercept = 0), linetype = "dashed", size = .5) +
    scale_fill_brewer(palette = "Set1") +
    theme(plot.title = element_text(hjust = 0.5), 
          text = element_text(size = 18),
          legend.position = 'none',
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
          strip.background = element_blank())
  ggsave(file.path(ob_dir_plot, paste0(age_group, '_means_all_pooled_',  mode, ".png")), width = 16, height=10)
}

plot_means_all_countries('mics', '15_19')

# plot attribution across countries --------

gg_all <- ggplot(data = all_plot_dt[catg == "OB attribution"], aes(x = lab, y = as.numeric(mean), fill = direction, alpha = sig)) +
  facet_wrap(~pretty_country_lab, ncol=5)+
  geom_bar(stat='identity', width = 0.75, position = 'dodge') +
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  coord_flip() +
  labs(y = "\n Attribution to outcome", x = "", fill = "Factor type", alpha = "Significance", title = paste0("Attribution to outcome by country, pooled data")) +
  theme_bw()+
  geom_hline(aes(yintercept = 0), linetype = "dashed", size = .5) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(limits=c(-0.2, 0.2), breaks = c(-0.2,0,0.2))+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18),
        legend.position = 'none',
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        strip.background = element_blank())
  
  ggsave(file.path(ob_dir_plot, '15_19_attr_all_pooled_mics.png'), width = 16, height=10)
  
# plot coefficients across countries --------
  
gg_all <- ggplot(data = all_plot_dt[catg == "Coefficient"], aes(x = lab, y = mean, alpha = significance)) +
    facet_wrap(~pretty_country_lab, ncol=5, scales='free_x')+
    scale_color_brewer(palette = "Set1") +
    geom_pointrange(aes(ymin = upper, ymax = lower, color = direction),size =1) + 
    coord_flip() +
    labs(y = "\n Coefficient", x = "", fill = "Factor type", alpha = "Significance", title = paste0("Coefficients by country, pooled data")) +
    theme_bw()+
    geom_hline(aes(yintercept = 0), linetype = "dashed", size = .5) +
    theme(plot.title = element_text(hjust = 0.5), 
          text = element_text(size = 18),
          legend.position = 'none',
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
          strip.background = element_blank())
  
  ggsave(file.path(ob_dir_plot, '15_19_coef_all_pooled_mics.png'), width = 16, height=10)
  
## Created by: Corinne Bintz
## Creation date: 2/13/2024
## plot ob results from stata
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

pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey,tidyr,glmnet, ggpattern)


in.date <- '2024-02-13'
ob.date <- '2024-02-14'

## directories
data_dir <- file.path('/share/scratch/projects/hssa/asher/phase1/data', in.date)
ob_dir <- file.path('/share/scratch/projects/hssa/asher/phase1/results/ob', ob.date)
ob_dir_plot <- file.path(ob_dir, 'plots')
dir.create(ob_dir_plot, showWarnings = FALSE)

# test <- fread(file.path(h,'alt_oaxaca_15_19_coef_detail_mw.csv'))
# names(test) <- c("variable", "val")
# ## remove summary values
# test <- test[-c(1: 9), ] 
# ## find row where unexplained starts
# unexplained_row <- which(test$variable == '="unexplained"')
# 
# ## extract explained df
# explained_df <- test[0:(unexplained_row-1)]
# ## extract unexplained df
# unexplained_df <- test[(unexplained_row+1):(nrow(test)-2)]
# 
# ## clean variable names and values
# explained_df[, variable :=  gsub('["=]', '', variable)]
# explained_df[, val :=  gsub('["=]', '', val)]
# 
# unexplained_df[, variable :=  gsub('["=]', '', variable)]
# unexplained_df[, val :=  gsub('["=]', '', val)]
# 
# explained_df[, catg := "explained"]
# unexplained_df[, catg := "unexplained"]
plot_ob <- function(cur_country, mode, age_group){
  # read in CI
  test_ci <- fread(file.path(ob_dir, paste0('alt_', mode, '_oaxaca_', age_group, '_ci_detail_', cur_country, '.csv')), fill = T)
  names(test_ci) <- c("variable", "ci_l", "ci_u")
  test_ci <- test_ci[rowSums(test_ci == "" | is.na(test_ci)) != ncol(test_ci)]
  
  ## remove summary values
  test_ci <- test_ci[-c(1: 14), ] 
  
  ## find row where unexplained starts
  unexplained_row_ci <- which(test_ci$variable == '="unexplained"')
  
  explained_df_ci <- test_ci[1:(unexplained_row_ci-1)]
  unexplained_df_ci <- test_ci[(unexplained_row_ci+1):(nrow(test_ci)-5)]
  
  ## explained 
  ## extract mean value from data
  explained_df_mean_only <- explained_df_ci[seq(1, .N, by = 2), -c("ci_u")]
  names(explained_df_mean_only) <- c("variable", "mean")
  explained_df_mean_only[, significance := ifelse(grepl("\\*+", mean), "significant", "insignificant")]
  
  explained_df_mean_only <- explained_df_mean_only[, variable := gsub('["=\\*]', '', variable)]
  explained_df_mean_only <- explained_df_mean_only[, mean := gsub('["=\\*]', '', mean)]
  
  explained_df_ci_only <- explained_df_ci[seq(2, .N, by = 2)]
  explained_df_ci_only <- explained_df_ci_only[, ci_l := gsub('["=\\*\\]', '', ci_l)]
  explained_df_ci_only <- explained_df_ci_only[, ci_l := gsub('\\[', '', ci_l)]
  
  explained_df_ci_only <- explained_df_ci_only[, ci_u := gsub('["=\\*\\]', '', ci_u)]
  explained_df_ci_only <- explained_df_ci_only[, ci_u := gsub('\\]', '', ci_u)]
  
  explained_df_ci_only[, variable := unique(explained_df_mean_only$variable)]
  
  explained_df <- merge(explained_df_mean_only, explained_df_ci_only, by = "variable")
  explained_df[, catg := 'explained']
  
  ## unexplained 
  ## extract mean value from data
  unexplained_df_mean_only <- unexplained_df_ci[seq(1, .N, by = 2), -c("ci_u")]
  names(unexplained_df_mean_only) <- c("variable", "mean")
  unexplained_df_mean_only[, significance := ifelse(grepl("\\*+", mean), "significant", "insignificant")]
  
  unexplained_df_mean_only <- unexplained_df_mean_only[, variable := gsub('["=\\*]', '', variable)]
  unexplained_df_mean_only <- unexplained_df_mean_only[, mean := gsub('["=\\*]', '', mean)]
  
  unexplained_df_ci_only <- unexplained_df_ci[seq(2, .N, by = 2)]
  unexplained_df_ci_only <- unexplained_df_ci_only[, ci_l := gsub('["=\\*\\]', '', ci_l)]
  unexplained_df_ci_only <- unexplained_df_ci_only[, ci_l := gsub('\\[', '', ci_l)]
  
  unexplained_df_ci_only <- unexplained_df_ci_only[, ci_u := gsub('["=\\*\\]', '', ci_u)]
  unexplained_df_ci_only <- unexplained_df_ci_only[, ci_u := gsub('\\]', '', ci_u)]
  
  unexplained_df_ci_only[, variable := unique(unexplained_df_mean_only$variable)]
  
  unexplained_df <- merge(unexplained_df_mean_only, unexplained_df_ci_only, by = "variable")
  unexplained_df[, catg := 'unexplained']
  
  plot_df <- rbind(explained_df, unexplained_df)
  
  ## merge on absolute difference
  ## try percent diff 
  
  diff_df <- fread(file.path(data_dir,  paste0(paste(cur_country, mode, age_group, sep = "_"),".csv") ))
  diff_df <- diff_df[, c("variable", "percent_change_mean", "significance")]
  names(diff_df) <- c("variable", "mean","significance")
  
  variables_ob <- unique(plot_df$variable)
  diff_df <- diff_df[variable %in% variables_ob]
  
  diff_df[, catg := "data"]
  ## don't currently have CI from data diff
  diff_df[, ci_l := NA]
  diff_df[, ci_u := NA]
  
  plot_df <- rbind(plot_df, diff_df)
  plot_df[, mean := as.numeric(mean)]
  plot_df[, ci_l := as.numeric(ci_l)]
  plot_df[, ci_u := as.numeric(ci_u)]
  
  ## group variables by individual, hh/community, and health system factors
  plot_df[, grouping := ifelse(variable %in% c("rural", "wealth_index"), "Household/community", "Individual")]
  
  plot_df_cont <- plot_df[variable %in% c("educ_single_yrs", "ideal_child",
                                          "knowledge_mod", "wealth_index")]
  
  plot_df_prop <- plot_df[variable %in% c("attend_school", "curr_cohabit","desire_child_teen", "fp_exp_media",
                                          "mcpr", "never_had_intercourse", "rural", "sex_activity_last_4_weeks" )]
  
  # #plot all together 
  # ggplot(data = plot_df, aes(x = variable, y = mean, fill = grouping)) +
  #   facet_wrap(~catg, ncol=3, scales='free')+
  #   geom_bar(stat='identity', width = 0.75, position = 'dodge') +
  #   geom_errorbar(aes(ymin=ci_l, ymax=ci_u))+
  #   coord_flip() +
  #   labs(y = "Value", x = "Variable", fill = "Factor type", title = paste("OB results", cur_country, mode, age_group)) +
  #   theme_bw()+
  #   theme(plot.title = element_text(hjust = 0.5), 
  #         text = element_text(size = 28))
  # #ggsave(file.path(ob_dir_plot, paste0(age_group, '_ob_results_', cur_country, "_", mode, ".png")), width = 25, height=15)
  # ggsave(file.path(ob_dir_plot, paste0(age_group, '_ob_results_percent_change_', cur_country, "_", mode, ".png")), width = 25, height=15)
  # 
  #plot continuous 
 gg_cont <- ggplot(data = plot_df_cont, aes(x = variable, y = mean, fill = grouping, alpha = significance)) +
    facet_wrap(~catg, ncol=3, scales='free')+
    geom_bar(stat='identity', width = 0.75, position = 'dodge') +
    geom_errorbar(aes(ymin=ci_l, ymax=ci_u))+
    coord_flip() +
    labs(y = "", x = "", fill = "Factor type", alpha = "Significance", title = paste("OB continuous variable results", cur_country, mode, age_group)) +
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5), 
          text = element_text(size = 28),
          legend.position = 'none',
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
 
 gg_prop <- ggplot(data = plot_df_prop, aes(x = variable, y = mean, fill = grouping, alpha = significance)) +
   facet_wrap(~catg, ncol=3, scales='free')+
   geom_bar(stat='identity', width = 0.75, position = 'dodge') +
   geom_errorbar(aes(ymin=ci_l, ymax=ci_u))+
   coord_flip() +
   labs(y = "", x = "", fill = "Factor type", alpha = "Significance",title = paste("OB proportion variable results", cur_country, mode, age_group)) +
   theme_bw()+
   theme(plot.title = element_text(hjust = 0.5), 
         text = element_text(size = 28), 
         legend.position = 'bottom',
         axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

  ggarrange(gg_cont, gg_prop, nrow=2)
  ggsave(file.path(ob_dir_plot, paste0(age_group, '_ob_results_cont_prop_', cur_country, "_", mode, ".png")), width = 25, height=20)
  
  
}
plot_ob('mw', 'dhs', '15_19')
plot_ob('np', 'dhs', '15_19')
plot_ob('cm', 'dhs', '15_19')
plot_ob('rw', 'dhs', '15_19')
plot_ob('gh', 'dhs', '15_19')

plot_ob('mw', 'dhs', '15_24')
plot_ob('np', 'dhs', '15_24')
plot_ob('cm', 'dhs', '15_24')
plot_ob('rw', 'dhs', '15_24')
plot_ob('gh', 'dhs', '15_24')

plot_ob('mw', 'mics', '15_19')
plot_ob('mw', 'mics', '15_24')

# 
# cur_country <- 'mw'
# mode <- 'dhs'
# age_group <- '15_19'
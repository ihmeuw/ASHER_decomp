# Project: IHME ASHER Decomposition
# Creation date: 2/19/2024
# Shapely decomp for ASHER phase 1

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

pacman::p_load(relaimpo, data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, 
               haven, survey,tidyr,glmnet,matrixStats, gridExtra)

## select input data date
in.date <- '2024-04-06'

## directories
data_dir <- file.path('filepath', in.date)

## directories 
in.dir <- file.path('filepath', in.date)
out.dir <- file.path('filepath', Sys.Date())
dir.create(out.dir, showWarnings = FALSE)
out.dir <- file.path('filepath', Sys.Date(), '/ob_vars_only')
dir.create(out.dir, showWarnings = FALSE)

# pretty variable labels 
pretty_labs <- data.table(var = c( "wealth_dummies2", "wealth_dummies3" , "wealth_dummies4" , "wealth_dummies5" , "rural" ,"educ_single_yrs" , "attend_school" , "curr_cohabit" , "ideal_child" , "mcpr" ,
                                     "fp_exp_media" , "knowledge_mod" , "had_intercourse" , "sex_activity_last_4_weeks", "age", "baseline", "unmet_need"),
                          pretty_lab = c("Wealth index 2", "Wealth index 3","Wealth index 4","Wealth index 5", "Rurality", "Education in years", "School attendance in the prior year",
                                         "Marital status", "Ideal number of children", "mCPR", "Exposure to family planning radio, TV, or newspaper", "Number of methods known",
                                         "Ever had intercourse", "Sex in the last 4 weeks", "Age", "Baseline", "Unmet need"))

# pretty country labels 
pretty_country_labs <- data.table(country = c( "cm", "rw" , "gh" , "mw" , "np"),
                          pretty_country_lab = c("Cameroon", "Rwanda","Ghana","Malawi", "Nepal"))

## read in dhs data frames
dhs_df_15_24 <- fread(file.path(in.dir,'ob_input_prepped_df_dhs.csv'))

test <-dhs_df_15_24[, .(mean_val = mean(any_birth_preg_2_yr_dhs)), by = c("country", "baseline")]

## read in mics data frames
mics_df_15_24 <- fread(file.path(in.dir,'ob_input_prepped_df_mics.csv'))

## Function to perform shapely decomposition
perform_shap <- function(cur_country, mode, age_group, form){
  if (mode == "dhs"){
    if(age_group == "15_19"){
      pretty_age_group <- "15 to 19"
      current_data <- dhs_df_15_24[age %in% c(seq(15,19)) & country == cur_country]
    } else{
      pretty_age_group <- "15 to 24"
      current_data <- dhs_df_15_24[ country == cur_country] # default is 15-24
    }
  } else{
    if(age_group == "15_19"){
      pretty_age_group <- "15 to 19"
      current_data <- mics_df_15_24[age %in% c(seq(15,19)) & country == cur_country]
    } else{
      pretty_age_group <- "15 to 24"
      current_data <- mics_df_15_24[ country == cur_country] # default is 15-24
    }
  }
  
  data_svy <- svydesign(id= ~psu_unique,  weights = ~pweight, data = current_data, nest = TRUE)
  
  mod_1 <- svyglm(form, design = data_svy, data =current_data)

  
  save_dt <- data.table(var = names(coef(mod_1)),
                        beta = coef(mod_1),
                        lower = confint(mod_1)[,1],
                        upper = confint(mod_1)[,2])
  # preform 1000 bootstraps of the regression
  bt <- boot.relimp(mod_1, b = 100, rela = T)
  
  ### calculate and display results
  draws <- booteval.relimp(bt)$lmg.boot
  
    save_dt <- data.table(var = names(coef(mod_1)),
                          beta = coef(mod_1),
                          lower = confint(mod_1)[,1],
                          upper = confint(mod_1)[,2],
                          rel_importance_mean = c(NA, colMeans(draws)),
                          rel_importance_lower = c(NA, colQuantiles(draws, probs = 0.025)),
                          rel_importance_upper = c(NA, colQuantiles(draws, probs = 0.975)))

      # standardize predictors
  for(this.var in all.vars(form)){
    std_dt <- copy(current_data)
    std_dt[, eval(this.var) := (get(this.var) - mean(get(this.var), na.rm = T))/sd(get(this.var), na.rm = T)]
  }
  
    std_mod_1 <- svyglm(formula = form, design = data_svy, data = std_dt)
    
  
  save_dt <- merge(save_dt, data.table(var = names(coef(std_mod_1)),
                                       beta_std = coef(std_mod_1),
                                       lower_std = confint(std_mod_1)[,1],
                                       upper_std = confint(std_mod_1)[,2]), by = "var")
  
  # create significance label
  save_dt[, significance := "Not significant"]
  save_dt[upper < 0, significance := "Significantly negative"]
  save_dt[lower > 0, significance := "Significantly positive"]

  # melt and identify measures and metrics
  plot_dt <- melt.data.table(save_dt, id.vars = c("var", "significance"))
  plot_dt[, measure := "Relative Importance"]
  plot_dt[variable %in% c("beta", "lower", "upper"), measure := "Coefficient"]
  plot_dt[variable %in% c("beta_std", "lower_std", "upper_std"), measure := "Standardized Coefficient"]
  plot_dt[, metric := "mean"]
  plot_dt[grepl("lower", variable), metric := "lower"]
  plot_dt[grepl("upper", variable), metric := "upper"]
  
  plot_dt <- dcast.data.table(plot_dt, var + measure + significance ~ metric, value.var = "value")
  
  cutoff <- max(plot_dt[var != "(Intercept)", abs(mean)])
  plot_dt[lower > cutoff, lower := Inf]
  plot_dt[mean > cutoff, mean := Inf]
  plot_dt[upper > cutoff, upper := Inf]
  plot_dt[lower < (-1)*cutoff, lower := -Inf]
  plot_dt[mean  < (-1)*cutoff, mean := -Inf]
  plot_dt[upper  < (-1)*cutoff, upper := -Inf]
  plot_dt[, country := cur_country]
  plot_dt[, age_group := age_group]
  
  plot_dt <- merge(plot_dt[var != "(Intercept)"], pretty_labs, by = "var")
  plot_dt[, pretty_lab := factor(pretty_lab, levels = pretty_labs[var %in% plot_dt$var]$pretty_lab)]
  plot_dt <- merge(plot_dt,pretty_country_labs, by = "country")
  plot_dt[, measure := factor(measure, levels = c("Coefficient", "Standardized Coefficient", "Relative Importance"))]

  
  ## save data here
  write.csv(plot_dt, file.path(out.dir, paste0(paste(cur_country, age_group, mode, sep="_"), ".csv")),row.names=F)
  
  ## create title here based on parameters
  pretty_age_group 
  model_title <- paste(unique(plot_dt$pretty_country_lab), pretty_age_group, toupper(mode), 'endline')
  
  gg <- ggplot(plot_dt, aes(y = pretty_lab, x = mean, fill = significance)) +
    geom_col() +
    geom_segment(aes(yend = pretty_lab, x = lower, xend = upper)) +
    facet_wrap(~measure, scales = "free_x") +
    labs(title = model_title,
         x = "",
         y = "",
         fill = "")+
    scale_fill_manual(values = c("Not significant" = "#adadad",
                                 "Significantly negative" = "#f2766d",
                                 "Significantly positive" = "#6da9e8"))+
    theme_bw()+
    theme(legend.position = "bottom",
          text = element_text(size=20))
  
  ggsave(file.path(out.dir, paste0(paste(cur_country, age_group, mode, sep="_"), ".png")), width = 15, height =10)
  
}

  
# perform shapely with outcome of any birth or pregnancy in the last 2 years (including terminations, miscarriages, and stillbirths)
form <- any_birth_preg_2_yr_dhs ~ age + wealth_dummies2 + wealth_dummies3 + wealth_dummies4 + wealth_dummies5 +educ_single_yrs + curr_cohabit + had_intercourse + unmet_need

perform_shap('cm', 'dhs', '15_19', form)

perform_shap('mw', 'dhs', '15_19', form)

perform_shap('gh', 'dhs', '15_19', form)

perform_shap('np', 'dhs', '15_19', form)

perform_shap('rw', 'dhs', '15_19', form)

form <- any_birth_preg_2_yr_mics ~ age + wealth_dummies2 + wealth_dummies3 + wealth_dummies4 + wealth_dummies5 +educ_single_yrs + curr_cohabit + had_intercourse +unmet_need
perform_shap('mw', 'mics', '15_19', form)

## plot coefficients and relative importance across countries
files <- list.files(out.dir)
files <- files[grepl("15_19_dhs.csv", files) | grepl("15_19_mics.csv", files)]
files <- files[!grepl("mw_15_19_dhs.csv", files)]

shapely_dt <- rbindlist(lapply(file.path(out.dir, files), fread), fill = T)
ggplot(shapely_dt[measure == "Coefficient"], aes(y = pretty_lab, x = mean, fill = significance)) +
  facet_wrap(~pretty_country_lab, nrow=1)+
  geom_bar(stat='identity') +
  geom_segment(aes(yend = pretty_lab, x = lower, xend = upper)) +
  labs(title = "Shapely coefficients ages 15-19", y = "",
       x = "\n Coefficient",
       fill = "")+
  scale_fill_manual(values = c("Not significant" = "#adadad",
                               "Significantly negative" = "#f2766d",
                               "Significantly positive" = "#6da9e8"))+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size=20),
        axis.text.x = element_text(angle=60, vjust=0))

ggsave(file.path(out.dir, 'all_coef.png'), width = 15, height =10)

ggplot(shapely_dt[measure == "Standardized Coefficient"], aes(y = pretty_lab, x = mean, fill = significance)) +
  facet_wrap(~pretty_country_lab, nrow=1)+
  geom_bar(stat='identity') +
  geom_segment(aes(yend = pretty_lab, x = lower, xend = upper)) +
  labs(title = "Shapely standardized coefficients ages 15-19", y = "",
       x = "\n Standardized Coefficient",
       fill = "")+
  scale_fill_manual(values = c("Not significant" = "#adadad",
                               "Significantly negative" = "#f2766d",
                               "Significantly positive" = "#6da9e8"))+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size=20),
        axis.text.x = element_text(angle=60, vjust=0))

ggsave(file.path(out.dir, 'all_std_coef.png'), width = 15, height =10)

ggplot(shapely_dt[measure == "Relative Importance"], aes(y = pretty_lab, x = mean, fill = significance)) +
  facet_wrap(~pretty_country_lab, nrow=1)+
  geom_bar(stat='identity') +
  geom_segment(aes(yend = pretty_lab, x = lower, xend = upper)) +
  labs(title = "Shapely relative importance ages 15-19", y = "",
       x = "\n Relative importance",
       fill = "")+
  scale_fill_manual(values = c("Not significant" = "#adadad",
                               "Significantly negative" = "#f2766d",
                               "Significantly positive" = "#6da9e8"))+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size=20),
        axis.text.x = element_text(angle=60, vjust=0))

ggsave(file.path(out.dir, 'rel_imp_coef.png'), width = 15, height =10)


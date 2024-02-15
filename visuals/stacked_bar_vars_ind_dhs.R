## Created by: Corinne Bintz
## Creation date: 1/3/2023
## TODO: change ggsave to be one pdf 
## create diagnostic plots a series of variables in the ASHER decomp for baseline and endline: individual level data
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
pacman::p_load(data.table,magrittr,tidyverse,parallel,ggrepel,plyr,viridis,scales,ggridges,openxlsx,readxl,ggpubr,dplyr,RColorBrewer, haven, survey,tidyr)

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source)) # IHME-specific line. comment out if running outside IHME

in.dir <- '/share/scratch/projects/hssa/asher/processed/'
out.dir <- '/share/scratch/projects/hssa/asher/plots/'

'%ni%' <- Negate('%in%')

## read in data
merged_dt <- fread(file.path(in.dir, 'merged_baseline_endline_dhs.csv'))

merged_dt[, country := substr(country, 1,2)]
merged_dt[, baseline := min(year), by = "country"]
merged_dt[, latest := max(year), by = "country"]
merged_dt[, year_type := ifelse(year == baseline, "baseline", "latest")]

continuous_vars <- c("age", "age_1st_birth", "age_1st_sex_imp","ideal_child", "knowledge_mod","gap_sex_mar")

prop_vars <- c("curr_cohabit", "decision_use_joint", "decision_use_respondent",
                     "decision_use_joint_respondent", "desire_child_teen", "fp_exp_media", 
                     "mcpr", "never_had_intercourse","fp_facility12m", "fp_hcw12m", "fp_dealse", "fp_othermethod", "fp_se","any_birth_preg")


## function to organize means table into means and se when there is only one variable in the svy mean call 
organize_mean_se_singular <- function(dt,variable_name ){
  means_tmp <-  dt[variable %ni% c("se", "ci_l", "ci_u")]
  setnames(means_tmp, "value", "mean")
  
  se_tmp <-  dt[variable == "se"]
  setnames(se_tmp, "value", "se")
  
  se_ci_l <-  dt[variable == "ci_l"]
  setnames(se_ci_l, "value", "ci_l")
  
  se_ci_u <-  dt[variable == "ci_u"]
  setnames(se_ci_u, "value", "ci_u")
  
  se_tmp[, variable := variable_name]
  se_ci_l[, variable := variable_name]
  se_ci_u[, variable := variable_name]
  
  mean_se <- merge(means_tmp,se_tmp, by = c("variable", "year"))
  mean_se <- merge(mean_se,se_ci_l, by = c("variable", "year"))
  mean_se <- merge(mean_se,se_ci_u, by = c("variable", "year"))
  
  return(mean_se)
}

### LEVEL --------

create_data <- function(var, age_start, age_end){
  all_ind_means <- data.table()
  countries <- c("cm", "mw", "np", "rw")
  for (cur_country in countries) {
    options(survey.adjust.domain.lonely=TRUE)
    options(survey.lonely.psu="adjust")
    current_data <- merged_dt[country ==cur_country]
    first_year <- min(current_data$year)
    last_year <- max(current_data$year)
    dhs_svy <- svydesign(id= ~psu, strata = ~strata, weights = ~pweight, data = merged_dt[country ==cur_country & age %in% c(seq(age_start,age_end)) ], nest = TRUE)
    means_p1 <- svyby(~get(var), ~year, dhs_svy , svymean, na.rm =T, vartype = c("se", "ci")) 
    
    means_p1_long <- data.table(melt(means_p1, id.var = "year"))
    means_p1_long[variable == "get(var)", variable :=var]
    
    mean_se_p1 <- organize_mean_se_singular(means_p1_long, eval(var))
    
    mean_se_p1[, country := cur_country]
    all_ind_means <- rbind(all_ind_means, mean_se_p1)
  }
  
  # variable labels 
  variable_labs <- list("age" = "Age",
                        "age_1st_birth" = "Age at first birth",
                        "age_1st_sex_imp" = "Age at first intercourse",
                        "ideal_child" = "Ideal number of children",
                        "knowledge_mod" = "Number of modern contraceptive methods known",
                        "gap_sex_mar" = "Average gap in years between median age at first \n intercourse and first marriage among those ever married",
                        "curr_cohabit" = "Currently married/in-union", 
                        "decision_use_joint" = "Percentage of  women 15-19 years who make decisions \n regarding contraceptive use with their partner",
                        "decision_use_respondent" = "Percentage of women 15-19 years who make decisions \n regarding contraceptive use on their own",
                        "decision_use_joint_respondent" = "Percentage of women 15-19 years who make decisions \n regarding contraceptive use with their \n partner or on their own",
                        "desire_child_teen" = 'Percentage reporting desire for other child \n within adolescence (15-19)' , 
                        "fp_exp_media" = "Percentage of women exposed to media for \n family planning", 
                        "mcpr" = "Percentage of women using modern contraception",
                        "never_had_intercourse" = "Percentage of women who report \n never having intercourse",
                        "fp_facility12m" = "Percentage of women who report they have visited \n a health facility in last 12 months",
                        "fp_hcw12m" = "Percentage of women who report they have been \n visited by a family planning worker in last 12 months",
                        "fp_dealse" = "Percentage of women who report they were told how to deal \n with side effects \n at the time they received contraception",
                        "fp_othermethod"= "Percentage of women who report they were told \n about other methods at the time they \n received contraception",
                        "fp_se"= "Percentage of women who report they were told about \n side effects at the time they \n received contraception",
                        "any_birth_preg" = "Percentage of women who have had any live birth \n or are currently pregnant")
  
  # country labels
  country_labs <- list("cm" = "Cameroon",
                       "gh" = "Ghana",
                       "mw" = "Malawi",
                       "np" = "Nepal",
                       "rw" = "Rwanda")
  all_ind_means[,variable := as.character(variable)]
  all_ind_means[, variable := unlist(variable_labs[variable])]
  all_ind_means[, country_lab := unlist(country_labs[country])]
  all_ind_means[, baseline := min(year), by = "country"]
  all_ind_means[, latest := max(year), by = "country"]
  all_ind_means[, year_type := ifelse(year == baseline, "baseline", "latest")]
  
  return(all_ind_means)
}



plot_continuous_level <- function(data, age_start, age_end){
  variable <- unique(data$variable)
  
  
  if (variable %in% c("Age", "Age at first intercourse")) {
    if (variable == "Age") {
      
      if (age_end == 19){
        data[, mean := mean -15]
        data[, ci_u := ci_u -15]
        data[, ci_l :=  ci_l -15]
        
        breaks = c(1,2)
        labs = c(16,17)
      } else if (age_end == 24){
        data[, mean := mean -17]
        data[, ci_u := ci_u -17]
        data[, ci_l :=  ci_l -17] 
        
        breaks = c(1,2,3)
        labs = c(18,19,20)
      }else{
        data[, mean := mean -26]
        data[, ci_u := ci_u -26]
        data[, ci_l :=  ci_l -26]
        
        breaks = c(1,2,3)
        labs = c(27, 28,29)
      }
    }
    if (variable == "Age at first intercourse") {
      
      
      data[, mean := mean -14]
      data[, ci_u := ci_u -14]
      data[, ci_l :=  ci_l -14]
      
      breaks = c(1,2,3)
      labs = c(15,16,17)
      
    }
    
    min_val <- min(data$ci_l) 
    max_val <- max(data$ci_u) 
    var_lab <- unique(data$variable)
    ## plot all countries at once: continuous 
    gg <- ggplot(data= data, aes(x = year_type, y = mean))+
      facet_wrap(~country_lab)+
      scale_y_continuous(paste0("Mean value, ", age_start, "-", age_end), limits = c(0, max_val+0.5), breaks = breaks, labels = labs) +
      geom_bar(stat = 'identity', aes(fill = country_lab))+
      theme_bw()   + theme(legend.position="none",panel.spacing.x = unit(4, "mm"), strip.text = element_text(size = 12),
                           plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                           axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
      guides(fill = guide_legend(ncol = 3)) +
      geom_errorbar(aes(ymin =ci_l, ymax = ci_u))+
      ggtitle(paste0(var_lab, ", ", age_start, "-", age_end))
  } else {
    min_val <- min(data$ci_l) 
    min_val <- ifelse(min_val <0, min_val, 0)
    max_val <- max(data$ci_u) 
    if (variable %in% c("Number of births in the past 3 years","Number of births in the past 5 years")){
      max_val <- max(data$ci_u) 
    } else{
      max_val <- max_val+0.5
    }
    
    var_lab <- unique(data$variable)
    
    ## plot all countries at once: continuous 
    gg <- ggplot(data= data, aes(x = year_type, y = mean))+
      facet_wrap(~country_lab)+
      scale_y_continuous(paste0("Mean value, ", age_start, "-", age_end), limits = c(min_val, max_val)) +
      geom_bar(stat = 'identity', aes(fill = country_lab))+
      theme_bw()   + theme(legend.position="none",panel.spacing.x = unit(4, "mm"), strip.text = element_text(size = 12),
                           plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                           axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
      guides(fill = guide_legend(ncol = 3)) +
      geom_errorbar(aes(ymin =ci_l, ymax = ci_u))+
      ggtitle(paste0(var_lab, ", ", age_start, "-", age_end))
  }
  print(gg)
}

plot_prop_level <- function(data, age_start, age_end){
  min_val <- ifelse(min(data$ci_l) < 0, min(data$ci_l) -0.05, 0)
  max_val <- max(data$ci_u) 
  var_lab <- unique(data$variable)
  ## plot all countries at once: proportion 
  gg <- ggplot(data= data, aes(x = year_type, y = mean))+
    facet_wrap(~country_lab)+
    scale_y_continuous(paste0("Percent of women, ", age_start, "-", age_end), label = percent, expand = c(0,0), limits = c(min_val, max_val+0.05)) +
    geom_bar(stat = 'identity', aes(fill = country_lab))+
    labs(fill = "Method") +
    theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                         legend.position = "none", strip.text = element_text(size = 12),
                         plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                         axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
    guides(fill = guide_legend(ncol = 3)) +
    geom_errorbar(aes(ymin =ci_l, ymax = ci_u))+
    ggtitle(paste0(var_lab, ", ", age_start, "-", age_end))
  print(gg)
}

## here open pdf
pdf(file = file.path(out.dir, 'diagnostic_vars_asher_dhs.pdf'))
for (var in continuous_vars){
  print(var)
  data <- create_data(var, 15,19) 
  plot_continuous_level(data, 15,19)
}

for (var in prop_vars){
  print(var)
  data <- create_data(var, 15,19) 
  plot_prop_level(data, 15,19)
}

for (var in continuous_vars){
  print(var)
  data <- create_data(var, 15,24) 
  plot_continuous_level(data, 15,24)
}

for (var in prop_vars){
  print(var)
  data <- create_data(var, 15,24) 
  plot_prop_level(data, 15,24)
}


for (var in continuous_vars){
  print(var)
  data <- create_data(var, 15,49) 
  plot_continuous_level(data, 15,49)
}

for (var in prop_vars){
  print(var)
  data <- create_data(var, 15,49) 
  plot_prop_level(data, 15,49)
}


## ---DIFF-----
plot_continuous_diff <- function(data, age_start, age_end){
  var_lab <- unique(data$variable)
  
  #long to wide
  data_wide <- dcast(data=data, variable + country+country_lab~year_type, value.var = "mean")
  
  data_wide[, diff := latest-baseline]
  min_val <- ifelse(min(data_wide$diff) < 0, min(data_wide$diff) -1, 0)
  max_val <- max(data_wide$diff) 
  
  ## plot all countries at once: continuous 
  gg <- ggplot(data= data_wide, aes(x = country_lab, y = diff))+
    scale_y_continuous(paste0("Mean value, ", age_start, "-", age_end), expand = c(0,0), limits = c(min_val, max_val+1)) +
    geom_bar(stat = 'identity', aes(fill = country_lab))+
    theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                         legend.position = "none", strip.text = element_text(size = 12),
                         plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                         axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
    guides(fill = guide_legend(ncol = 3)) +
    labs(y = "Absolute difference endline to baseline")+
    ggtitle(paste0(var_lab, ", ", age_start, "-", age_end))
  print(gg)
}

plot_prop_diff <- function(data, age_start, age_end){
  var_lab <- unique(data$variable)
  
  #long to wide
  data_wide <- dcast(data=data, variable + country+country_lab~year_type, value.var = "mean")
  
  data_wide[, diff := latest-baseline]
  min_val <- ifelse(min(data_wide$diff) < 0, min(data_wide$diff) -0.05, 0)
  max_val <- max(data_wide$diff) 
  ## plot all countries at once: proportion 
  gg <- ggplot(data= data_wide, aes(x = country_lab, y = diff))+
    scale_y_continuous(paste0("Difference in percent of women, ", age_start, "-", age_end, ", baseline to endline"), label = percent, expand = c(0,0), limits = c(min_val, max_val+0.05)) +
    geom_bar(stat = 'identity', aes(fill = country_lab))+
    theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                         legend.position = "none", strip.text = element_text(size = 12),
                         plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                         axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
    guides(fill = guide_legend(ncol = 3)) +
    ggtitle(paste0(var_lab, ", ", age_start, "-", age_end))
    print(gg)
}



for (var in continuous_vars){
  print(var)
  data <- create_data(var, 15,19) 
  plot_continuous_diff(data, 15,19)
}

for (var in prop_vars){
  print(var)
  data <- create_data(var, 15,19) 
  plot_prop_diff(data, 15,19)
}


for (var in continuous_vars){
  print(var)
  data <- create_data(var, 15,24) 
  plot_continuous_diff(data, 15,24)
}

for (var in prop_vars){
  print(var)
  data <- create_data(var, 15,24) 
  plot_prop_diff(data, 15,24)
}


for (var in continuous_vars){
  print(var)
  data <- create_data(var, 15,49) 
  plot_continuous_diff(data, 15,49)
}

for (var in prop_vars){
  print(var)
  data <- create_data(var, 15,49) 
  plot_prop_diff(data, 15,49)
}

# here close pdf
dev.off()

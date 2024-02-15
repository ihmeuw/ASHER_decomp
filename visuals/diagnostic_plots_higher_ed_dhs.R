## Created by: Corinne Bintz
## Creation date: 12/27/2023
## create diagnostic plots for higher education in ASHER decomp: with DHS as endline for Malawi
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

## read in individual data
ind_dt <- fread(file.path(in.dir, 'ind_dt_baseline_dhs.csv'))

## filter to 15-49 year olds 
ind_dt <- ind_dt[age %in% c(seq(15,49))]
ind_dt[, country := substr(country, 1,2)]
ind_dt[, baseline := min(year), by = "country"]
ind_dt[, latest := max(year), by = "country"]
ind_dt[, year_type := ifelse(year == baseline, "baseline", "latest")]

## if unmet need, mark this as current contraceptive method for method mix plots. if not unmet need and current method is none, make NA because we don't want to plot these people
ind_dt[, current_method := ifelse(current_method == "none" & unmet_need != 1,NA, current_method)]

## function to organize means table into means and se when there are mulitple variables in the svy mean call 
organize_mean_se_multiple <- function(dt){
  means_tmp <-  dt[!grepl("\\<se.", dt$variable)]
  setnames(means_tmp, "value", "mean")
  
  se_tmp <-  dt[grepl("se.", dt$variable)]
  setnames(se_tmp, "value", "se")
  
  
  se_tmp[, variable := str_replace(variable,"se.", "")]
  
  mean_se <- merge(means_tmp,se_tmp, by = c("variable", "year"))
  return(mean_se)
}

####### ----- PLOT 15-19 -------
means_15_19 <- data.table()
countries <- c("cm", "mw", "np", "rw", "gh")
for (cur_country in countries) {
  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")
  current_data <- ind_dt[country ==cur_country & age %in% c(seq(15,19))]
  first_year <- min(current_data$year)
  last_year <- max(current_data$year)
  dhs_svy <- svydesign(id= ~psu, strata = ~strata, weights = ~pweight, data = ind_dt[country ==cur_country], nest = TRUE)
  means <- svyby(~highest_ed_level, ~year, dhs_svy , svymean, na.rm =T) 

  means_long <- data.table(melt(means, id.var = "year"))

  mean_se <- organize_mean_se_multiple(means_long)

  mean_se[, country := cur_country]
  means_15_19 <- rbind(means_15_19, mean_se)
}


## rename highest ed variable
means_15_19[, variable := str_to_title(str_replace(variable, 'highest_ed_level', ''))]
means_15_19$variable <- factor(means_15_19$variable , levels = c("Less_than_primary",
                                                                 "Primary",
                                                                 "Secondary",
                                                                 "Tertiary"),
                               labels =  c("Less than primary",
                                           "Primary",
                                           "Secondary",
                                           "Tertiary"))

# method colors 
ed_cols <-  c("Less than primary" = "#E30E0E", # dark red
                  "Primary" = "#FA7D7A", # light red
                  "Secondary" = "#FA8D20", # orange
                  "Tertiary" = "#FFCC12") # yellow
# country labels
country_labs <- list("cm" = "Cameroon",
                     "gh" = "Ghana",
                     "mw" = "Malawi",
                     "np" = "Nepal",
                     "rw" = "Rwanda")

means_15_19[, country_lab := unlist(country_labs[country])]

# method labels
means_15_19[, baseline := min(year), by = "country"]
means_15_19[, latest := max(year), by = "country"]
means_15_19[, year_type := ifelse(year == baseline, "baseline", "latest")]

## plot all countries at once
ggplot(data= means_15_19, aes(x = year_type, y = mean, fill = variable))+
  facet_wrap(~country_lab)+
  scale_y_continuous("Percent of population", label = percent, expand = c(0,0)) +
  geom_bar(stat = 'identity')+
  scale_fill_manual(values = ed_cols) +
  labs(fill = "Method") +
  theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                       legend.position = "bottom", legend.title = element_blank(), legend.key = element_rect(colour = "black", size = .3),
                       legend.text = element_text(size = 11), strip.text = element_text(size = 12),
                       plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                       axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
  guides(fill = guide_legend(ncol = 3)) +
  ggtitle("Highest education breakdown, 15-19")
ggsave(file.path(out.dir, 'higher_ed_all_15_19_dhs.png'), width = 10, height =10)

## plot each country separately
for (cur_country in c("cm", "mw", "np", "rw",'gh')){
  ggplot(data= means_15_19[country==cur_country], aes(x = year_type, y = mean, fill = variable))+
    scale_y_continuous("Percent of population", label = percent, expand = c(0,0)) +
    geom_bar(stat = 'identity')+
    scale_fill_manual(values = ed_cols) +
    labs(fill = "Method") +
    theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                         legend.position = "bottom", legend.title = element_blank(), legend.key = element_rect(colour = "black", size = .3),
                         legend.text = element_text(size = 11), strip.text = element_text(size = 12),
                         plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                         axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
    guides(fill = guide_legend(ncol = 3)) +
    ggtitle(paste("Highest education breakdown, 15-19,", unique(means_15_19[country==cur_country]$country_lab)))
  title <- paste0('higher_ed_', cur_country, '_15_19_dhs.png')
  ggsave(file.path(out.dir, title), width = 10, height =10)
}
####### ----- PLOT 15-24 -------
means_15_24 <- data.table()
countries <- c("cm", "mw", "np", "rw", 'gh')
for (cur_country in countries) {
  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")
  current_data <- ind_dt[country ==cur_country & age %in% c(seq(15,24))]
  first_year <- min(current_data$year)
  last_year <- max(current_data$year)
  dhs_svy <- svydesign(id= ~psu, strata = ~strata, weights = ~pweight, data = ind_dt[country ==cur_country], nest = TRUE)
  means <- svyby(~highest_ed_level, ~year, dhs_svy , svymean, na.rm =T) 
  
  means_long <- data.table(melt(means, id.var = "year"))
  
  mean_se <- organize_mean_se_multiple(means_long)
  
  mean_se[, country := cur_country]
  means_15_24 <- rbind(means_15_24, mean_se)
}


## rename highest ed variable
means_15_24[, variable := str_to_title(str_replace(variable, 'highest_ed_level', ''))]
means_15_24$variable <- factor(means_15_24$variable , levels = c("Less_than_primary",
                                                                 "Primary",
                                                                 "Secondary",
                                                                 "Tertiary"),
                               labels =  c("Less than primary",
                                           "Primary",
                                           "Secondary",
                                           "Tertiary"))
means_15_24[, country_lab := unlist(country_labs[country])]

# method labels
means_15_24[, baseline := min(year), by = "country"]
means_15_24[, latest := max(year), by = "country"]
means_15_24[, year_type := ifelse(year == baseline, "baseline", "latest")]

## plot all countries at once
ggplot(data= means_15_24, aes(x = year_type, y = mean, fill = variable))+
  facet_wrap(~country_lab)+
  scale_y_continuous("Percent of population", label = percent, expand = c(0,0)) +
  geom_bar(stat = 'identity')+
  scale_fill_manual(values = ed_cols) +
  labs(fill = "Method") +
  theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                       legend.position = "bottom", legend.title = element_blank(), legend.key = element_rect(colour = "black", size = .3),
                       legend.text = element_text(size = 11), strip.text = element_text(size = 12),
                       plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                       axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
  guides(fill = guide_legend(ncol = 3)) +
  ggtitle("Highest education breakdown, 15-24")
ggsave(file.path(out.dir, 'higher_ed_all_15_24.png'), width = 10, height =10)

## plot each country separately
for (cur_country in c("cm", "mw", "np", "rw",'gh')){
  ggplot(data= means_15_24[country==cur_country], aes(x = year_type, y = mean, fill = variable))+
    scale_y_continuous("Percent of population", label = percent, expand = c(0,0)) +
    geom_bar(stat = 'identity')+
    scale_fill_manual(values = ed_cols) +
    labs(fill = "Method") +
    theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                         legend.position = "bottom", legend.title = element_blank(), legend.key = element_rect(colour = "black", size = .3),
                         legend.text = element_text(size = 11), strip.text = element_text(size = 12),
                         plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                         axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
    guides(fill = guide_legend(ncol = 3)) +
    ggtitle(paste("Highest education breakdown, 15-24,", unique(means_15_24[country==cur_country]$country_lab)))
  title <- paste0('higher_ed_', cur_country, '_15_24_dhs.png')
  ggsave(file.path(out.dir, title), width = 10, height =10)
}

####### ----- PLOT 15-49 -------
means_15_49 <- data.table()
countries <- c("cm", "mw", "np", "rw",'gh')
for (cur_country in countries) {
  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")
  current_data <- ind_dt[country ==cur_country]
  first_year <- min(current_data$year)
  last_year <- max(current_data$year)
  dhs_svy <- svydesign(id= ~psu, strata = ~strata, weights = ~pweight, data = ind_dt[country ==cur_country], nest = TRUE)
  means <- svyby(~highest_ed_level, ~year, dhs_svy , svymean, na.rm =T) 
  
  means_long <- data.table(melt(means, id.var = "year"))
  
  mean_se <- organize_mean_se_multiple(means_long)
  
  mean_se[, country := cur_country]
  means_15_49 <- rbind(means_15_49, mean_se)
}


## rename highest ed variable
means_15_49[, variable := str_to_title(str_replace(variable, 'highest_ed_level', ''))]
means_15_49$variable <- factor(means_15_49$variable , levels = c("Less_than_primary",
                                                                 "Primary",
                                                                 "Secondary",
                                                                 "Tertiary"),
                               labels =  c("Less than primary",
                                           "Primary",
                                           "Secondary",
                                           "Tertiary"))

means_15_49[, country_lab := unlist(country_labs[country])]

# method labels
means_15_49[, baseline := min(year), by = "country"]
means_15_49[, latest := max(year), by = "country"]
means_15_49[, year_type := ifelse(year == baseline, "baseline", "latest")]

## plot all countries at once
ggplot(data= means_15_49, aes(x = year_type, y = mean, fill = variable))+
  facet_wrap(~country_lab)+
  scale_y_continuous("Percent of population", label = percent, expand = c(0,0)) +
  geom_bar(stat = 'identity')+
  scale_fill_manual(values = ed_cols) +
  labs(fill = "Method") +
  theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                       legend.position = "bottom", legend.title = element_blank(), legend.key = element_rect(colour = "black", size = .3),
                       legend.text = element_text(size = 11), strip.text = element_text(size = 12),
                       plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                       axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
  guides(fill = guide_legend(ncol = 3)) +
  ggtitle("Highest education breakdown, 15-49")
ggsave(file.path(out.dir, 'higher_ed_all_15_49_dhs.png'), width = 10, height =10)

## plot each country separately
for (cur_country in c("cm", "mw", "np", "rw",'gh')){
  ggplot(data= means_15_49[country==cur_country], aes(x = year_type, y = mean, fill = variable))+
    scale_y_continuous("Percent of population", label = percent, expand = c(0,0)) +
    geom_bar(stat = 'identity')+
    scale_fill_manual(values = ed_cols) +
    labs(fill = "Method") +
    theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                         legend.position = "bottom", legend.title = element_blank(), legend.key = element_rect(colour = "black", size = .3),
                         legend.text = element_text(size = 11), strip.text = element_text(size = 12),
                         plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                         axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
    guides(fill = guide_legend(ncol = 3)) +
    ggtitle(paste("Highest education breakdown, 15-49,", unique(means_15_49[country==cur_country]$country_lab)))
  title <- paste0('higher_ed_', cur_country, '_15_49_dhs.png')
  ggsave(file.path(out.dir, title), width = 10, height =10)
}




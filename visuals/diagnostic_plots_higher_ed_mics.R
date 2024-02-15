## Created by: Corinne Bintz
## Creation date: 12/27/2023
## create diagnostic plots for higher education in ASHER decomp  
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


## read in individual data
ind_dt <- fread(file.path(in.dir, 'ind_dt_baseline_mics.csv'))

## filter to 15-49 year olds 
ind_dt <- ind_dt[age %in% c(seq(15,49))]
ind_dt[, country := substr(country, 1,2)]
ind_dt[, baseline := min(year), by = "country"]
ind_dt[, latest := max(year), by = "country"]
ind_dt[, year_type := ifelse(year == baseline, "baseline", "latest")]

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

create_data <- function(age_start, age_end){
  means_dt <- data.table()
  countries <- c("cm", "mw", "np", "rw", "gh")
  for (cur_country in countries) {
    options(survey.adjust.domain.lonely=TRUE)
    options(survey.lonely.psu="adjust")
    current_data <- ind_dt[country ==cur_country & age %in% c(seq(age_start,age_end))]
    first_year <- min(current_data$year)
    last_year <- max(current_data$year)
    dhs_svy <- svydesign(id= ~psu, strata = ~strata, weights = ~pweight, data = ind_dt[country ==cur_country], nest = TRUE)
    means_a <- svyby(~highest_ed_level, ~year, subset(dhs_svy, year == first_year) , svymean, na.rm =T) 
    means_b <- svyby(~highest_ed_level, ~year, subset(dhs_svy, year == last_year) , svymean, na.rm =T) 
    
    means_a_long <- data.table(melt(means_a, id.var = "year"))
    means_b_long <- data.table(melt(means_b, id.var = "year"))
    
    mean_se_a <- organize_mean_se_multiple(means_a_long)
    mean_se_b <- organize_mean_se_multiple(means_b_long)
    
    mean_se <- rbind(mean_se_a, mean_se_b)
    mean_se[, country := cur_country]
    means_dt <- rbind(means_dt, mean_se)
  }
  ## rename highest ed variable
  means_dt[, variable := str_to_title(str_replace(variable, 'highest_ed_level', ''))]
  means_dt$variable <- factor(means_dt$variable , levels = c("Less_than_primary",
                                                                   "Primary",
                                                                   "Secondary",
                                                                   "Tertiary"),
                                 labels =  c("Less than primary",
                                             "Primary",
                                             "Secondary",
                                             "Tertiary"))
  means_dt[, country_lab := unlist(country_labs[country])]
  
  # method labels
  means_dt[, baseline := min(year), by = "country"]
  means_dt[, latest := max(year), by = "country"]
  means_dt[, year_type := ifelse(year == baseline, "baseline", "latest")]
  
  return(means_dt)
}

means_15_19 <- create_data(15,19)

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
ggsave(file.path(out.dir, 'higher_ed_all_15_19_mics.png'), width = 10, height =10)

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
  title <- paste0('higher_ed_', cur_country, '_15_19_mics.png')
  ggsave(file.path(out.dir, title), width = 10, height =10)
}
####### ----- PLOT 15-24 -------
means_15_24 <- create_data(15,24)
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
ggsave(file.path(out.dir, 'higher_ed_all_15_24_mics.png'), width = 10, height =10)

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
  title <- paste0('higher_ed_', cur_country, '_15_24_mics.png')
  ggsave(file.path(out.dir, title), width = 10, height =10)
}

####### ----- PLOT 15-49 -------
means_15_49 <- create_data(15,49)

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
ggsave(file.path(out.dir, 'higher_ed_all_15_49_mics.png'), width = 10, height =10)

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
  title <- paste0('higher_ed_', cur_country, '_15_49_mics.png')
  ggsave(file.path(out.dir, title), width = 10, height =10)
}




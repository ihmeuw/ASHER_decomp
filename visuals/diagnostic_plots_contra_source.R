## Created by: Corinne Bintz
## Creation date: 1/5/24
## create diagnostic plots for higher education in ASHER decomp: contra source: with DHS as endline for MICS
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
hs_dt <- fread(file.path(in.dir, 'hs_dt_baseline.csv'))

## filter to 15-49 year olds 
hs_dt <- hs_dt[age %in% c(seq(15,49))]
hs_dt[, country := substr(country, 1,2)]
hs_dt[, baseline := min(year), by = "country"]
hs_dt[, latest := max(year), by = "country"]
hs_dt[, year_type := ifelse(year == baseline, "baseline", "latest")]

## function to organize means table into means and se when there is only one variable in the svy mean call 
organize_mean_se_singular <- function(dt,variable_name ){
  means_tmp <-  dt[variable != "se"]
  setnames(means_tmp, "value", "mean")
  
  se_tmp <-  dt[variable == "se"]
  setnames(se_tmp, "value", "se")
  
  
  se_tmp[, variable := variable_name]
  
  mean_se <- merge(means_tmp,se_tmp, by = c("variable", "year"))
  return(mean_se)
}

####### ----- PLOT 15-19 -------
means_15_19 <- data.table()
countries <- c("cm", "mw", "np", "rw")
for (cur_country in countries) {
  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")
  current_data <- hs_dt[country ==cur_country & age %in% c(seq(15,19))]
  first_year <- min(current_data$year)
  last_year <- max(current_data$year)
  dhs_svy <- svydesign(id= ~psu, strata = ~strata, weights = ~pweight, data = hs_dt[country ==cur_country], nest = TRUE)
  means_p1 <- svyby(~contra_source_public, ~year, dhs_svy , svymean, na.rm =T) 
  means_p2 <- svyby(~contra_source_priv, ~year, dhs_svy , svymean, na.rm =T) 
  means_p3 <- svyby(~contra_source_other, ~year, dhs_svy , svymean, na.rm =T) 
  
  means_p1_long <- data.table(melt(means_p1, id.var = "year"))
  means_p2_long <- data.table(melt(means_p2, id.var = "year"))
  means_p3_long <- data.table(melt(means_p3, id.var = "year"))
  
  mean_p1_se <- organize_mean_se_singular(means_p1_long, "contra_source_public")
  mean_p2_se <- organize_mean_se_singular(means_p2_long, "contra_source_priv")
  mean_p3_se <- organize_mean_se_singular(means_p3_long, "contra_source_other")
  
  mean_se <- rbind(mean_p1_se, mean_p2_se, mean_p3_se)
  
  mean_se[, country := cur_country]
  means_15_19 <- rbind(means_15_19, mean_se)
}

## do ghana separately for now
countries <- c("gh")
for (cur_country in countries) {
  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")
  current_data <- hs_dt[country ==cur_country & age %in% c(seq(15,19))]
  dhs_svy <- svydesign(id= ~psu, strata = ~strata, weights = ~pweight, data = current_data, nest = TRUE)
  means_p1 <- svyby(~contra_source_public, ~year, dhs_svy , svymean, na.rm =T) 
  means_p2 <- svyby(~contra_source_priv, ~year, dhs_svy , svymean, na.rm =T) 
  means_p3 <- svyby(~contra_source_other, ~year, dhs_svy , svymean, na.rm =T) 
  
  means_p1_long <- data.table(melt(means_p1, id.var = "year"))
  means_p2_long <- data.table(melt(means_p2, id.var = "year"))
  means_p3_long <- data.table(melt(means_p3, id.var = "year"))
  
  mean_p1_se <- organize_mean_se_singular(means_p1_long, "contra_source_public")
  mean_p2_se <- organize_mean_se_singular(means_p2_long, "contra_source_priv")
  mean_p3_se <- organize_mean_se_singular(means_p3_long, "contra_source_other")
  
  mean_se <- rbind(mean_p1_se, mean_p2_se, mean_p3_se)
  
  mean_se[, country := cur_country]
  means_15_19 <- rbind(means_15_19, mean_se)
}

## rename highest ed variable
means_15_19$variable <- factor(means_15_19$variable , levels = c("contra_source_public",
                                                                 "contra_source_priv",
                                                                 "contra_source_other"),
                               labels =  c("Public",
                                           "Private",
                                           "Other"))

# method colors 
var_cols <-  c("Public" = "#E30E0E", # dark red
              "Private" = "#FA7D7A", # light red
              "Other" = "#FA8D20") # orange
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
  scale_fill_manual(values = var_cols) +
  labs(fill = "Method") +
  theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                       legend.position = "bottom", legend.title = element_blank(), legend.key = element_rect(colour = "black", size = .3),
                       legend.text = element_text(size = 11), strip.text = element_text(size = 12),
                       plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                       axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
  guides(fill = guide_legend(ncol = 3)) +
  ggtitle("Most recent source of contraception, 15-19")
ggsave(file.path(out.dir, 'contra_source_15_19.png'), width = 10, height =10)

## plot each country separately
for (cur_country in c("cm", "mw", "np", "rw",'gh')){
  ggplot(data= means_15_19[country==cur_country], aes(x = year_type, y = mean, fill = variable))+
    scale_y_continuous("Percent of population", label = percent, expand = c(0,0)) +
    geom_bar(stat = 'identity')+
    scale_fill_manual(values = var_cols) +
    labs(fill = "Method") +
    theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                         legend.position = "bottom", legend.title = element_blank(), legend.key = element_rect(colour = "black", size = .3),
                         legend.text = element_text(size = 11), strip.text = element_text(size = 12),
                         plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                         axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
    guides(fill = guide_legend(ncol = 3)) +
    ggtitle(paste("Most recent source of contraception, 15-19,", unique(means_15_19[country==cur_country]$country_lab)))
  title <- paste0('contra_source_', cur_country, '_15_19.png')
  ggsave(file.path(out.dir, title), width = 10, height =10)
}
####### ----- PLOT 15-24 -------
means_15_24 <- data.table()
countries <- c("cm", "mw", "np", "rw")
for (cur_country in countries) {
  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")
  current_data <- hs_dt[country ==cur_country & age %in% c(seq(15,24))]
  first_year <- min(current_data$year)
  last_year <- max(current_data$year)
  dhs_svy <- svydesign(id= ~psu, strata = ~strata, weights = ~pweight, data = hs_dt[country ==cur_country], nest = TRUE)
  means_p1 <- svyby(~contra_source_public, ~year, dhs_svy , svymean, na.rm =T) 
  means_p2 <- svyby(~contra_source_priv, ~year, dhs_svy , svymean, na.rm =T) 
  means_p3 <- svyby(~contra_source_other, ~year, dhs_svy , svymean, na.rm =T) 
  
  means_p1_long <- data.table(melt(means_p1, id.var = "year"))
  means_p2_long <- data.table(melt(means_p2, id.var = "year"))
  means_p3_long <- data.table(melt(means_p3, id.var = "year"))
  
  mean_p1_se <- organize_mean_se_singular(means_p1_long, "contra_source_public")
  mean_p2_se <- organize_mean_se_singular(means_p2_long, "contra_source_priv")
  mean_p3_se <- organize_mean_se_singular(means_p3_long, "contra_source_other")
  
  mean_se <- rbind(mean_p1_se, mean_p2_se, mean_p3_se)
  
  mean_se[, country := cur_country]
  means_15_24 <- rbind(means_15_24, mean_se)
}

## do ghana separately for now
countries <- c("gh")
for (cur_country in countries) {
  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")
  current_data <- hs_dt[country ==cur_country & age %in% c(seq(15,24))]
  dhs_svy <- svydesign(id= ~psu, strata = ~strata, weights = ~pweight, data = current_data, nest = TRUE)
  means_p1 <- svyby(~contra_source_public, ~year, dhs_svy , svymean, na.rm =T) 
  means_p2 <- svyby(~contra_source_priv, ~year, dhs_svy , svymean, na.rm =T) 
  means_p3 <- svyby(~contra_source_other, ~year, dhs_svy , svymean, na.rm =T) 
  
  means_p1_long <- data.table(melt(means_p1, id.var = "year"))
  means_p2_long <- data.table(melt(means_p2, id.var = "year"))
  means_p3_long <- data.table(melt(means_p3, id.var = "year"))
  
  mean_p1_se <- organize_mean_se_singular(means_p1_long, "contra_source_public")
  mean_p2_se <- organize_mean_se_singular(means_p2_long, "contra_source_priv")
  mean_p3_se <- organize_mean_se_singular(means_p3_long, "contra_source_other")
  
  mean_se <- rbind(mean_p1_se, mean_p2_se, mean_p3_se)
  
  mean_se[, country := cur_country]
  means_15_24 <- rbind(means_15_24, mean_se)
}

## rename highest ed variable
means_15_24$variable <- factor(means_15_24$variable , levels = c("contra_source_public",
                                                                 "contra_source_priv",
                                                                 "contra_source_other"),
                               labels =  c("Public",
                                           "Private",
                                           "Other"))

# method colors 
var_cols <-  c("Public" = "#E30E0E", # dark red
               "Private" = "#FA7D7A", # light red
               "Other" = "#FA8D20") # orange
# country labels
country_labs <- list("cm" = "Cameroon",
                     "gh" = "Ghana",
                     "mw" = "Malawi",
                     "np" = "Nepal",
                     "rw" = "Rwanda")

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
  scale_fill_manual(values = var_cols) +
  labs(fill = "Method") +
  theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                       legend.position = "bottom", legend.title = element_blank(), legend.key = element_rect(colour = "black", size = .3),
                       legend.text = element_text(size = 11), strip.text = element_text(size = 12),
                       plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                       axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
  guides(fill = guide_legend(ncol = 3)) +
  ggtitle("Most recent source of contraception, 15-24")
ggsave(file.path(out.dir, 'contra_source_15_24.png'), width = 10, height =10)

## plot each country separately
for (cur_country in c("cm", "mw", "np", "rw",'gh')){
  ggplot(data= means_15_24[country==cur_country], aes(x = year_type, y = mean, fill = variable))+
    scale_y_continuous("Percent of population", label = percent, expand = c(0,0)) +
    geom_bar(stat = 'identity')+
    scale_fill_manual(values = var_cols) +
    labs(fill = "Method") +
    theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                         legend.position = "bottom", legend.title = element_blank(), legend.key = element_rect(colour = "black", size = .3),
                         legend.text = element_text(size = 11), strip.text = element_text(size = 12),
                         plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                         axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
    guides(fill = guide_legend(ncol = 3)) +
    ggtitle(paste("Most recent source of contraception, 15-24,", unique(means_15_24[country==cur_country]$country_lab)))
  title <- paste0('contra_source_', cur_country, '_15_24.png')
  ggsave(file.path(out.dir, title), width = 10, height =10)
}

####### ----- PLOT 15-49 -------
means_15_49 <- data.table()
countries <- c("cm", "mw", "np", "rw")
for (cur_country in countries) {
  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")
  current_data <- hs_dt[country ==cur_country & age %in% c(seq(15,49))]
  first_year <- min(current_data$year)
  last_year <- max(current_data$year)
  dhs_svy <- svydesign(id= ~psu, strata = ~strata, weights = ~pweight, data = hs_dt[country ==cur_country], nest = TRUE)
  means_p1 <- svyby(~contra_source_public, ~year, dhs_svy , svymean, na.rm =T) 
  means_p2 <- svyby(~contra_source_priv, ~year, dhs_svy , svymean, na.rm =T) 
  means_p3 <- svyby(~contra_source_other, ~year, dhs_svy , svymean, na.rm =T) 
  
  means_p1_long <- data.table(melt(means_p1, id.var = "year"))
  means_p2_long <- data.table(melt(means_p2, id.var = "year"))
  means_p3_long <- data.table(melt(means_p3, id.var = "year"))
  
  mean_p1_se <- organize_mean_se_singular(means_p1_long, "contra_source_public")
  mean_p2_se <- organize_mean_se_singular(means_p2_long, "contra_source_priv")
  mean_p3_se <- organize_mean_se_singular(means_p3_long, "contra_source_other")
  
  mean_se <- rbind(mean_p1_se, mean_p2_se, mean_p3_se)
  
  mean_se[, country := cur_country]
  means_15_49 <- rbind(means_15_49, mean_se)
}

## do ghana separately for now
countries <- c("gh")
for (cur_country in countries) {
  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")
  current_data <- hs_dt[country ==cur_country & age %in% c(seq(15,49))]
  dhs_svy <- svydesign(id= ~psu, strata = ~strata, weights = ~pweight, data = current_data, nest = TRUE)
  means_p1 <- svyby(~contra_source_public, ~year, dhs_svy , svymean, na.rm =T) 
  means_p2 <- svyby(~contra_source_priv, ~year, dhs_svy , svymean, na.rm =T) 
  means_p3 <- svyby(~contra_source_other, ~year, dhs_svy , svymean, na.rm =T) 
  
  means_p1_long <- data.table(melt(means_p1, id.var = "year"))
  means_p2_long <- data.table(melt(means_p2, id.var = "year"))
  means_p3_long <- data.table(melt(means_p3, id.var = "year"))
  
  mean_p1_se <- organize_mean_se_singular(means_p1_long, "contra_source_public")
  mean_p2_se <- organize_mean_se_singular(means_p2_long, "contra_source_priv")
  mean_p3_se <- organize_mean_se_singular(means_p3_long, "contra_source_other")
  
  mean_se <- rbind(mean_p1_se, mean_p2_se, mean_p3_se)
  
  mean_se[, country := cur_country]
  means_15_49 <- rbind(means_15_49, mean_se)
}

## rename highest ed variable
means_15_49$variable <- factor(means_15_49$variable , levels = c("contra_source_public",
                                                                 "contra_source_priv",
                                                                 "contra_source_other"),
                               labels =  c("Public",
                                           "Private",
                                           "Other"))

# method colors 
var_cols <-  c("Public" = "#E30E0E", # dark red
               "Private" = "#FA7D7A", # light red
               "Other" = "#FA8D20") # orange
# country labels
country_labs <- list("cm" = "Cameroon",
                     "gh" = "Ghana",
                     "mw" = "Malawi",
                     "np" = "Nepal",
                     "rw" = "Rwanda")

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
  scale_fill_manual(values = var_cols) +
  labs(fill = "Method") +
  theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                       legend.position = "bottom", legend.title = element_blank(), legend.key = element_rect(colour = "black", size = .3),
                       legend.text = element_text(size = 11), strip.text = element_text(size = 12),
                       plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                       axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
  guides(fill = guide_legend(ncol = 3)) +
  ggtitle("Most recent source of contraception, 15-49")
ggsave(file.path(out.dir, 'contra_source_15_49.png'), width = 10, height =10)

## plot each country separately
for (cur_country in c("cm", "mw", "np", "rw",'gh')){
  ggplot(data= means_15_49[country==cur_country], aes(x = year_type, y = mean, fill = variable))+
    scale_y_continuous("Percent of population", label = percent, expand = c(0,0)) +
    geom_bar(stat = 'identity')+
    scale_fill_manual(values = var_cols) +
    labs(fill = "Method") +
    theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                         legend.position = "bottom", legend.title = element_blank(), legend.key = element_rect(colour = "black", size = .3),
                         legend.text = element_text(size = 11), strip.text = element_text(size = 12),
                         plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                         axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
    guides(fill = guide_legend(ncol = 3)) +
    ggtitle(paste("Most recent source of contraception, 15-49,", unique(means_15_49[country==cur_country]$country_lab)))
  title <- paste0('contra_source_', cur_country, '_15_49.png')
  ggsave(file.path(out.dir, title), width = 10, height =10)
}

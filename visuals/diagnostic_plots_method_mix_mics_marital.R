## Created by: Corinne Bintz
## Creation date: 12/27/2023
## create diagnostic plots for method mix in ASHER decomp: with MICS as endline and by marital status 
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
ind_dt <- fread(file.path(in.dir, 'ind_dt_baseline_mics.csv'))

# method labels
method_labs <- list("unmet_need" = "Unmet Need",
                    "female_sterilization" = "Female Sterilisation",
                    "male_sterilization" = "Male Sterilisation",
                    "iud" = "IUD",
                    "injections" = "Injections",
                    "implants" = "Implants",
                    "pill" = "Pill",
                    "condom" = "Condom",
                    "diaphragm" = "Diaphragm",
                    "emergency_contraception" = "Emergency Contraception",
                    "other_modern_method" = "Other Modern",
                    "lactational_amenorrhea_method" = "LAM",
                    "rhythm" = "Rhythm",
                    "withdrawal" = "Withdrawal",
                    "other_traditional_method" = "Other Traditional",
                    "other"="Other")

# method colors 
method_cols <-  c("Female Sterilisation" = "#E30E0E", # dark red
                  "Male Sterilisation" = "#FA7D7A", # light red
                  "IUD" = "#FA8D20", # orange
                  "Injections" = "#FFCC12", # orange-yellow
                  "Implants" = "#FFFF12", # bright yellow
                  "Pill" = "#AEFA20", # light green
                  "Condom" = "#48FA20", # lime green
                  "Diaphragm" = "#21A105", # forest green
                  "Emergency Contraception" = "#09D990", # green-blue
                  "Other Modern" = "#09D9CE", # seafoam
                  "LAM" = "#095CD9", # blue
                  "Rhythm" = "#934FE0", # dark purple
                  "Withdrawal" = "#BF87FF", # light purple
                  "Other Traditional" = "#F587FF", # pink
                  "Unmet Need" = "snow", # white
                  "Other" =  "#8B4000")

# country labels
country_labs <- list("cm" = "Cameroon",
                     "gh" = "Ghana",
                     "mw" = "Malawi",
                     "np" = "Nepal",
                     "rw" = "Rwanda")

#unique(ind_dt[country == "gh" & year == 2003]$unmet_need)

ind_dt[, country := substr(country, 1,2)]
ind_dt[, baseline := min(year), by = "country"]
ind_dt[, latest := max(year), by = "country"]
ind_dt[, year_type := ifelse(year == baseline, "baseline", "latest")]

## if unmet need, mark this as current contraceptive method for method mix plots. if not unmet need and current method is none, make NA because we don't want to plot these people
ind_dt[, current_method := ifelse((current_method == "none" | current_method == "blank"| is.na("current_method")) & unmet_need != 1,NA, current_method)]

## if unmet need ==1 and current method is NA, make current_method "none" so they don't get lost 
ind_dt[, current_method := ifelse(is.na(current_method) & unmet_need == 1, "none", current_method)]

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

create_data <- function(age_start, age_end){
  all_ind_means <- data.table()
countries <- c("cm", "mw", "np", "rw", "gh")
for (cur_country in countries) {
  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")
  current_data <- ind_dt[country ==cur_country]
  first_year <- min(current_data$year)
  last_year <- max(current_data$year)
  
  dhs_svy <- svydesign(id= ~psu, strata = ~strata, weights = ~pweight, data = ind_dt[country ==cur_country & age %in% c(seq(age_start,age_end)) ], nest = TRUE)
  means_p1_mar <- svyby(~current_method, ~year, subset(dhs_svy, year == first_year & curr_cohabit == 1) , svymean, na.rm =T) 
  means_p1_unmar <- svyby(~current_method, ~year, subset(dhs_svy, year == first_year & curr_cohabit == 0) , svymean, na.rm =T) 
  
  means_p2_mar <- svyby(~current_method, ~year, subset(dhs_svy, year == last_year & curr_cohabit == 1) , svymean, na.rm =T) 
  means_p2_unmar <- svyby(~current_method, ~year, subset(dhs_svy, year == last_year & curr_cohabit == 0) , svymean, na.rm =T) 
  
  means_p1_mar_long <- data.table(melt(means_p1_mar, id.var = "year"))
  means_p1_unmar_long <- data.table(melt(means_p1_unmar, id.var = "year"))
  
  means_p2_mar_long <- data.table(melt(means_p2_mar, id.var = "year"))
  means_p2_unmar_long <- data.table(melt(means_p2_unmar, id.var = "year"))
  
  mean_se_p1_mar <- organize_mean_se_multiple(means_p1_mar_long)
  mean_se_p1_mar[, curr_cohabit := 1]
  mean_se_p1_unmar <- organize_mean_se_multiple(means_p1_unmar_long)
  mean_se_p1_unmar[, curr_cohabit := 0]
  
  mean_se_p2_mar <- organize_mean_se_multiple(means_p2_mar_long)
  mean_se_p2_mar[, curr_cohabit := 1]
  mean_se_p2_unmar <- organize_mean_se_multiple(means_p2_unmar_long)
  mean_se_p2_unmar[, curr_cohabit := 0]
  
  mean_se_all <- rbind(mean_se_p1_mar, mean_se_p1_unmar,
                       mean_se_p2_mar,mean_se_p2_unmar)
  mean_se_all[, country := cur_country]
  all_ind_means <- rbind(all_ind_means, mean_se_all)
}
## rename current method none to reflect unmet need instead
all_ind_means[, variable := str_replace(variable, 'current_method', '')]
all_ind_means[, variable := ifelse(variable %in% c("none", "blank"), 'unmet_need', variable)]

## combine calendar methods into rhythm 
all_ind_means[, variable := ifelse(variable == "calendar_methods", 'rhythm', variable)]
## combine foam jelly sponge into other modern 
all_ind_means[, variable := ifelse(variable == "foam_jelly_sponge", 'other_modern_method', variable)]

all_ind_means[, country_lab := unlist(country_labs[country])]

# method labels
all_ind_means[, method_lab := unlist(method_labs[variable])]
all_ind_means[, method_lab := factor(method_lab, levels = unlist(unname(method_labs)))]

all_ind_means[, curr_cohabit := factor(curr_cohabit, levels = c(0,1),
                                       labels = c('Unmarried/not in-union', "Married/in-union"))]


all_ind_means[, baseline := min(year), by = "country"]
all_ind_means[, latest := max(year), by = "country"]
all_ind_means[, year_type := ifelse(year == baseline, "baseline", "latest")]

}

all_ind_means_15_19 <- create_data(15,19)

## plot all countries at once
ggplot(data= all_ind_means_15_19, aes(x = year_type, y = mean, fill = method_lab))+
  facet_wrap(~country_lab+curr_cohabit)+
  scale_y_continuous("Percent of Total Need", label = percent, expand = c(0,0)) +
  geom_bar(stat = 'identity')+
  scale_fill_manual(values = method_cols) +
  labs(fill = "Method") +
  theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                       legend.position = "bottom", legend.title = element_blank(), legend.key = element_rect(colour = "black", size = .3),
                       legend.text = element_text(size = 11), strip.text = element_text(size = 12),
                       plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                       axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
    guides(fill = guide_legend(ncol = 3)) +
    ggtitle("Method mix, 15-19")
ggsave(file.path(out.dir, 'method_mix_all_15_19_by_marital.png'), width = 10, height =10)

## plot each country separately
for (cur_country in c("cm", "mw", "np", "rw",'gh')){
ggplot(data= all_ind_means_15_19[country == cur_country], aes(x = year_type, y = mean, fill = method_lab))+
  facet_wrap(~curr_cohabit)+
  scale_y_continuous("Percent of Total Need", label = percent, expand = c(0,0)) +
  geom_bar(stat = 'identity')+
  scale_fill_manual(values = method_cols) +
  labs(fill = "Method") +
  theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                       legend.position = "bottom", legend.title = element_blank(), legend.key = element_rect(colour = "black", size = .3),
                       legend.text = element_text(size = 11), strip.text = element_text(size = 12),
                       plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                       axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
  guides(fill = guide_legend(ncol = 3)) +
  ggtitle(paste("Method mix, 15-19, ", unique(all_ind_means_15_19[country == cur_country]$country_lab)))

title <- paste0('method_mix_15_19_', cur_country, '_by_marital.png')
ggsave(file.path(out.dir, title), width = 10, height =10)
}

## 15-24 ---------
all_ind_means_15_24 <- create_data(15,24)

## plot all countries at once
ggplot(data= all_ind_means_15_24, aes(x = year_type, y = mean, fill = method_lab))+
  facet_wrap(~country_lab+curr_cohabit)+
  scale_y_continuous("Percent of Total Need", label = percent, expand = c(0,0)) +
  geom_bar(stat = 'identity')+
  scale_fill_manual(values = method_cols) +
  labs(fill = "Method") +
  theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                       legend.position = "bottom", legend.title = element_blank(), legend.key = element_rect(colour = "black", size = .3),
                       legend.text = element_text(size = 11), strip.text = element_text(size = 12),
                       plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                       axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
  guides(fill = guide_legend(ncol = 3)) +
  ggtitle("Method mix, 15-24")
ggsave(file.path(out.dir, 'method_mix_all_15_24_by_marital.png'), width = 10, height =10)

## plot each country separately
for (cur_country in c("cm", "mw", "np", "rw",'gh')){
  ggplot(data= all_ind_means_15_24[country == cur_country], aes(x = year_type, y = mean, fill = method_lab))+
    facet_wrap(~curr_cohabit)+
    scale_y_continuous("Percent of Total Need", label = percent, expand = c(0,0)) +
    geom_bar(stat = 'identity')+
    scale_fill_manual(values = method_cols) +
    labs(fill = "Method") +
    theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                         legend.position = "bottom", legend.title = element_blank(), legend.key = element_rect(colour = "black", size = .3),
                         legend.text = element_text(size = 11), strip.text = element_text(size = 12),
                         plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                         axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
    guides(fill = guide_legend(ncol = 3)) +
    ggtitle(paste("Method mix, 15-24, ", unique(all_ind_means_15_24[country == cur_country]$country_lab)))
  
  title <- paste0('method_mix_15_24_', cur_country, '_by_marital.png')
  ggsave(file.path(out.dir, title), width = 10, height =10)
}
## 15-49 ---------
all_ind_means_15_49 <- create_data(15,49)

## plot all countries at once
ggplot(data= all_ind_means_15_49, aes(x = year_type, y = mean, fill = method_lab))+
  facet_wrap(~country_lab + curr_cohabit)+
  scale_y_continuous("Percent of Total Need", label = percent, expand = c(0,0)) +
  geom_bar(stat = 'identity')+
  scale_fill_manual(values = method_cols) +
  labs(fill = "Method") +
  theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                       legend.position = "bottom", legend.title = element_blank(), legend.key = element_rect(colour = "black", size = .3),
                       legend.text = element_text(size = 11), strip.text = element_text(size = 12),
                       plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                       axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
  guides(fill = guide_legend(ncol = 3)) +
  ggtitle("Method mix, 15-49")
ggsave(file.path(out.dir, 'method_mix_all_15_49_by_marital.png'), width = 10, height =10)

## plot each country separately
for (cur_country in c("cm", "mw", "np", "rw",'gh')){
  ggplot(data= all_ind_means_15_49[country == cur_country], aes(x = year_type, y = mean, fill = method_lab))+
    facet_wrap(~curr_cohabit)+
    scale_y_continuous("Percent of Total Need", label = percent, expand = c(0,0)) +
    geom_bar(stat = 'identity')+
    scale_fill_manual(values = method_cols) +
    labs(fill = "Method") +
    theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                         legend.position = "bottom", legend.title = element_blank(), legend.key = element_rect(colour = "black", size = .3),
                         legend.text = element_text(size = 11), strip.text = element_text(size = 12),
                         plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                         axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
    guides(fill = guide_legend(ncol = 3)) +
    ggtitle(paste("Method mix, 15-49, ", unique(all_ind_means_15_49[country == cur_country]$country_lab)))
  
  title <- paste0('method_mix_15_49_', cur_country, '_by_marital.png')
  ggsave(file.path(out.dir, title), width = 10, height =10)
}



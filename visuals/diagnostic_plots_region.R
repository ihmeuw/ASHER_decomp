## Created by: Corinne Bintz
## Creation date: 12/28/2023
## create diagnostic plots for region in ASHER decomp  
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

## read in hh summary data
hh_dt <- fread(file.path(in.dir, 'hh_table.csv'))

## filter to just region variables
hh_dt <- hh_dt[merge_var == 'region']

## rename region variable
hh_dt[, variable := str_replace(variable, 'region', '')]

## reshape wide to long
setnames(hh_dt, c("mean_baseline", "mean_latest", "variable"), c("baseline", "endline", "variable_name"))
hh_dt_long <- melt(hh_dt, id.vars = c("country", "variable_name"),
             measure.vars = c("baseline", "endline"))
setnames(hh_dt_long, "variable", "year_type")

# # method labels
# method_labs <- list("unmet_need" = "Unmet Need",
#                     "female_sterilization" = "Female Sterilisation",
#                     "male_sterilization" = "Male Sterilisation",
#                     "iud" = "IUD",
#                     "injections" = "Injections",
#                     "implants" = "Implants",
#                     "pill" = "Pill",
#                     "condom" = "Condom",
#                     "diaphragm" = "Diaphragm",
#                     "emergency_contraception" = "Emergency Contraception",
#                     "other_modern_method" = "Other Modern",
#                     "lactational_amenorrhea_method" = "LAM",
#                     "rhythm" = "Rhythm",
#                     "withdrawal" = "Withdrawal",
#                     "other_traditional_method" = "Other Traditional")
# 
# # method colors 
# method_cols <-  c("Female Sterilisation" = "#E30E0E", # dark red
#                   "Male Sterilisation" = "#FA7D7A", # light red
#                   "IUD" = "#FA8D20", # orange
#                   "Injections" = "#FFCC12", # orange-yellow
#                   "Implants" = "#FFFF12", # bright yellow
#                   "Pill" = "#AEFA20", # light green
#                   "Condom" = "#48FA20", # lime green
#                   "Diaphragm" = "#21A105", # forest green
#                   "Emergency Contraception" = "#09D990", # green-blue
#                   "Other Modern" = "#09D9CE", # seafoam
#                   "LAM" = "#095CD9", # blue
#                   "Rhythm" = "#934FE0", # dark purple
#                   "Withdrawal" = "#BF87FF", # light purple
#                   "Other Traditional" = "#F587FF", # pink
#                   "Unmet Need" = "snow") # white

# country labels
country_labs <- list("cm" = "Cameroon",
                     "gh" = "Ghana",
                     "mw" = "Malawi",
                     "np" = "Nepal",
                     "rw" = "Rwanda")

hh_dt_long[, country_lab := unlist(country_labs[country])]

## plot each country separately
for (cur_country in c("cm", "mw", "np", "rw",'gh')){
  print(cur_country)
  colourCount <- length(unique(hh_dt_long[country == cur_country]$variable_name))
  getPalette <- colorRampPalette(brewer.pal(9, "Set3"))
  
  ggplot(data= hh_dt_long[country == cur_country], aes(x = year_type, y = value, fill = variable_name))+
    scale_y_continuous("Percent of population", label = percent, expand = c(0,0)) +
    geom_bar(stat = 'identity')+
    scale_fill_manual(values = getPalette(colourCount))+
    labs(fill = "Region") +
    theme_bw()   + theme(panel.spacing.x = unit(4, "mm"),
                         legend.position = "bottom", legend.title = element_blank(), legend.key = element_rect(colour = "black", size = .3),
                         legend.text = element_text(size = 11), strip.text = element_text(size = 12),
                         plot.title = element_text(size = 16), axis.title = element_text(size = 12),
                         axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())+
    guides(fill = guide_legend(ncol = 3)) +
    ggtitle(paste("Regional breakdown,", unique(hh_dt_long[country == cur_country]$country_lab)))
  
  title <- paste0('regional_breakdown_', cur_country, '.png')
  ggsave(file.path(out.dir, title), width = 10, height =10)
}


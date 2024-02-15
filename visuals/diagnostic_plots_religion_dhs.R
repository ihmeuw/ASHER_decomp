## Created by: Corinne Bintz
## Creation date: 1/17/2023
## create diagnostic plots for religion in ASHER decomp: using dhs as endline 
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
hh_dt <- fread(file.path(in.dir, 'hh_table_dhs.csv'))

## filter to just region variables
hh_dt <- hh_dt[merge_var == 'religion']

## rename region variable
hh_dt[, variable := str_replace(variable, 'religion', '')]

## reshape wide to long
setnames(hh_dt, c("mean_baseline", "mean_latest", "variable"), c("baseline", "endline", "variable_name"))
hh_dt_long <- melt(hh_dt, id.vars = c("country", "variable_name"),
             measure.vars = c("baseline", "endline"))
setnames(hh_dt_long, "variable", "year_type")

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
    ggtitle(paste("Religious breakdown,", unique(hh_dt_long[country == cur_country]$country_lab)))
  
  title <- paste0('religious_breakdown_', cur_country, '_dhs.png')
  ggsave(file.path(out.dir, title), width = 10, height =10)
}


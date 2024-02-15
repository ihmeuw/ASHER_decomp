############################################################################################################
## Name: Corinne Bintz 
## Team: Health Systems
## Purpose: Investigate changes in variables of interest prompted by diagnostic plots for ASHER project 
## Date: 1/02/2024
## Notes: 
###########################################################################################################
rm(list=ls())

## SET-UP -----------------------------------------------------------------------------

# username is pulled automatically
username <- Sys.getenv("USER") 

# runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j <- "/home/j/"
  h <- paste0("/homes/", username, "/")
} else {
  j <- "J:/"
  h <- "H:/"
}

# packages
pacman::p_load(data.table,tidyverse,openxlsx,readxl)
out.dir <- '/share/scratch/projects/hssa/asher/plots/'

## load shared functions
source("/ihme/cc_resources/libraries/current/r/get_covariate_estimates.R")
functions <- list.files('/ihme/cc_resources/libraries/gbd_env/r/')
invisible(lapply(paste0('/ihme/cc_resources/libraries/gbd_env/r/',functions), source))

id.vars <- c("location_id","year_id", "age_group_id", "sex_id", "sex", "location_name", "age_group_name")

## -------- education questions ------
## AH:	Education: Large increases in educational attainment among adolescents in all countries where we have an endline, interesting.  
## Could you check that the educational increase in Nepal in particular is consistent with the GBD estimates?  

## Mean years of education:
# cameroon
adolescent_ed_cm <- get_covariate_estimates(covariate_id = 33, release = 10, year_id = c(2004,2022), location_id = c(202), age_group_id = 8, sex_id = 2)
# nepal
adolescent_ed_np <- get_covariate_estimates(covariate_id = 33, release = 10, year_id = c(2001,2022), location_id = c(164), age_group_id = 8, sex_id = 2)
# rwanda
adolescent_ed_rw <- get_covariate_estimates(covariate_id = 33, release = 10, year_id = c(2000,2019), location_id = c(185), age_group_id = 8, sex_id = 2)
# malawi 
adolescent_ed_mw <- get_covariate_estimates(covariate_id = 33, release = 10, year_id = c(2000,2015), location_id = c(182), age_group_id = 8, sex_id = 2)

adolescent_ed <- rbind(adolescent_ed_cm,adolescent_ed_np,adolescent_ed_rw,adolescent_ed_mw)

## education by level:

# cameroon
primary_ed_cm <- get_covariate_estimates(covariate_id = 2016, release = 10, year_id = c(2004,2022), location_id = c(202), age_group_id = 8, sex_id = 2)
# nepal
primary_ed_np <- get_covariate_estimates(covariate_id = 2016, release = 10, year_id = c(2001,2022), location_id = c(164), age_group_id = 8, sex_id = 2)
# rwanda
primary_ed_rw <- get_covariate_estimates(covariate_id = 2016, release = 10, year_id = c(2000,2019), location_id = c(185), age_group_id = 8, sex_id = 2)
# malawi 
primary_ed_mw <- get_covariate_estimates(covariate_id = 2016, release = 10, year_id = c(2000,2015), location_id = c(182), age_group_id = 8, sex_id = 2)

ed_primary <- rbind(primary_ed_cm,primary_ed_np,primary_ed_rw,primary_ed_mw)

# cameroon
secondary_ed_cm <- get_covariate_estimates(covariate_id = 2017, release = 10, year_id = c(2004,2022), location_id = c(202), age_group_id = 8, sex_id = 2)
# nepal
secondary_ed_np <- get_covariate_estimates(covariate_id = 2017, release = 10, year_id = c(2001,2022), location_id = c(164), age_group_id = 8, sex_id = 2)
# rwanda
secondary_ed_rw <- get_covariate_estimates(covariate_id = 2017, release = 10, year_id = c(2000,2019), location_id = c(185), age_group_id = 8, sex_id = 2)
# malawi 
secondary_ed_mw <- get_covariate_estimates(covariate_id = 2017, release = 10, year_id = c(2000,2015), location_id = c(182), age_group_id = 8, sex_id = 2)

ed_secondary <- rbind(secondary_ed_cm,secondary_ed_np,secondary_ed_rw,secondary_ed_mw)

# cameroon
tertiary_ed_cm <- get_covariate_estimates(covariate_id = 2018, release = 10, year_id = c(2004,2022), location_id = c(202), age_group_id = 8, sex_id = 2)
# nepal
tertiary_ed_np <- get_covariate_estimates(covariate_id = 2018, release = 10, year_id = c(2001,2022), location_id = c(164), age_group_id = 8, sex_id = 2)
# rwanda
tertiary_ed_rw <- get_covariate_estimates(covariate_id = 2018, release = 10, year_id = c(2000,2019), location_id = c(185), age_group_id = 8, sex_id = 2)
# malawi 
tertiary_ed_mw <- get_covariate_estimates(covariate_id = 2018, release = 10, year_id = c(2000,2015), location_id = c(182), age_group_id = 8, sex_id = 2)

tertiary_ed <- get_covariate_estimates(covariate_id = 2018, release = 10, year_id = c(2000,2015), location_set_id = 22, age_group_id = 8, sex_id = 2)


ed_tertiary <- rbind(tertiary_ed_cm,tertiary_ed_np,tertiary_ed_rw,tertiary_ed_mw)

## rename means
setnames(ed_primary, "mean_value", "primary")
setnames(ed_secondary, "mean_value", "secondary")
setnames(ed_tertiary, "mean_value", "tertiary")
education <- merge(ed_primary[, -c("lower_value", "upper_value")], 
                   ed_secondary[, -c("lower_value", "upper_value")], by = id.vars)
education <- merge(education,
                   ed_tertiary[, -c("lower_value", "upper_value")], by = id.vars)
education <- education[, c("primary", "secondary", "tertiary", "location_id","year_id", "age_group_id", "sex_id", "sex", "location_name", "age_group_name")]

## calculate share of women in each grouping instead of "at least" 
education[, less_than_primary := 1-primary]
education[, secondary_only := secondary - tertiary]
education[, primary_only := primary - (secondary_only + tertiary)]

## reshape wide to long
education_long <- melt(education[, -c("primary", "secondary")], id.vars = c(id.vars),
                   measure.vars = c("less_than_primary", "secondary_only","primary_only", "tertiary"))

education_long[, baseline := min(year_id), by = "location_name"]
education_long[, latest := max(year_id), by = "location_name"]
education_long[, year_type := ifelse(year_id == baseline, "baseline", "latest")]

# method colors 
ed_cols <-  c("No Education" = "#E30E0E", # dark red
              "Primary" = "#FA7D7A", # light red
              "Secondary" = "#FA8D20", # orange
              "Higher" = "#FFCC12") # yellow
education_long$variable <- factor(education_long$variable,
                                  levels = c("less_than_primary",
                                             "primary_only",
                                             "secondary_only",
                                             "tertiary"),
                                  labels = c("No Education",
                                             "Primary",
                                             "Secondary",
                                             "Higher"))

## plot all countries at once
ggplot(data= education_long, aes(x = year_type, y = value, fill = variable))+
  facet_wrap(~location_name)+
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
  ggtitle("Highest education breakdown, 15-19, GBD results")
ggsave(file.path(out.dir, 'higher_ed_all_15_19_gbd.png'), width = 10, height =10)

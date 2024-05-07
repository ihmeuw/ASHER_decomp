#-------------------Header------------------------------------------------
# Author: Olivia Angelino
# Project: ASHER Decomp
# Purpose: Visualize cohort model results
# Date: 3/27/2024
# Notes:
#***************************************************************************

# SET-UP -------------------------------------------------------------------

# if running right after 05, nice to not erase memory
# clear memory
# rm(list=ls())

# Username is pulled automatically
username <- Sys.getenv("USER") 

# runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j <- "/snfs1/"
  h <- paste0("/homes/", username, "/")
} else {
  j <- "J:/"
  h <- "H:/"
}

# load packages
pacman::p_load(data.table,tidyverse,dplyr)

# in/out
out.dir <- '/share/scratch/projects/hssa/asher/data/04_model_cohort'


# PREP DATA ----------------------------------------------------------------

# list files in 04_model_cohort
files <- list.files(out.dir)

# subset to files with model coefficients
files <- files[grepl("_models_coefs", files)]

# read in and combine for all the countries
model_coefs <- rbindlist(lapply(file.path(out.dir, files), fread), fill = T)

# rename columns
setnames(model_coefs, c("rn", "coef", "Pr(>|z|)"), c("var", "val", "p_val"))

# T/F if significant at the 0.05 level
model_coefs[, signif := ifelse(p_val < .05, T, F)]

# identify direction of coefficient
model_coefs[val < 0, group := "neg"]
model_coefs[val > 0, group := "pos"]

# construct confidence intervals
model_coefs[, conf_high := val + (1.96 * `robust se`)]
model_coefs[, conf_low := val - (1.96 * `robust se`)]

# cleaned variable names
var_names <- c("urban" = "Urban",
               "as_factor(windex5)2" = "Wealth 2nd quintile",
               "as_factor(windex5)3" = "Wealth 3rd quintile",
               "as_factor(windex5)4" = "Wealth 4th quintile",
               "as_factor(windex5)5" = "Wealth 5th quintile",
               "as_factor(wealth_quintiles)2" = "Wealth 2nd quintile",
               "as_factor(wealth_quintiles)3" = "Wealth 3rd quintile",
               "as_factor(wealth_quintiles)4" = "Wealth 4th quintile",
               "as_factor(wealth_quintiles)5" = "Wealth 5th quintile",
               "mean_yrs_schooling_head" = "HH head educ yrs",
               "knowledge_mod" = "# modern methods known",
               "curr_cohabit_timevary" = "Married/in-union",
               "ever_had_intercourse_timevary" = "Ever had intercourse",
               "ed_levelCurrently attending" = "Attending school",
               "ed_levelNot attending: completed primary" = "Not attending: completed primary",            
               "ed_levelNot attending: completed secondary" = "Not attending: completed secondary", 
               "ed_levelNot attending: some primary" = "Not attending: some primary",
               "ed_levelNot attending: some secondary" = "Not attending: some secondary",
               "ed_level_customCurrently attending" = "Attending school",
               "ed_level_customNot attending: completed primary" = "Not attending: completed primary",            
               "ed_level_customNot attending: completed secondary" = "Not attending: completed secondary", 
               "ed_level_customNot attending: some primary" = "Not attending: some primary",
               "ed_level_customNot attending: some secondary" = "Not attending: some secondary",
               "educ_yrs_timevary" = "Educ yrs completed",
               "attend_school_timevary" = "Attending school",
               "mod_contra" = "Mod contra",
               "mod_contra_lag_1m" = "Mod contra, lag 1 mo",
               "mod_contra_past_2m" = "Mod contra, past 2 months",
               "year" = "Calendar Year",
               "beating_just" = "Beating is justified",
               "wants_children" = "Wants children",
               "age_1st_sex_imp" = "Age at first sex",
               "decision_use_joint_respondent_timevary_np_2022" = "Self/joint decision re:contra",
               "mod_contra_inc_fail" = "Mod contra",
               "as.factor(current_contra_type)Modern method" = "Modern method",     
               "as.factor(current_contra_type)Traditional method" = "Trad method",
               "as.factor(current_contra_subtype)Modern Long-acting" = "Modern Long-acting",    
               "as.factor(current_contra_subtype)Modern Short-acting" = "Modern Short-acting",
               "as.factor(current_contra_subtype)Traditional" = "Traditional")

# apply variable names and factorize to set plot order
model_coefs[, var_label := var_names[var]]
model_coefs[, var_label := factor(var_label, levels = c("Urban","HH head educ yrs","Wealth 2nd quintile","Wealth 3rd quintile","Wealth 4th quintile",
                                                        "Wealth 5th quintile","Married/in-union","Ever had intercourse","Age at first sex","# modern methods known",
                                                        "Mod contra","Mod contra, lag 1 mo","Modern method", "Trad method", 
                                                        "Modern Long-acting", "Modern Short-acting", "Traditional","Self/joint decision re:contra","Attending school",
                                                        "Educ yrs completed","Not attending: some primary","Not attending: completed primary",
                                                        "Not attending: some secondary","Not attending: completed secondary","Wants children","Beating is justified"))]


# PLOT COEFFICIENTS --------------------------------------------------------

# all data sources
pdf(file.path(out.dir, "all_data_model_coefs.pdf"), onefile = T, width = 11, height = 6)
for (mod in unique(model_coefs[grepl("ALL", data)]$model)) {
  gg <- ggplot(data = model_coefs[model == mod & grepl("ALL", data)], aes(x = val, y = var_label, color = group, alpha = signif)) +
    geom_vline(aes(xintercept = 0), linetype = "dashed") +
    geom_pointrange(aes(xmin = conf_low, xmax = conf_high)) +
    scale_color_brewer(palette = "Set1") +
    scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.2)) +
    scale_y_discrete(limits = rev) +
    labs(x = "Coefficient", title = mod, color = "Direction", alpha = "Significance") +
    theme_bw() + theme(axis.title.y = element_blank()) +
    facet_wrap(~data, nrow = 1, scales = "free_x")
  print(gg)  
}
dev.off()


# PLOT PRESENTATION MODELS --------------------------------------------------

# final models for country-specific presentations

# list files in 04_model_cohort
files <- list.files(out.dir)

# subset to files with model coefficients
files <- files[grepl("_models_coefs", files)]

# read in and combine for all the countries
model_coefs_final <- rbindlist(lapply(file.path(out.dir, files), fread), fill = T)
model_coefs_final <- model_coefs_final[grepl(", categorical education", model)]

# rename columns
setnames(model_coefs_final, c("rn", "coef", "Pr(>|z|)"), c("var", "val", "p_val"))

# T/F if significant at the 0.05 level
model_coefs_final[, signif := ifelse(p_val < .05, T, F)]

# identify direction of coefficient
model_coefs_final[val < 0, group := "Negative"]
model_coefs_final[val > 0, group := "Positive"]

# construct confidence intervals
model_coefs_final[, conf_high := val + (1.96 * `robust se`)]
model_coefs_final[, conf_low := val - (1.96 * `robust se`)]


## BASE MODEL -----------------------------------------------------------

# base model
model_coefs_base <- model_coefs_final[model == "no contra lag, inc method failure, categorical education"]
model_coefs_base[, country := str_sub(data, 1, 3)]

# prep coefficients adding filler rows for titles
plot_dt <- data.table()
for (cur_country in unique(model_coefs_base$country)) {
  tmp <- rbind(data.table(var="----TIME-FIXED----", val=NA, country=cur_country),
               model_coefs_base[country == cur_country & grepl("urban|mean_yrs_schooling|beating", var)],
               data.table(var="DHS Wealth Index", val=NA, country=cur_country),
               data.table(var="1st quintile (ref)", val = 0, conf_low = 0, conf_high = 0, group="Reference", signif = FALSE, country=cur_country),
               model_coefs_base[country == cur_country & grepl("wealth", var)],
               data.table(var="----TIME-VARYING----", val=NA, country=cur_country),
               model_coefs_base[country == cur_country & grepl("curr_cohabit", var)],model_coefs_base[country == cur_country & grepl("ever_had_inter", var)],
               model_coefs_base[country == cur_country & grepl("mod_contra_inc_fail", var)],
               data.table(var="Educational Attainment", val=NA, country=cur_country),
               data.table(var="Not attending: less than secondary (ref)", val = 0, conf_low = 0, conf_high = 0, group="Reference", signif = FALSE, country=cur_country),
               model_coefs_base[country == cur_country & grepl("ed_levelNot", var)],model_coefs_base[country == cur_country & grepl("ed_levelCur", var)],
               fill=T)
  plot_dt <- rbind(plot_dt, tmp, fill = T)
}
plot_dt$var <- plot_dt$var %>% factor(., levels=., labels=., ordered=T)

# variable labels pretty
y_axis_labs <- c("-----TIME-FIXED-----","Urban","HH head educ yrs","Beating is justified",
                 "DHS Wealth Index","1st quintile (Ref)", "2nd quintile", "3rd quintile", "4th quintile", "5th quintile",
                 "----TIME-VARYING----","Married/in-union","Ever had intercourse","Modern contraception",
                 "Educational Attainment","Not attending: less than secondary (Ref)","Not attending: completed secondary","Attending school") %>% rev()

# set which variable labels to bold
bold_titles <- c("-----TIME-FIXED-----","DHS Wealth Index","----TIME-VARYING----","Educational Attainment")
bold_labels <- ifelse(y_axis_labs %in% bold_titles, yes = "bold", no = "plain")

bold_color <- "#1b4e5f"
plain_color <- "black"
bold_colors <- ifelse(y_axis_labs %in% bold_titles, yes = bold_color, no = plain_color)

# plot individual country results
for (cur_country in unique(plot_dt$country)) {
  p.effects <- ggplot(data = plot_dt[country == cur_country], aes(y = var, x = val, alpha = signif)) +
    geom_vline(aes(xintercept = 0), linetype = "dashed", size = .5) +
    geom_pointrange(aes(xmin = conf_low, xmax = conf_high, color = group)) +
    geom_point(aes(fill = group), color = "white", shape = 21, size = 3.5) +
    scale_color_manual(values = c("Positive" = "#377EB8", "Negative" = "#E41A1C", "Reference"= "grey35")) +
    scale_fill_manual(values = c("Positive" = "#377EB8", "Negative" = "#E41A1C", "Reference" = "grey35")) +
    scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.2)) +
    scale_y_discrete(labels=y_axis_labs, limits = rev) +   
    labs(x="Coefficient", alpha = "Significance", fill = "Direction", color = "Direction") +
    coord_cartesian() +
    theme_sjplot2() + theme(axis.title.y = element_blank(), axis.line.x = element_line(color = NA), axis.line.y = element_line(color = NA),
                            axis.text.y = element_text(face = bold_labels, color = bold_colors, size = 10.5), panel.grid.major.x = element_line(linetype = "dotted"), 
                            panel.grid.minor.x = element_line(linetype = "dashed"), legend.text = element_text(size = 10), axis.title.x = element_text(size = 10),
                            axis.text.x.bottom = element_text(color = plain_color))
  p.effects
  
  ggsave(filename = file.path(out.dir, paste0(cur_country, "_base_model_coefs.png")), width = 6.5, height = 6)
}

# get full country names
plot_dt[country == "GHA", country_full := "Ghana"]
plot_dt[country == "RWA", country_full := "Rwanda"]
plot_dt[country == "NPL", country_full := "Nepal"]
plot_dt[country == "MWI", country_full := "Malawi"]

# plot all countries in one plot
p.effects <- ggplot(data = plot_dt, aes(y = var, x = val, alpha = signif)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", size = .5) +
  geom_pointrange(aes(xmin = conf_low, xmax = conf_high, color = group)) +
  geom_point(aes(fill = group), color = "white", shape = 21, size = 3.5) +
  scale_color_manual(values = c("Positive" = "#377EB8", "Negative" = "#E41A1C", "Reference"= "grey35")) +
  scale_fill_manual(values = c("Positive" = "#377EB8", "Negative" = "#E41A1C", "Reference" = "grey35")) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.2)) +
  scale_y_discrete(labels=y_axis_labs, limits = rev) +
  labs(x="Coefficient", alpha = "Significance", fill = "Direction", color = "Direction") +
  coord_cartesian() +
  theme_bw() + theme(axis.title.y = element_blank(), axis.line.x = element_line(color = NA), axis.line.y = element_line(color = NA),
                          axis.text.y = element_text(face = bold_labels, color = bold_colors, size = 10.5), panel.grid.major.x = element_line(linetype = "dotted"), 
                          panel.grid.minor.x = element_line(linetype = "dashed"), legend.text = element_text(size = 10), axis.title.x = element_text(size = 10),
                          axis.text.x.bottom = element_text(color = plain_color), axis.ticks.y = element_blank(), strip.text.x = element_text(size = 11)) + 
  facet_wrap(~country_full, ncol = 4, scales = "free_x")
p.effects

ggsave(filename = file.path(out.dir, "ALL_base_model_coefs.png"), width = 12, height = 6)



## BASE MODEL W CONTRA SUBTYPE -------------------------------------------

# base model w method subtype
model_coefs_base <- model_coefs_final[model == "contra subtype inc fail, drop knowledge, categorical education"]
model_coefs_base[, country := str_sub(data, 1, 3)]

# prep coefficients adding filler rows for titles
plot_dt <- data.table()
for (cur_country in unique(model_coefs_base$country)) {
  tmp <- rbind(data.table(var="----TIME-FIXED----", val=NA, country=cur_country),
               model_coefs_base[country == cur_country & grepl("urban|mean_yrs_schooling|beating", var)],
               data.table(var="DHS Wealth Index", val=NA, country=cur_country),
               data.table(var="1st quintile (ref)", val = 0, conf_low = 0, conf_high = 0, group="Reference", signif = FALSE, country=cur_country),
               model_coefs_base[country == cur_country & grepl("wealth", var)],
               data.table(var="----TIME-VARYING----", val=NA, country=cur_country),
               model_coefs_base[country == cur_country & grepl("curr_cohabit", var)],model_coefs_base[country == cur_country & grepl("ever_had_inter", var)],
               data.table(var="Contraceptive Use", val=NA, country=cur_country),
               data.table(var="No method (ref)", val = 0, conf_low = 0, conf_high = 0, group="Reference", signif = FALSE, country=cur_country),
               model_coefs_base[country == cur_country & grepl("current_contra_subtype)Trad", var)],
               model_coefs_base[country == cur_country & grepl("current_contra_subtype)Modern Short", var)],
               model_coefs_base[country == cur_country & grepl("current_contra_subtype)Modern Long", var)],
               data.table(var="Educational Attainment", val=NA, country=cur_country),
               data.table(var="Not attending: less than secondary (ref)", val = 0, conf_low = 0, conf_high = 0, group="Reference", signif = FALSE, country=cur_country),
               model_coefs_base[country == cur_country & grepl("ed_levelNot", var)],model_coefs_base[country == cur_country & grepl("ed_levelCur", var)],
               fill=T)
  plot_dt <- rbind(plot_dt, tmp, fill = T)
}
plot_dt$var <- plot_dt$var %>% factor(., levels=., labels=., ordered=T)

# variable labels pretty
y_axis_labs <- c("-----TIME-FIXED-----","Urban","HH head educ yrs","Beating is justified",
                 "DHS Wealth Index","1st quintile (Ref)", "2nd quintile", "3rd quintile", "4th quintile", "5th quintile",
                 "----TIME-VARYING----","Married/in-union","Ever had intercourse","Contraceptive Use","No method (ref)", "Traditional", "Modern short-acting","Modern long-acting",
                 "Educational Attainment","Not attending: less than secondary (Ref)","Not attending: completed secondary","Attending school") %>% rev()

# set which variable labels to bold
bold_titles <- c("-----TIME-FIXED-----","DHS Wealth Index","----TIME-VARYING----","Educational Attainment","Contraceptive Use")
bold_labels <- ifelse(y_axis_labs %in% bold_titles, yes = "bold", no = "plain")

bold_color <- "#1b4e5f"
plain_color <- "black"
bold_colors <- ifelse(y_axis_labs %in% bold_titles, yes = bold_color, no = plain_color)

# plot individual country results
for (cur_country in unique(plot_dt$country)) {
  p.effects <- ggplot(data = plot_dt[country == cur_country], aes(y = var, x = val, alpha = signif)) +
    geom_vline(aes(xintercept = 0), linetype = "dashed", size = .5) +
    geom_pointrange(aes(xmin = conf_low, xmax = conf_high, color = group)) +
    geom_point(aes(fill = group), color = "white", shape = 21, size = 3.5) +
    scale_color_manual(values = c("Positive" = "#377EB8", "Negative" = "#E41A1C", "Reference"= "grey35")) +
    scale_fill_manual(values = c("Positive" = "#377EB8", "Negative" = "#E41A1C", "Reference" = "grey35")) +
    scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.2)) +
    scale_y_discrete(labels=y_axis_labs, limits = rev) +
    labs(x="Coefficient", alpha = "Significance", fill = "Direction", color = "Direction") +
    coord_cartesian() +
    theme_sjplot2() + theme(axis.title.y = element_blank(), axis.line.x = element_line(color = NA), axis.line.y = element_line(color = NA),
                            axis.text.y = element_text(face = bold_labels, color = bold_colors, size = 10.5), panel.grid.major.x = element_line(linetype = "dotted"), 
                            panel.grid.minor.x = element_line(linetype = "dashed"), legend.text = element_text(size = 10), axis.title.x = element_text(size = 10),
                            axis.text.x.bottom = element_text(color = plain_color))
  p.effects
  
  ggsave(filename = file.path(out.dir, paste0(cur_country, "_base_model_method_subtype_coefs.png")), width = 6.5, height = 6)
}

# get full country names
plot_dt[country == "GHA", country_full := "Ghana"]
plot_dt[country == "RWA", country_full := "Rwanda"]
plot_dt[country == "NPL", country_full := "Nepal"]
plot_dt[country == "MWI", country_full := "Malawi"]

# plot all countries in one plot
p.effects <- ggplot(data = plot_dt, aes(y = var, x = val, alpha = signif)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", size = .5) +
  geom_pointrange(aes(xmin = conf_low, xmax = conf_high, color = group)) +
  geom_point(aes(fill = group), color = "white", shape = 21, size = 3.5) +
  scale_color_manual(values = c("Positive" = "#377EB8", "Negative" = "#E41A1C", "Reference"= "grey35")) +
  scale_fill_manual(values = c("Positive" = "#377EB8", "Negative" = "#E41A1C", "Reference" = "grey35")) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.2)) +
  scale_y_discrete(labels=y_axis_labs, limits = rev) +
  labs(x="Coefficient", alpha = "Significance", fill = "Direction", color = "Direction") +
  coord_cartesian() +
  theme_bw() + theme(axis.title.y = element_blank(), axis.line.x = element_line(color = NA), axis.line.y = element_line(color = NA),
                     axis.text.y = element_text(face = bold_labels, color = bold_colors, size = 10.5), panel.grid.major.x = element_line(linetype = "dotted"), 
                     panel.grid.minor.x = element_line(linetype = "dashed"), legend.text = element_text(size = 10), axis.title.x = element_text(size = 10),
                     axis.text.x.bottom = element_text(color = plain_color), axis.ticks.y = element_blank(), strip.text.x = element_text(size = 11)) + 
  facet_wrap(~country_full, ncol = 4, scales = "free_x")
p.effects

ggsave(filename = file.path(out.dir, "ALL_base_model_method_subtype_coefs.png"), width = 12, height = 6)


## PART 1 MODEL OUTCOME IS FIRST SEX -----------------------------------------

# part 1 model
model_coefs_base <- model_coefs_final[model == "Two-stage model: part 1 outcome is first sexual intercourse, categorical education"]
model_coefs_base[, country := str_sub(data, 1, 3)]

# prep coefficients adding filler rows for titles
plot_dt <- data.table()
for (cur_country in unique(model_coefs_base$country)) {
  tmp <- rbind(data.table(var="----TIME-FIXED----", val=NA, country=cur_country),
               model_coefs_base[country == cur_country & grepl("urban|mean_yrs_schooling|beating", var)],
               data.table(var="DHS Wealth Index", val=NA, country=cur_country),
               data.table(var="1st quintile (ref)", val = 0, conf_low = 0, conf_high = 0, group="Reference", signif = FALSE, country=cur_country),
               model_coefs_base[country == cur_country & grepl("wealth", var)],
               data.table(var="----TIME-VARYING----", val=NA, country=cur_country),
               model_coefs_base[country == cur_country & grepl("curr_cohabit", var)],
               data.table(var="Educational Attainment", val=NA, country=cur_country),
               data.table(var="Not attending: less than secondary (ref)", val = 0, conf_low = 0, conf_high = 0, group="Reference", signif = FALSE, country=cur_country),
               model_coefs_base[country == cur_country & grepl("ed_levelNot", var)],model_coefs_base[country == cur_country & grepl("ed_levelCur", var)],
               fill=T)
  plot_dt <- rbind(plot_dt, tmp, fill = T)
}
plot_dt$var <- plot_dt$var %>% factor(., levels=., labels=., ordered=T)

# variable labels pretty
y_axis_labs <- c("-----TIME-FIXED-----","Urban","HH head educ yrs","Beating is justified",
                 "DHS Wealth Index","1st quintile (Ref)", "2nd quintile", "3rd quintile", "4th quintile", "5th quintile",
                 "----TIME-VARYING----","Married/in-union",
                 "Educational Attainment","Not attending: less than secondary (Ref)","Not attending: completed secondary","Attending school") %>% rev()

# set which variable labels to bold
bold_titles <- c("-----TIME-FIXED-----","DHS Wealth Index","----TIME-VARYING----","Educational Attainment")
bold_labels <- ifelse(y_axis_labs %in% bold_titles, yes = "bold", no = "plain")

bold_color <- "#1b4e5f"
plain_color <- "black"
bold_colors <- ifelse(y_axis_labs %in% bold_titles, yes = bold_color, no = plain_color)

# plot individual country results
for (cur_country in unique(plot_dt$country)) {
  p.effects <- ggplot(data = plot_dt[country == cur_country], aes(y = var, x = val, alpha = signif)) +
    geom_vline(aes(xintercept = 0), linetype = "dashed", size = .5) +
    geom_pointrange(aes(xmin = conf_low, xmax = conf_high, color = group)) +
    geom_point(aes(fill = group), color = "white", shape = 21, size = 3.5) +
    scale_color_manual(values = c("Positive" = "#377EB8", "Negative" = "#E41A1C", "Reference"= "grey35")) +
    scale_fill_manual(values = c("Positive" = "#377EB8", "Negative" = "#E41A1C", "Reference" = "grey35")) +
    scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.2)) +
    scale_y_discrete(labels=y_axis_labs, limits = rev) +   
    labs(x="Coefficient", alpha = "Significance", fill = "Direction", color = "Direction") +
    coord_cartesian() +
    theme_sjplot2() + theme(axis.title.y = element_blank(), axis.line.x = element_line(color = NA), axis.line.y = element_line(color = NA),
                            axis.text.y = element_text(face = bold_labels, color = bold_colors, size = 10.5), panel.grid.major.x = element_line(linetype = "dotted"), 
                            panel.grid.minor.x = element_line(linetype = "dashed"), legend.text = element_text(size = 10), axis.title.x = element_text(size = 10),
                            axis.text.x.bottom = element_text(color = plain_color))
  p.effects
  
  ggsave(filename = file.path(out.dir, paste0(cur_country, "_part_1_model_outcome_sex_coefs.png")), width = 6.5, height = 5.5)
}

# get full country names
plot_dt[country == "GHA", country_full := "Ghana"]
plot_dt[country == "RWA", country_full := "Rwanda"]
plot_dt[country == "NPL", country_full := "Nepal"]
plot_dt[country == "MWI", country_full := "Malawi"]

# plot all countries in one plot
p.effects <- ggplot(data = plot_dt, aes(y = var, x = val, alpha = signif)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", size = .5) +
  geom_pointrange(aes(xmin = conf_low, xmax = conf_high, color = group)) +
  geom_point(aes(fill = group), color = "white", shape = 21, size = 3.5) +
  scale_color_manual(values = c("Positive" = "#377EB8", "Negative" = "#E41A1C", "Reference"= "grey35")) +
  scale_fill_manual(values = c("Positive" = "#377EB8", "Negative" = "#E41A1C", "Reference" = "grey35")) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.2)) +
  scale_y_discrete(labels=y_axis_labs, limits = rev) +
  labs(x="Coefficient", alpha = "Significance", fill = "Direction", color = "Direction") +
  coord_cartesian() +
  theme_bw() + theme(axis.title.y = element_blank(), axis.line.x = element_line(color = NA), axis.line.y = element_line(color = NA),
                     axis.text.y = element_text(face = bold_labels, color = bold_colors, size = 10.5), panel.grid.major.x = element_line(linetype = "dotted"), 
                     panel.grid.minor.x = element_line(linetype = "dashed"), legend.text = element_text(size = 10), axis.title.x = element_text(size = 10),
                     axis.text.x.bottom = element_text(color = plain_color), axis.ticks.y = element_blank(), strip.text.x = element_text(size = 11)) + 
  facet_wrap(~country_full, ncol = 4, scales = "free_x")
p.effects

ggsave(filename = file.path(out.dir, "ALL_part_1_model_outcome_sex_coefs.png"), width = 12, height = 6)


## PART 2 MODEL OUTCOME IS PREGNANCY AMONG EVER SEX --------------------------

# part 2 model
model_coefs_base <- model_coefs_final[model == "Two-stage model: part 2 outcome is first pregnancy among those who have had sex, categorical education"]
model_coefs_base[, country := str_sub(data, 1, 3)]

plot_dt <- data.table()
for (cur_country in unique(model_coefs_base$country)) {
  tmp <- rbind(data.table(var="----TIME-FIXED----", val=NA, country=cur_country),
               model_coefs_base[country == cur_country & grepl("urban|mean_yrs_schooling|beating", var)],
               data.table(var="DHS Wealth Index", val=NA, country=cur_country),
               data.table(var="1st quintile (ref)", val = 0, conf_low = 0, conf_high = 0, group="Reference", signif = FALSE, country=cur_country),
               model_coefs_base[country == cur_country & grepl("wealth", var)],
               data.table(var="----TIME-VARYING----", val=NA, country=cur_country),
               model_coefs_base[country == cur_country & grepl("curr_cohabit", var)],
               model_coefs_base[country == cur_country & grepl("mod_contra", var)],
               data.table(var="Educational Attainment", val=NA, country=cur_country),
               data.table(var="Not attending: less than secondary (ref)", val = 0, conf_low = 0, conf_high = 0, group="Reference", signif = FALSE, country=cur_country),
               model_coefs_base[country == cur_country & grepl("ed_levelNot", var)],model_coefs_base[country == cur_country & grepl("ed_levelCur", var)],
               fill=T)
  plot_dt <- rbind(plot_dt, tmp, fill = T)
}
plot_dt$var <- plot_dt$var %>% factor(., levels=., labels=., ordered=T)

# variable labels pretty
y_axis_labs <- c("-----TIME-FIXED-----","Urban","HH head educ yrs","Beating is justified",
                 "DHS Wealth Index","1st quintile (Ref)", "2nd quintile", "3rd quintile", "4th quintile", "5th quintile",
                 "----TIME-VARYING----","Married/in-union","Modern contraception",
                 "Educational Attainment","Not attending: less than secondary (Ref)","Not attending: completed secondary","Attending school") %>% rev()

# set which variable labels to bold
bold_titles <- c("-----TIME-FIXED-----","DHS Wealth Index","----TIME-VARYING----","Educational Attainment")
bold_labels <- ifelse(y_axis_labs %in% bold_titles, yes = "bold", no = "plain")

bold_color <- "#1b4e5f"
plain_color <- "black"
bold_colors <- ifelse(y_axis_labs %in% bold_titles, yes = bold_color, no = plain_color)

# plot individual country results
for (cur_country in unique(plot_dt$country)) {
  p.effects <- ggplot(data = plot_dt[country == cur_country], aes(y = var, x = val, alpha = signif)) +
    geom_vline(aes(xintercept = 0), linetype = "dashed", size = .5) +
    geom_pointrange(aes(xmin = conf_low, xmax = conf_high, color = group)) +
    geom_point(aes(fill = group), color = "white", shape = 21, size = 3.5) +
    scale_color_manual(values = c("Positive" = "#377EB8", "Negative" = "#E41A1C", "Reference"= "grey35")) +
    scale_fill_manual(values = c("Positive" = "#377EB8", "Negative" = "#E41A1C", "Reference" = "grey35")) +
    scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.2)) +
    scale_y_discrete(labels=y_axis_labs, limits = rev) +   
    labs(x="Coefficient", alpha = "Significance", fill = "Direction", color = "Direction") +
    coord_cartesian() +
    theme_sjplot2() + theme(axis.title.y = element_blank(), axis.line.x = element_line(color = NA), axis.line.y = element_line(color = NA),
                            axis.text.y = element_text(face = bold_labels, color = bold_colors, size = 10.5), panel.grid.major.x = element_line(linetype = "dotted"), 
                            panel.grid.minor.x = element_line(linetype = "dashed"), legend.text = element_text(size = 10), axis.title.x = element_text(size = 10),
                            axis.text.x.bottom = element_text(color = plain_color))
  p.effects
  
  ggsave(filename = file.path(out.dir, paste0(cur_country, "_part_2_model_outcome_preg_ever_sex_coefs.png")), width = 6.5, height = 5.5)
}

# get full country names
plot_dt[country == "GHA", country_full := "Ghana"]
plot_dt[country == "RWA", country_full := "Rwanda"]
plot_dt[country == "NPL", country_full := "Nepal"]
plot_dt[country == "MWI", country_full := "Malawi"]

# plot all countries in one plot
p.effects <- ggplot(data = plot_dt, aes(y = var, x = val, alpha = signif)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", size = .5) +
  geom_pointrange(aes(xmin = conf_low, xmax = conf_high, color = group)) +
  geom_point(aes(fill = group), color = "white", shape = 21, size = 3.5) +
  scale_color_manual(values = c("Positive" = "#377EB8", "Negative" = "#E41A1C", "Reference"= "grey35")) +
  scale_fill_manual(values = c("Positive" = "#377EB8", "Negative" = "#E41A1C", "Reference" = "grey35")) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.2)) +
  scale_y_discrete(labels=y_axis_labs, limits = rev) +
  labs(x="Coefficient", alpha = "Significance", fill = "Direction", color = "Direction") +
  coord_cartesian() +
  theme_bw() + theme(axis.title.y = element_blank(), axis.line.x = element_line(color = NA), axis.line.y = element_line(color = NA),
                     axis.text.y = element_text(face = bold_labels, color = bold_colors, size = 10.5), panel.grid.major.x = element_line(linetype = "dotted"), 
                     panel.grid.minor.x = element_line(linetype = "dashed"), legend.text = element_text(size = 10), axis.title.x = element_text(size = 10),
                     axis.text.x.bottom = element_text(color = plain_color), axis.ticks.y = element_blank(), strip.text.x = element_text(size = 11)) + 
  facet_wrap(~country_full, ncol = 4, scales = "free_x")
p.effects

ggsave(filename = file.path(out.dir, "ALL_part_2_model_outcome_preg_ever_sex_coefs.png"), width = 12, height = 6)

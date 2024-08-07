#-------------------Header------------------------------------------------
# Project: IHME ASHER Decomposition
# Purpose: Plot means in covariates and outcomes by admin 1
# Date: 6/19/2024
# Notes:

# SET-UP ----------------------------------------------------------

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

# load packages
pacman::p_load(magrittr,tidyverse,parallel,plyr,dplyr,haven,survey,tools,devtools,sf, viridis,ggpubr,data.table)

# in/out
out.dir <- 'FILEPATH'

# create function to convert CMC date to year and month
get_cmc_year <- function(x) 1900 + as.integer((as.integer(x) - 1) / 12)
get_cmc_month <- function(x) as.integer(x) - 12 * (get_cmc_year(x) - 1900)

# current admin 2 shapefile
shp2 <- st_read(file.path("/FILEPATH/lbd_standard_admin_2.shp"))

# variable labels
variable_labs <- list("age" = "Age",
                      "age_1st_sex_imp" = "Age at first sex",
                      "curr_cohabit" = "Marital status",
                      "educ_single_yrs" = "Years of education",
                      "had_intercourse" = "Has had intercourse",
                      "wealth_index" = "Wealth Index",
                      "beating_just" = "Believes beating is justified",
                      "any_birth_preg_2_yr_dhs" = "Pregnancy, including terminations, miscarriages\n and still births, in the last two years",
                      "any_birth_preg_2_yr_mics" = "Pregnancy in the last two years",
                      "mcpr" = "Modern contraceptive prevlance",
                      "mean_yrs_schooling_head" = "Years of education of household head",
                      "unmet_need" = "Unmet need for contraception")
# ------ plot means per country by region -------
plot_admin1 <- function(cur_country, admin_1_dt_file){
  
  # ihme_loc_id of current country
  if (cur_country == "cm") country <- "Cameroon"
  if (cur_country == "gh") country <- "Ghana"
  if (cur_country == "mw") country <- "Malawi"
  if (cur_country == "np") country <- "Nepal"
  if (cur_country == "rw") country <- "Rwanda"
  
  plot_dt <- fread(file.path(out.dir, '01_processed', admin_1_dt_file))
  
  # for now, only plot baseline and endline
  baseline <- min(plot_dt$year)
  endline <- max(plot_dt$year)
  plot_dt <- plot_dt[year %in% c(baseline,endline)]
  
  plot_dt[, var_lab := unlist(variable_labs[plot_dt$variable])]
  # reshape long to wide
  plot_dt <- plot_dt[, -c('se', 'ci_u', 'ci_l')]

  plot_dt_wide <- dcast(plot_dt,year+survey+admin_1_shp ~ var_lab, value.var= "mean")
  
  # subset admin 2 shapefile 
  adm2 <- subset(shp2, shp2$ADM0_NAME == country) %>% dplyr::select(ADM1_NAME, ADM2_NAME, geometry)
  setnames(adm2, c("ADM1_NAME", "ADM2_NAME"), c("admin_1_shp", "admin_2_shp"))

  variable_means <- names(plot_dt_wide)
  variable_means <- variable_means[! variable_means %in% c("year", "survey", "admin_1_shp")]

  plot_dt_wide<- merge(adm2,plot_dt_wide,by = c("admin_1_shp"),allow.cartesian=TRUE)
  
    for (var in variable_means) {
      pdf(file.path('FILEPATH', cur_country, paste0(paste(cur_country, var, sep = "_"), ".pdf")))
     gg <- ggplot(plot_dt_wide, aes(fill = get(var)))  +
        facet_wrap(~year)+
        geom_sf(color = "black", linewidth = 0.3) +
       scale_fill_viridis()+
       #scale_fill_gradient(low = "red", high = "green", na.value = NA)+
      labs(fill = "Mean", title = paste0(eval(var), ", ", country, ', 15-19')) +
        theme(panel.background = element_rect(fill = "white"),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              text = element_text(size = 14),
              panel.border = element_rect(colour = "black", fill=NA, size=2),
               panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.background = element_rect(fill = "white", color = "white"),
              strip.background = element_blank())
     
     print(gg)
      dev.off()
    }
    
}
plot_admin1('cm', 'CMR_admin_1_means.csv')
plot_admin1('gh', 'GHA_admin_1_means.csv')
plot_admin1('mw', 'MWI_admin_1_means.csv')
plot_admin1('np', 'NPL_admin_1_means.csv')
plot_admin1('rw', 'RWA_admin_1_means.csv')

# ------ plot outcome across all countries by region -------
plot_outcome <- function(outcome){
  # outcome is either any pregnancy in the last two years (any_birth_preg_2_yr_mics) (so all countries have the same outcome) or any pregnancy in the last 
  # two years including terminations (any_birth_preg_2_yr_dhs) for all countries (except malawi) and any pregnancy in the last two years (any_birth_preg_2_yr_mics) 
  # (in malawi only)
  
  files <- list.files(file.path(out.dir, '01_processed'))
  files <- files[grepl('admin_1_means.csv', files)]
  plot_dt <- rbindlist(lapply(file.path(out.dir,'01_processed', files), fread), fill = T)
  
  # NPL 2001 is ever married only
  plot_dt <- plot_dt[survey != "NPL_DHS4_2001"]

  # for now, only plot baseline and endline
  plot_dt[, baseline := min(year), by = "country"]
  plot_dt[, endline := max(year), by = "country"]
  plot_dt <- plot_dt[year == baseline | year == endline]
  
  # restrict to outcome 
  if (outcome == "any_birth_preg_2_yr_mics"){
    plot_dt <- plot_dt[variable == outcome]
  } else{
    # malawi is any_birth_preg_2_yr_mics
    # all other countries are any_birth_preg_2_yr_dhs
    mw_dt <- plot_dt[country == "MWI" & variable == outcome]
    other_dt <- plot_dt[country != "MWI" & variable == 'any_birth_preg_2_yr_mics']
    plot_dt <- rbind(mw_dt, other_dt)
  }
  
  plot_dt[, diff := mean - lag(mean), by = c("admin_1_shp")]
  plot_dt_check <- plot_dt[, c("year", "country", "admin_1_shp", "mean", "ci_l", "ci_u", "diff")]
  
  ## for number plugging appendix
  plot_dt_check[, mean := round(mean, 2)]
  plot_dt_check[, ci_l := round(ci_l, 2)]
  plot_dt_check[, ci_u := round(ci_u, 2)]
  plot_dt_check[, diff := round(diff, 2)]
  
  plot_dt_check_cmr <- plot_dt_check[country == "CMR"]
  plot_dt_check_gha <- plot_dt_check[country == "GHA"]
  plot_dt_check_mwi <- plot_dt_check[country == "MWI"]
  plot_dt_check_npl <- plot_dt_check[country == "NPL"]
  plot_dt_check_rwa <- plot_dt_check[country == "RWA"]
  
  plot_dt <- plot_dt[!is.na(diff)]
  plot_dt[, var_lab := unlist(variable_labs[plot_dt$variable])]
  title_lab <- ifelse(outcome == 'any_birth_preg_2_yr_mics', paste0(unique(plot_dt$var_lab), ', 15-19'),
                      "Pregnancy in the last two years only in Malawi, terminations, miscarriages, \n and still births included in Cameroon, Ghana, Rwanda, and Nepal, \n in the past 2 years, 15-19")
  
  # Plot each country with the same scale -----    
  # plot Cameroon
  adm2_tmp <- subset(shp2, shp2$ADM0_NAME == "Cameroon") %>% dplyr::select(ADM1_NAME, ADM2_NAME, geometry)
  setnames(adm2_tmp, c("ADM1_NAME", "ADM2_NAME"), c("admin_1_shp", "admin_2_shp"))
  plot_dt_tmp <- merge(adm2_tmp,plot_dt[country == "CMR"],by = c("admin_1_shp"),allow.cartesian=TRUE)
  gg_cmr <- ggplot(plot_dt_tmp)  +
    geom_sf(aes(fill =diff, geometry=geometry)) +
    scale_fill_viridis(limits = c(-0.55, 0.055))+
    labs(fill = "Difference between baseline and endline", title = "Cameroon")+
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          text = element_text(size = 14),
          panel.border = element_rect(colour = "black", fill=NA, size=2),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
   # facet_wrap(~year)+
    guides(fill = 'none')
  
  # plot Ghana
  adm2_tmp <- subset(shp2, shp2$ADM0_NAME == "Ghana") %>% dplyr::select(ADM1_NAME, ADM2_NAME, geometry)
  setnames(adm2_tmp, c("ADM1_NAME", "ADM2_NAME"), c("admin_1_shp", "admin_2_shp"))
  plot_dt_tmp <- merge(adm2_tmp,plot_dt[country == "GHA"],by = c("admin_1_shp"),allow.cartesian=TRUE)
  gg_gha <- ggplot(plot_dt_tmp)  +
    geom_sf(aes(fill =diff, geometry=geometry)) +
    scale_fill_viridis(limits = c(-0.55, 0.055))+
    labs(fill = "Difference between baseline and endline", title = "Ghana")+
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          text = element_text(size = 14),
          panel.border = element_rect(colour = "black", fill=NA, size=2),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    #facet_wrap(~year)+
    guides(fill = 'none')
  
  # plot Malawi
  adm2_tmp <- subset(shp2, shp2$ADM0_NAME == "Malawi") %>% dplyr::select(ADM1_NAME, ADM2_NAME, geometry)
  setnames(adm2_tmp, c("ADM1_NAME", "ADM2_NAME"), c("admin_1_shp", "admin_2_shp"))
  plot_dt_tmp <- merge(adm2_tmp,plot_dt[country == "MWI"],by = c("admin_1_shp"),allow.cartesian=TRUE)
  gg_mwi <- ggplot(plot_dt_tmp)  +
    geom_sf(aes(fill =diff, geometry=geometry)) +
    scale_fill_viridis(limits = c(-0.55, 0.055))+
    labs(fill = "Difference between baseline and endline", title = "Malawi")+
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          text = element_text(size = 14),
          panel.border = element_rect(colour = "black", fill=NA, size=2),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
   # facet_wrap(~year)+
    guides(fill = 'none')
  
  # plot Nepal
  adm2_tmp <- subset(shp2, shp2$ADM0_NAME == "Nepal") %>% dplyr::select(ADM1_NAME, ADM2_NAME, geometry)
  setnames(adm2_tmp, c("ADM1_NAME", "ADM2_NAME"), c("admin_1_shp", "admin_2_shp"))
  plot_dt_tmp <- merge(adm2_tmp,plot_dt[country == "NPL"],by = c("admin_1_shp"),allow.cartesian=TRUE)
  gg_npl <- ggplot(plot_dt_tmp)  +
    geom_sf(aes(fill =diff, geometry=geometry)) +
    scale_fill_viridis(limits = c(-0.55, 0.055))+
    labs(fill = "Difference between baseline and endline", title = "Nepal")+
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          text = element_text(size = 14),
          panel.border = element_rect(colour = "black", fill=NA, size=2),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
  #  facet_wrap(~year)+
    guides(fill = 'none')
  
  # plot Rwanda
  adm2_tmp <- subset(shp2, shp2$ADM0_NAME == "Rwanda") %>% dplyr::select(ADM1_NAME, ADM2_NAME, geometry)
  setnames(adm2_tmp, c("ADM1_NAME", "ADM2_NAME"), c("admin_1_shp", "admin_2_shp"))
  plot_dt_tmp <- merge(adm2_tmp,plot_dt[country == "RWA"],by = c("admin_1_shp"),allow.cartesian=TRUE)
  gg_rwa <- ggplot(plot_dt_tmp)  +
    geom_sf(aes(fill =diff, geometry=geometry)) +
    scale_fill_viridis(limits = c(-0.55, 0.055))+
    labs(fill = "Difference between baseline and endline", title = "Rwanda")+
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          text = element_text(size = 14),
          panel.border = element_rect(colour = "black", fill=NA, size=2),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    #facet_wrap(~year)+
    guides(fill = 'none')
  
  gg_rwa_legend <- ggplot(plot_dt_tmp)  +
    geom_sf(aes(fill =diff, geometry=geometry)) +
    scale_fill_viridis(limits = c(-0.55, 0.055))+
    labs(fill = "Difference between baseline and endline", title = "Rwanda")+
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          text = element_text(size = 14),
          panel.border = element_rect(colour = "black", fill=NA, size=2),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          strip.background = element_blank()) 
  
  legend <-cowplot::get_plot_component(gg_rwa_legend, 'guide-box', return_all = TRUE)[[1]]
  
  pdf(file.path('FILEPATH', paste0('panel_fixed_scale_', outcome, ".pdf")), height = 10, width = 12)
  x<-ggarrange(gg_cmr, gg_gha, gg_mwi, nrow=1)
  y<-ggarrange(gg_npl, gg_rwa,legend,nrow=1)
  z <- ggarrange(x,y,nrow=2)
  z<-annotate_figure(z, top = text_grob(title_lab, size = 20))
  print(z)
  dev.off()
  
  # Plot each country with flexible scales -----    
  # plot Cameroon
  adm2_tmp <- subset(shp2, shp2$ADM0_NAME == "Cameroon") %>% dplyr::select(ADM1_NAME, ADM2_NAME, geometry)
  setnames(adm2_tmp, c("ADM1_NAME", "ADM2_NAME"), c("admin_1_shp", "admin_2_shp"))
  plot_dt_tmp <- merge(adm2_tmp,plot_dt[country == "CMR"],by = c("admin_1_shp"),allow.cartesian=TRUE)
  gg_cmr <- ggplot(plot_dt_tmp)  +
    geom_sf(aes(fill =diff, geometry=geometry)) +
    scale_fill_viridis()+
    labs(fill = "Difference between baseline and endline", title = "Cameroon")+
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          text = element_text(size = 14),
          panel.border = element_rect(colour = "black", fill=NA, size=2),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5)) 
  
  # plot Ghana
  adm2_tmp <- subset(shp2, shp2$ADM0_NAME == "Ghana") %>% dplyr::select(ADM1_NAME, ADM2_NAME, geometry)
  setnames(adm2_tmp, c("ADM1_NAME", "ADM2_NAME"), c("admin_1_shp", "admin_2_shp"))
  plot_dt_tmp <- merge(adm2_tmp,plot_dt[country == "GHA"],by = c("admin_1_shp"),allow.cartesian=TRUE)
  gg_gha <- ggplot(plot_dt_tmp)  +
    geom_sf(aes(fill =diff, geometry=geometry)) +
    scale_fill_viridis()+
    labs(fill = "Difference between baseline and endline", title = "Ghana")+
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          text = element_text(size = 14),
          panel.border = element_rect(colour = "black", fill=NA, size=2),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5)) 
  
  # plot Malawi
  adm2_tmp <- subset(shp2, shp2$ADM0_NAME == "Malawi") %>% dplyr::select(ADM1_NAME, ADM2_NAME, geometry)
  setnames(adm2_tmp, c("ADM1_NAME", "ADM2_NAME"), c("admin_1_shp", "admin_2_shp"))
  plot_dt_tmp <- merge(adm2_tmp,plot_dt[country == "MWI"],by = c("admin_1_shp"),allow.cartesian=TRUE)
  gg_mwi <- ggplot(plot_dt_tmp)  +
    geom_sf(aes(fill =diff, geometry=geometry)) +
    scale_fill_viridis()+
    labs(fill = "Difference between baseline and endline", title = "Malawi")+
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          text = element_text(size = 14),
          panel.border = element_rect(colour = "black", fill=NA, size=2),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  # plot Nepal
  adm2_tmp <- subset(shp2, shp2$ADM0_NAME == "Nepal") %>% dplyr::select(ADM1_NAME, ADM2_NAME, geometry)
  setnames(adm2_tmp, c("ADM1_NAME", "ADM2_NAME"), c("admin_1_shp", "admin_2_shp"))
  plot_dt_tmp <- merge(adm2_tmp,plot_dt[country == "NPL"],by = c("admin_1_shp"),allow.cartesian=TRUE)
  gg_npl <- ggplot(plot_dt_tmp)  +
    geom_sf(aes(fill =diff, geometry=geometry)) +
    scale_fill_viridis()+
    labs(fill = "Difference between baseline and endline", title = "Nepal")+
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          text = element_text(size = 14),
          panel.border = element_rect(colour = "black", fill=NA, size=2),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5)) 
  
  # plot Rwanda
  adm2_tmp <- subset(shp2, shp2$ADM0_NAME == "Rwanda") %>% dplyr::select(ADM1_NAME, ADM2_NAME, geometry)
  setnames(adm2_tmp, c("ADM1_NAME", "ADM2_NAME"), c("admin_1_shp", "admin_2_shp"))
  plot_dt_tmp <- merge(adm2_tmp,plot_dt[country == "RWA"],by = c("admin_1_shp"),allow.cartesian=TRUE)
  gg_rwa <- ggplot(plot_dt_tmp)  +
    geom_sf(aes(fill =diff, geometry=geometry)) +
    scale_fill_viridis()+
    labs(fill = "Difference between baseline and endline", title = "Rwanda")+
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          text = element_text(size = 14),
          panel.border = element_rect(colour = "black", fill=NA, size=2),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5)) 
  
  pdf(file.path('FILEPATH', paste0('panel_free_scale_', outcome, ".pdf")), height = 10, width = 12)
  x<-ggarrange(gg_cmr, gg_gha, gg_mwi, nrow=1)
  y<-ggarrange(gg_npl, gg_rwa,nrow=1)
  z <- ggarrange(x,y,nrow=2)
  z<-annotate_figure(z, top = text_grob(title_lab, size = 20))
  print(z)
  dev.off()
  
}
## call function to plot ------
plot_outcome('any_birth_preg_2_yr_mics')
plot_outcome('any_birth_preg_2_yr_dhs')

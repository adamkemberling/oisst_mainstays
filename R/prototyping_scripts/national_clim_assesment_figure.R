# Creating figure for National Climate Assessment, NE Chapter
# Visualize Heatwave periodicity with annual temperatures:



# Packages
library(heatwaveR)
library(gmRi)
library(tidyverse)
library(lubridate)
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Functions
# Provides a wrapper for heatwaveR function that detects MHW and MCS
# Also provides functions for supplementing date information
suppressPackageStartupMessages(source(here::here("R/oisst_support_funs.R")))
suppressPackageStartupMessages(source(here::here("R/temp_report_support.R")))


# Load data for Gulf of Maine GMRI bounding Box - Not used for Climate Assesment Figure
# Used for comparison when developing figure, bounding box given below
# xmin: -70.875 , xmax: -66.375, ymin: 40.375, ymax: 45.125
andy_gom <- oisst_access_timeseries(
    "gmri",
    poly_name = "apershing gulf of maine",
    box_location = "cloudstorage") %>%
  mutate(tod = format(time, format = "%H:%M:%S"))

# Load the timeseries for the Gulf of Maine ecological production unit
# For dimensions: ecodata::epu_sf %>% filter(EPU == "GOM") %>% st_bbox() # sf and ecodata packages
epu_gom <- oisst_access_timeseries(
    region_family = "epu", 
    poly_name = "gom", 
    box_location = "cloudstorage") %>% 
  mutate(tod = format(time, format = "%H:%M:%S")) 



# Put in list to mirror steps
gom_list <- list("GMRI - Gulf of Maine" = andy_gom,
                 "EPU - Gulf of Maine" = epu_gom)


# Format timeseries for group estimates
region_timeseries <- map(gom_list, function(x){
  x %>% 
  mutate(
    time = as.Date(time),
    area_wtd_f = as_fahrenheit(area_wtd_sst),
    anom_f     = as_fahrenheit(area_wtd_anom, "anomalies")) %>% 
  distinct(time, .keep_all = T) %>% 
  supplement_season_info() %>% 
  filter(year %in% c(1982:2021))
})



####  Get heatwave statuses for each day:


# Note: Uses area weighted sst by default
region_hw <- map(region_timeseries, function(x){
  pull_heatwave_events(
  temperature_timeseries = x, 
  threshold = 90, 
  clim_ref_period = c("1982-01-01", "2011-12-31")) %>% 
  supplement_hw_data() %>% 
  filter(doy != 366) %>% 
  mutate(year = year(time),
         yday = yday(time),
         yr_rev = factor(year),
         yr_rev = fct_rev(yr_rev),
         flat_date = as.Date("2000-01-01") + yday - 1)
})



# Process annual points as dates
hw_annual <- map(region_hw, function(x){ 
  x %>% 
  group_by(yr) %>% 
  summarise(sst_anom = mean(sst_anom),
            anom_f = mean(anom_f),
            .groups = "drop") %>% 
  mutate(yr_as_dt = as.Date(str_c(yr, "-06-15")))
  })



# Plot the hw days below as hash marks:
index_list <- list(1, 2) %>% setNames(names(region_hw))
figures <- imap(index_list, function(.index, .name){
  plot_x <- ggplot() +
    geom_line(data = region_hw[[.index]], aes(time, anom_f), color = "gray60", alpha = 0.5) +
    geom_line(data = hw_annual[[.index]], aes(yr_as_dt, anom_f), linewidth = 1) +
    geom_segment(data = filter(region_hw[[.index]], mhw_event),
                aes(x = time, xend = time,
                    y = -4.6, yend = -4.3), 
                linewidth = .2, color = "darkred") +
    scale_y_continuous(labels = scales::number_format(suffix = " \u00b0F")) +
    theme(plot.caption = element_text(hjust = 0)) + # set the left align for caption
    labs(x = "Date", 
         y = "Temperature Anomaly",
         title = "Gulf of Maine SST Anomalies",
         caption = "")
  
  return(plot_x)
})


# Display the figure
figures$`EPU - Gulf of Maine`



# Save the figure(s)
iwalk(figures, function(fig_x, label_y){
  ggsave(plot = fig_x, filename = here::here(
    "local_data/", str_c(label_y,"_sst_anomalies.svg")
  ))
  
  
})





####  Figure Formatting Tests:  ####

# # Just change the line color
# ggplot() +
#   geom_line(data = region_hw, aes(time, sst_anom, color = mhw_event, group = 1), alpha = 0.5) +
#   geom_line(data = hw_annual, aes(yr_as_dt, sst_anom), size = 1) +
#   scale_color_manual(values = c("TRUE" = "darkred", "FALSE" = "gray60"))  +
#   labs(x = "Date", y = "SST Anom")

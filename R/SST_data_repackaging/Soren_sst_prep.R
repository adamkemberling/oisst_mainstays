# For Soren Walljasper
# Nat-Geo
# Request for Annual Gulf of Maine Temperatures
# And Percentile Warming


# Libraries
library(tidyverse)
library(gmRi)
library(heatwaveR)


suppressPackageStartupMessages( source(here::here("R/oisst_support_funs.R"), verbose = FALSE) )


# Data Prep



# 1. Gulf of MAine Timeseries

oisst_path <- cs_path("res", "OISST/oisst_mainstays")

# File paths for various extents based on "apershing_gulf_of_maine"
region_paths <- get_timeseries_paths(
  region_group = "gmri_sst_focal_areas", 
  box_location = "cloudstorage")

# Timeseries Path
timeseries_path <- region_paths[["apershing_gulf_of_maine"]][["timeseries_path"]]

# Load the timeseries
region_timeseries <- read_csv(
  timeseries_path, 
  col_types = cols(), 
  guess_max = 1e6)


# Clean up the data - add labels
region_timeseries <- region_timeseries %>% 
  mutate(time = as.Date(time)) %>% 
  distinct(time, .keep_all = T) %>% 
  filter(lubridate::year(time) %in% c(1982:2023))

# Get anomalioes from the new reference period
region_hw <- pull_heatwave_events(
  temperature_timeseries = region_timeseries, 
  threshold = 90, 
  clim_ref_period = c("1991-01-01", "2020-12-31")) %>% 
  filter(doy != 366)

# Pull out the necessary info
# Make Annual
# Rename for clarity
region_sst <- region_hw %>% 
  select(time, sst, sst_anom) %>% 
  group_by(year = lubridate::year(time)) %>% 
  summarise(
    avg_sst_c = mean(sst, na.rm = T),
    avg_sst_anom_c = mean(sst_anom, na.rm = T),
    .groups = "drop") %>% 
  mutate(
    avg_sst_f = as_fahrenheit(avg_sst_c, data_type  = "temperature"),
    avg_sst_anom_f = as_fahrenheit(avg_sst_anom_c, data_type  = "anomalies"))



# Visual Check:
ggplot(region_sst) +
  geom_line(aes(year, avg_sst_f), color = "darkred", linewidth = 1) +
  geom_hline(yintercept = mean(region_sst$avg_sst_f), color = "black", linewidth = 0.5)
ggplot(region_sst) +
  geom_line(aes(year, avg_sst_anom_f), color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5)


# Save Locally
write_csv(region_sst, here::here("local_data/GulfOfMaine_Annual_SST_1982to2023.csv"))


####____####
# 2. Warming Rate Percentiles


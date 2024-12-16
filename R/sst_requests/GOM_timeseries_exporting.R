# Load a standard timeseries
# select area weighted as only values
# filter out recent dates (eliminates preliminary data)
# export as a single timeseries


####  Packages  ####
{
  library(lubridate)
  library(here)
  library(rnaturalearth)
  library(sf)
  library(scales)
  library(gmRi)
  library(heatwaveR)
  library(patchwork)
  library(tidyverse)
}



# # Support Functions
source(here("R/oisst_support_funs.R"), verbose = FALSE)
source(here("R/temp_report_support.R"), verbose = FALSE)

# Conflicts
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")

# Box paths
oisst_path <- cs_path("res", "OISST/oisst_mainstays")


# File paths for various extents based on "apershing_gulf_of_maine"
region_paths <- get_timeseries_paths(
  region_group = "gmri_sst_focal_areas", 
  box_location = "cloudstorage")


# Timeseries Path +
timeseries_path <- region_paths[["apershing_gulf_of_maine"]][["timeseries_path"]]

# Load timeseries of SST for Region
# Run with climatology from heatwaveR to ensure consistency 
# and ensure climatology is known
region_timeseries <- read_csv(
  timeseries_path, 
  col_types = cols(), 
  guess_max = 1e5)  %>% 
  distinct(time, .keep_all = T) %>% 
  filter(time <= Sys.Date() - 16) %>% 
  rename(temp = area_wtd_sst) %>% 
  pull_heatwave_events(
    temperature_timeseries = .,
    threshold = 90, 
    clim_ref_period = c("1991-01-01", "2020-12-31"))


# Format timeseries for group estimates
region_timeseries <- region_timeseries %>% 
  transmute(
    date = as.Date(time),
    sst_c = sst,
    clim_c = seas,
    sst_anom_c = sst_anom,
    sst_f      = as_fahrenheit(sst, "temperature"),
    clim_f     = as_fahrenheit(seas, "temperature"),
    sst_anom_f     = as_fahrenheit(sst_anom_c, "anomalies")) 


# Ensure that C to F transformation is happening consistently
region_timeseries %>% 
  transmute(
    date = date,
    temp = ((sst_c * (9/5))+32) - sst_f, # sst
    clim = ((clim_c * (9/5))+32) - clim_f, # climatology
    anom = (sst_anom_c * (9/5)) - sst_anom_f # anomalies
  ) %>% 
  pivot_longer(cols = -date, names_to = "var", values_to = "differences") %>% 
  ggplot(aes(date, differences)) +
  facet_wrap(~var, ncol = 1) +
  geom_point()


# Recreate the major yearly warming plot

# Summarize by year to return mean annual anomalies and variance
annual_summary <- region_timeseries %>% 
  filter(
    date >= as.Date("1982-01-01"),
    date <= as.Date("2023-12-31")) %>% 
  group_by(year = year(date)) %>% 
  summarise(
    sst_f = mean(sst_f, na.rm = T),
    anom_f = mean(sst_anom_f, na.rm = T), 
    .groups = "drop") %>% 
  arrange(desc(sst_f)) %>% 
  mutate(sst_rank = row_number()) %>% 
  mutate(yr_as_dtime = as.Date(paste0(year, "-07-02")))



# plot it
ggplot(annual_summary, aes(year, sst_f)) +
  geom_point(size = 1) +
  geom_line(linewidth = 1.2) +
  geom_smooth(method = "lm")

ggplot(annual_summary, aes(year, anom_f)) +
  geom_point(size = 1) +
  geom_line(linewidth = 1.2) +
  geom_smooth(method = "lm")



# Export it
date_lab <- str_replace_all(Sys.Date(), "-", "")
write_csv(
  region_timeseries, 
  str_c(
    here::here("local_data/GOM_timeseries_exports/GMRI_GOM_Daily_OISSTv2_"), 
    date_lab, ".csv")
  )


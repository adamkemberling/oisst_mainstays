# Producing EPU Anomaly Timeseries

library(gmRi)
library(tidyverse)
library(heatwaveR)
library(here)


# Support Functions
source(here("R/oisst_support_funs.R"), verbose = FALSE)
source(here("R/temp_report_support.R"), verbose = FALSE)

# Box paths
oisst_path <- cs_path("res", "OISST/oisst_mainstays")



# EPU SSTs
epu_paths <- gmRi::get_timeseries_paths(region_group = "epu", box_location = "cloudstorage")
epu_ts <- map(epu_paths, function(x){
  timeseries_path <- x$timeseries_path
  
  # Load timeseries of SST for Region
  region_timeseries <- read_csv(
    timeseries_path, 
    col_types = cols(), 
    guess_max = 1e5) 
  
  # Format timeseries for group estimates
  region_timeseries <- region_timeseries %>% 
    mutate(
      time = as.Date(time),
      area_wtd_f = as_fahrenheit(area_wtd_sst),
      anom_f     = as_fahrenheit(area_wtd_anom, "anomalies")) %>% 
    distinct(time, .keep_all = T) %>% 
    supplement_season_info() %>% 
    filter(year %in% c(1982:2024))
  
  
  ####  Get heatwave statuses for each day:
  
  # Uses area weighted sst by default
  region_hw <- pull_heatwave_events(
    temperature_timeseries = region_timeseries,
    threshold = 90, 
    clim_ref_period = c("1991-01-01", "2020-12-31")) %>% 
    supplement_hw_data() %>% 
    filter(doy != 366) 
  
  return(region_hw)
  
  }) %>% 
  bind_rows(.id = "EPU")


theme_set(theme_gmri())
epu_seasonal <- epu_ts %>% 
 group_by(EPU, season_yr, season_eng) %>% 
  summarise(
    sst = mean(sst, na.rm = T), 
    sst_anom = mean(sst_anom, na.rm = T),
    seas = mean(seas, na.rm = T))

library(ggpmisc)
epu_seasonal %>% 
  mutate(EPU = factor(EPU, levels = c("SS", "GOM", "GB", "MAB"))) %>% 
  filter(season_eng %in% c("Spring", "Fall")) %>% 
  ggplot(aes(season_yr, sst_anom, color = fct_rev(season_eng))) +
  geom_point(size = 1.5) +
  geom_line(alpha = 0.6) +
  geom_smooth(
    method = "lm", 
    formula = y~x, 
    se = F) +
  ggpmisc::stat_poly_eq(
    method = "lm",
    ggpmisc::use_label("eq"), 
    formula = y ~ x,
    geom = "label", 
    label.x = 2005, label.y = -2) +
  scale_color_gmri() +
  facet_grid(fct_rev(season_eng)~EPU) +
  labs(
    x = "Year",
    y = "SST Anomaly\n(1991-2020 Climatology)",
    color = "Season")




####  Heatwave days Per Month
# GoM or US Northeast Continental shelf per month


####  Packages  ####
library(rnaturalearth)
library(sf)
library(gmRi)
library(here)
library(raster)
library(patchwork)
library(heatwaveR)
library(tidyverse)

#box paths
box_paths <- research_access_paths()
res_path <-  box_paths$res

# Support Functions
source(here("R/oisst_support_funs.R"))
source(here("R/temp_report_support.R"))

# set ggplot theme for figures
theme_set(theme_bw())


####  OISST Data  ####

# gulf of maine
gom_oisst <- oisst_access_timeseries(oisst_path = box_paths$oisst_mainstays, 
                                     region_family = "gmri focus areas", 
                                     poly_name = "apershing gulf of maine")%>% 
  mutate(time = as.Date(time))


# NE shelf
shelf_oisst <- oisst_access_timeseries(oisst_path = box_paths$oisst_mainstays, 
                                       region_family = "lme", 
                                       poly_name = "northeast us continental shelf") %>% 
  mutate(time = as.Date(time))




#####  Pull Heatwaves  ####
gom_hw <- pull_heatwave_events(temperature_timeseries = gom_oisst, 
                               threshold = 90, 
                               clim_ref_period = c("1982-02-01", "2011-12-31")) %>% 
  mutate(year = lubridate::year(time),
         month = lubridate::month(time))


shelf_hw  <- pull_heatwave_events(temperature_timeseries = shelf_oisst, 
                                  threshold = 90, 
                                  clim_ref_period = c("1982-02-01", "2011-12-31")) %>% 
  mutate(year = lubridate::year(time),
         month = lubridate::month(time))




####  Total Heatwaves  ####

gom_hw_summary <- gom_hw %>% 
  group_by(year, month) %>% 
  summarise(num_hw_days = sum(mhw_event)) %>% 
  mutate(area = "Gulf of Maine")

shelf_hw_summary <- shelf_hw %>% 
  group_by(year, month) %>% 
  summarise(num_hw_days = sum(mhw_event)) %>% 
  mutate(area = "Northeast US Continental Shelf LME")

combined_summary <- full_join(gom_hw_summary, shelf_hw_summary)


# Save the heatwave data
write_csv(gom_hw_summary, here("local_data/gom_heatwave_timeline.csv"))

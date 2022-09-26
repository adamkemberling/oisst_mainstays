

# Heatwave periodicity with annual temps:

# Packages
library(heatwaveR)
library(gmRi)
library(tidyverse)
library(lubridate)
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# functions
suppressPackageStartupMessages(source(here::here("R/oisst_support_funs.R")))
suppressPackageStartupMessages(source(here::here("R/temp_report_support.R")))


# File paths for various extents based on "apershing_gulf_of_maine"
region_paths <- get_timeseries_paths(region_group = "gmri_sst_focal_areas", 
                                     mac_os = "mojave")

# Timeseries Path
gom_path <- region_paths[["apershing_gulf_of_maine"]][["timeseries_path"]]
gom <- read_csv(gom_path, 
                col_types = cols(), 
                guess_max = 1e6) %>% 
        mutate(tod = format(time, format = "%H:%M:%S")) 


# Format timeseries for group estimates
region_timeseries <- gom %>% 
  mutate(
    time = as.Date(time),
    area_wtd_f = as_fahrenheit(area_wtd_sst),
    anom_f     = as_fahrenheit(area_wtd_anom, "anomalies")) %>% 
  distinct(time, .keep_all = T) %>% 
  supplement_season_info() %>% 
  filter(year %in% c(1982:2021))



####  Get heatwave statuses for each day:

# Uses area weighted sst by default
region_hw <- pull_heatwave_events(
  temperature_timeseries = region_timeseries, 
  threshold = 90, 
  clim_ref_period = c("1982-01-01", "2011-12-31")) %>% 
  supplement_hw_data() %>% 
  filter(doy != 366) %>% 
  mutate(year = year(time),
         yday = yday(time),
         yr_rev = factor(year),
         yr_rev = fct_rev(yr_rev),
         flat_date = as.Date("2000-01-01") + yday - 1)


# annual points as dates
hw_annual <- region_hw %>% 
  group_by(yr) %>% 
  summarise(sst_anom = mean(sst_anom),
            anom_f = mean(anom_f),
            .groups = "drop") %>% 
  mutate(yr_as_dt = as.Date(str_c(yr, "-06-15")))



# Plot the hw days below as hash marks:
ggplot() +
  geom_line(data = region_hw, aes(time, anom_f), color = "gray60", alpha = 0.5) +
  geom_line(data = hw_annual, aes(yr_as_dt, anom_f), size = 1) +
  geom_segment(data = filter(region_hw, mhw_event),
              aes(x = time, xend = time,
                  y = -4.6, yend = -4.3), 
              size = .2, color = "darkred") +
  scale_y_continuous(labels = scales::number_format(suffix = " \u00b0F")) +
  theme(plot.caption = element_text(hjust = 0)) + # set the left align here
  labs(x = "Date", 
       y = "Sea Surface Temperature Anomaly",
       title = "Gulf of Maine Sea Surface Temperature Anomalies",
       caption = "Data Source: NOAA OISSTv2 Daily Sea Surface Temperature Data. Temperature anomalies & MHW status\ndetermined following the methods of Hobday et al. 2016 using a 30-year climatology reference period of 1982-2011.")




# # Just change the line color
# ggplot() +
#   geom_line(data = region_hw, aes(time, sst_anom, color = mhw_event, group = 1), alpha = 0.5) +
#   geom_line(data = hw_annual, aes(yr_as_dt, sst_anom), size = 1) +
#   scale_color_manual(values = c("TRUE" = "darkred", "FALSE" = "gray60"))  +
#   labs(x = "Date", y = "SST Anom")

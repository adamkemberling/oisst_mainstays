# North Atlantic Ocean Warming.

# Documenting code and processing for a data request form Boston Globe


# Libraries
library(gmRi)
library(sf)
library(raster)
library(tidyverse)
library(heatwaveR)


# Data

# # Load the grid using N. Atlantic Dimensions
# # Load data - Temperatures
# na_window <- data.frame(
#   lon = c(-80,0),
#   lat = c(0,60),
#   time = as.Date(c("1982-01-01", "2023-06-11")))
# 
# na_grid <- oisst_window_load(
#   data_window = na_window, 
#   anomalies = F, 
#   climate_ref = "1982-2011",
#   box_location = "cloudstorage") %>% 
#   stack()
# 
# 
# 
# # Plot to see it
# na_grid[[1]][[1]] %>% plot()


# Too big, back to python
# Load the timeseries created in python:
na_ts <- read_csv(here::here("notebooks/nb_testing_data/north_atlantic_sst_1982to2023.csv")) %>% 
  rename(date = time, sst = area_wtd_sst)




# Anomalies using heatwaveR
source(here::here("R/oisst_support_funs.R"), verbose = FALSE)
#source(here("R/temp_report_support.R"), verbose = FALSE)

atl_hw <- pull_heatwave_events(
  temperature_timeseries = na_ts, 
  date_col = "date", 
  temp_col = "sst",
  threshold = 90, 
  clim_ref_period = c("1982-01-01", "2011-12-31"))


# Renaming and trimming
# SST, Climatology, Anomaly, 10th percentile, 90th Percentile
sst_out <- atl_hw %>% #glimpse()
  select(date = time,
         sst = sst,
         sst_anomaly = sst_anom,
         clim_avg_1982to2011 = seas,
         clim_10th_percentile = mcs_thresh,
         clim_90th_percentile = mhw_thresh) %>% 
  mutate(date = as.Date(date, format = "yyyy-mm-dd")) %>% 
  mutate(year = lubridate::year(date), .before = "sst") %>% 
  mutate(is_prelim = ifelse(Sys.Date() - date <= 14, T, F), .before = "date")

# Save it Locally
write_csv(sst_out, here::here("local_data/GMRI_OISSTv2_North_Atlantic_2023_06_10.csv"))



# Mirror the graph from climate reanalyzer 
library(lubridate)
library(scales)
deg_sym <- "\u00b0" 
deg_c <- "\u00b0C"
deg_f <- "\u00b0F"

sst_plot <- sst_out %>% 
  mutate(flat_date = as.Date(str_c("2000", month(date), day(date), sep = "-")),
         is_23 = ifelse(year == 2023, "darkred", ifelse(year == 2022, "orange", "gray70")),
         is_23_line = ifelse(year %in% c(2022:2023), 1, 0.35),
         prelim_fade = ifelse(is_prelim, 90, 10)) 



# Compare 2022-2023 against all other years



# Simple Key
sst_plot %>% 
  ggplot(aes(flat_date, sst)) +
  theme_gmri() + 
  theme(axis.line.y = element_line(color = "black"),
        legend.position = c(0.85, 0.125)) +
  geom_line(aes(group = year), color = "gray75", alpha = 0.65, linewidth = 0.35) +
  geom_line(data = filter(sst_plot, year %in% c(2022, 2023)),
            aes(group = year, color = factor(year)), linewidth = 1) +
  geom_line(aes(y = clim_avg_1982to2011), color = "black", 
            linetype = 2, linewidth = 1) +
  geom_line(aes(y = clim_10th_percentile), color = "gray50", 
            linetype = 3, linewidth = 1) + 
  geom_line(aes(y = clim_90th_percentile), color = "gray50", 
            linetype = 3, linewidth = 1) +
  scale_x_date(date_breaks = "1 month", labels = scales::date_format("%b"), expand = expansion(add = c(0,0))) +
  scale_y_continuous(labels = number_format(suffix = deg_c)) +
  scale_color_manual(values = c("darkred", "orange")) +
  guides(color = guide_legend(nrow = 2)) +
  labs(y = "Sea Surface Temperature", x = "Date", title = "North Atlantic SST Sets Alarming New Records in 2023",
       caption = "Data and Visuals by the Gulf of Maine Research Institute\nSource Data: NOAA OISSTv2",
       color = "Year")


# More complicated Key work:
sst_plot %>% 
  ggplot(aes(flat_date, sst)) +
  theme_gmri() + 
  theme(axis.line.y = element_line(color = "black"),
        #legend.position = c(0.85, 0.125)
        legend.position = "top"
        ) +
  geom_line(aes(group = year), color = "gray75", alpha = 0.65, linewidth = 0.35) +
  geom_line(data = filter(sst_plot, year %in% c(2022, 2023)),
            aes(group = year, color = factor(year)), linewidth = 1) +
  geom_line(aes(y = clim_avg_1982to2011, color = "30-Year Average\n   (1982-2011)"), #color = "black", 
            linetype = 2, linewidth = 1) +
  geom_line(aes(y = clim_10th_percentile, color = "10th Percentile\n (1982-2011)"), #color = "gray50", 
            linetype = 3, linewidth = 1) + 
  geom_line(aes(y = clim_90th_percentile, color = "90th Percentile\n (1982-2011)"), #color = "gray50", 
            linetype = 3, linewidth = 1) +
  scale_x_date(date_breaks = "1 month", labels = scales::date_format("%b"), expand = expansion(add = c(0,0))) +
  scale_y_continuous(labels = number_format(suffix = deg_c)) +
  scale_color_manual(values = c("gray50", "gray50", "black", "darkred", "orange")) +
  guides(color = guide_legend(nrow = 2)) +
  labs(y = "Sea Surface Temperature", x = "Date", title = "North Atlantic SST Sets Alarming New Records in 2023",
       caption = "Data and Visuals by the Gulf of Maine Research Institute\nSource Data: NOAA OISSTv2",
       color = "Color")


# Comparing them as Anomalies
sst_plot %>% 
  ggplot(aes(flat_date, sst_anomaly)) +
  theme_gmri() + 
  theme(axis.line.y = element_line(color = "black"),
        legend.position = c(0.9, 0.95)) +
  geom_line(aes(group = year), color = "gray75", alpha = 0.65, linewidth = 0.35) +
  geom_line(data = filter(sst_plot, year %in% c(2022, 2023)),
            aes(group = year, color = factor(year)), linewidth = 1) +
  geom_line(data = filter(sst_plot, year %in% c(2023), is_prelim),
            aes(group = year), linewidth = 0.5, linetype = 3) +
  # geom_line(aes(y = clim_avg_1982to2011), color = "black", 
  #           linetype = 2, linewidth = 1) +
  # geom_line(aes(y = clim_10th_percentile), color = "gray50", 
  #           linetype = 3, linewidth = 1) + 
  # geom_line(aes(y = clim_90th_percentile), color = "gray50", 
  #           linetype = 3, linewidth = 1) +
  scale_x_date(date_breaks = "1 month", labels = scales::date_format("%b"), expand = expansion(add = c(0,0))) +
  scale_y_continuous(labels = number_format(suffix = deg_c)) +
  scale_color_manual(values = c("darkred", "orange")) +
  guides(color = guide_legend(nrow = 1)) +
  labs(y = "Sea Surface Temperature Anomaly", x = "Date", title = "North Atlantic SST Sets Alarming New Records in 2023",
       caption = "Data and Visuals by the Gulf of Maine Research Institute\nSource Data: NOAA OISSTv2\n Data under review shown with dotted line denotes ",
       color = "Year")



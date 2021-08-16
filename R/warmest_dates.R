####  Warmest Dates for SST
# For Christopher Nunley, Charter Communications


####  Load Packages  ####
library(lubridate)
library(ncdf4)
library(raster)
library(rnaturalearth)
library(sf)
library(stars)
library(gmRi)
library(here)
library(janitor)
library(knitr)
library(patchwork)
library(tidyverse)
library(heatwaveR)
library(ggpmisc)
library(gt)

# Support Functions
source(here("R/oisst_support_funs.R"))
source(here("R/temp_report_support.R"))

#box paths
box_paths <- research_access_paths()


# File Paths
mills_path <- box_paths$mills
res_path   <- str_replace(box_paths$res, "RES Data", "RES_Data")
okn_path   <- box_paths$okn  
oisst_path <- paste0(res_path, "OISST/oisst_mainstays/")


# Set ggplot theme for figures
theme_set(theme_bw())


# Set theme up for maps
map_theme <- list(
  theme(
    panel.border       = element_rect(color = "black", fill = NA),
    plot.background    = element_rect(color = "transparent", fill = "transparent"),
    line               = element_blank(),
    axis.title.x       = element_blank(), # turn off titles
    axis.title.y       = element_blank(),
    legend.position    = "bottom", 
    legend.title.align = 0.5))



#### Find Warmest Days

# Daily Records
global_sst <- read_csv(str_c(oisst_path, "global_timeseries/global_anoms_1982to2011.csv"), guess_max = 1e6)
gom_sst <- gmRi::oisst_access_timeseries("gmri focus areas", "apershing gulf of maine")
gom_sst <- gom_sst %>% mutate(time = as.Date(time))

# Monthly Records

# global
global_monthly <- global_sst %>% 
  mutate(year = lubridate::year(time), 
         month = lubridate::month(time)) %>% 
  group_by(year, month) %>% 
  summarise(sst = mean(sst),
            area_wtd_sst = mean(area_wtd_sst), 
            sst_clim = mean(sst_clim), 
            sst_anom = mean(sst_anom),
            .groups = "drop") %>% 
  mutate(day = "15", 
         date = as.Date(str_c(year, month, day, sep = "-")),
         sst_f = as_fahrenheit(sst),
         area_wtd_f = as_fahrenheit(area_wtd_sst))

# gom
gom_monthly <- gom_sst %>% 
  mutate(year = lubridate::year(time), 
         month = lubridate::month(time)) %>% 
  group_by(year, month) %>% 
  summarise(sst = mean(sst), 
            area_wtd_sst = mean(area_wtd_sst), 
            sst_clim = mean(sst_clim), 
            sst_anom = mean(sst_anom),
            .groups = "drop") %>% 
  mutate(day = "15", 
         date = as.Date(str_c(year, month, day, sep = "-")),
         sst_f = as_fahrenheit(sst),
         area_wtd_f = as_fahrenheit(area_wtd_sst))







####  Hottest Day  ####
global_sst %>% arrange(desc(area_wtd_sst)) %>% head()
gom_sst %>% arrange(desc(area_wtd_sst)) %>% head()



####  Hottest Month  ####
global_monthly %>% arrange(desc(area_wtd_sst)) %>% head()
gom_monthly %>% arrange(desc(area_wtd_sst)) %>% head()


# Plots
global_monthly %>% ggplot(aes(date, area_wtd_sst)) +
  geom_line() +
  labs(x = "Date", y = "Area Weighted SST", subtitle = "Global Temperature")

gom_monthly %>% ggplot(aes(date, area_wtd_sst)) +
  geom_line() +
  labs(x = "Date", y = "Area Weighted SST", subtitle = "Gulf of Maine Temperature")



####  Hottest June  ####
gom_monthly %>% 
  filter(month == 6) %>% 
  arrange(desc(sst_anom))



####  Warming Rates  ####

# Global annual averages
global_avg <- global_sst %>% filter(
  between(time, as.POSIXct("1982-01-01 00:00:00"), as.POSIXct("2020-12-31 00:00:00"))) %>% 
  mutate(year = lubridate::year(time)) %>% 
  group_by(year) %>% 
  summarise(sst = mean(sst),
            sst_anom = mean(sst),
            sst_clim = mean(sst_clim),
            area_wtd_sst = mean(area_wtd_sst),
            area_wtd_clim = mean(area_wtd_clim),
            area_wtd_anom = mean(area_wtd_anom),
            .groups = "drop")


# gom annual averages
gom_avg <- gom_sst %>% filter(
  between(time, as.Date("1982-01-01"), as.Date("2020-12-31"))) %>% 
  mutate(year = lubridate::year(time)) %>% 
  group_by(year) %>% 
  summarise(sst = mean(sst),
            sst_anom = mean(sst),
            sst_clim = mean(sst_clim),
            area_wtd_sst = mean(area_wtd_sst),
            area_wtd_clim = mean(area_wtd_clim),
            area_wtd_anom = mean(area_wtd_anom),
            .groups = "drop")

# annual warming rates
lm(area_wtd_sst ~ year, data = global_avg) # global warming rate
lm(area_wtd_sst ~ year, data = gom_avg)    # Gulf of Maine Rate

####Education Team Figures - for Robin Lea  ####


# Packages
library(heatwaveR)
library(gmRi)
library(tidyverse)
library(lubridate)
library(conflicted)
library(geomtextpath)
library(scales)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# functions
suppressPackageStartupMessages(source(here::here("R/oisst_support_funs.R")))
suppressPackageStartupMessages(source(here::here("R/temp_report_support.R")))


# Load data for Gulf of Maine & Gulf of Maine EPU
andy_gom <- oisst_access_timeseries(
    "gmri", 
    poly_name = "apershing gulf of maine", 
    box_location = "cloudstorage") %>% 
  mutate(tod = format(time, format = "%H:%M:%S")) 


# Put in list to mirror steps
gom_list <- list("GMRI - Gulf of Maine" = andy_gom)


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
    summarise(sst = mean(sst),
              sst_anom = mean(sst_anom),
              anom_f = mean(anom_f),
              .groups = "drop") %>% 
    mutate(yr_as_dt = as.Date(str_c(yr, "-06-15")))
})

####______#####
####  Figures  ####

# Figure 1. Annual Averages
# Get regional averages
temp_regimes <- hw_annual$`GMRI - Gulf of Maine` %>% 
  filter(yr > 1981) %>% 
  mutate(
    regime = ifelse(yr < 2010, "1982-2009 Average", "2010-2021 Average")) %>% 
  group_by(regime) %>% 
  mutate(
    avg_temp_c = round(mean(sst), 2),
    temp_label = str_c(avg_temp_c, "\u00b0C"),
    avg_anom_c = mean(sst_anom, na.rm = T),
    #avg_anom_c = ifelse(regime == "1982-2009 Regime", 0, avg_anom_c),
    x = ifelse(regime == "1982-2009 Average", 1981.5, 2009.5),
    xend = ifelse(regime == "1982-2009 Average", 2009.5, 2021.5),) %>% 
  ungroup()


# Plot the temperature regimes:
ggplot(temp_regimes, aes(yr, sst_anom)) +
  geom_col(aes(fill = regime),  size = 0.75, alpha = 0.4) +
  scale_fill_gmri() +
  # Using geomtextpath for label
  geom_textpath(
    aes(x = yr, 
        y = avg_anom_c, 
        label = temp_label, 
        linecolor = regime,
        colour = regime), 
    size = 4, vjust = -2, fontface = "bold", show.legend = FALSE) +
  # Line segments for the regime averages:
  geom_segment(
    aes(x = x, 
        xend = xend,
        y = avg_anom_c, 
        yend = avg_anom_c,
        color = regime),
    #key_glyph = "rect",
    size = .8,
    linetype = 1) +
  scale_x_continuous(expand = expansion(add = c(0.25,0.25))) +
  scale_y_continuous(labels = number_format(suffix = " \u00b0C")) +
  scale_color_gmri() +
  guides(fill = "none") +
  theme(
    legend.title = element_blank(),
    #legend.position = "bottom",
    legend.position = c(0.2, 0.85),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent", color = "transparent")) + 
  labs(
    title = "Gulf of Maine Temperature Regime Shift",
    x = "Year", 
    y = "Surface Temperature Anomaly",
    caption = "Anomalies calculated using 1982-2011 reference period.") +
  guides(label = "none",
         color = guide_legend(keyheight = unit(0.5, "cm"), ))







####______####

####  Figure 2. Annual Timeline  ####
ggplot(temp_regimes, aes(yr, sst)) +
  geom_point(size = 2, color = gmri_cols("gmri blue")) +
  geom_line(group = 1, linetype = 2 , color = gmri_cols("gmri blue")) +
  scale_y_continuous(labels = number_format(suffix = " \u00b0C")) +
  scale_color_gmri() +
  guides(fill = "none") +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent", color = "transparent")) + 
  labs(
    title = "Gulf of Maine Average Annual Temperature",
    x = "Year", 
    y = "Sea Surface Temperature",
    caption = "Data Source: NOAA OISSTv2 Daily Sea Surface Temperature Product") +
  guides(label = "none",
         color = guide_legend(keyheight = unit(0.5, "cm")))


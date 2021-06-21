# Comparing Current Year to 2012

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


# OISST Data
gom_oisst <- oisst_access_timeseries(oisst_path = box_paths$oisst_mainstays, 
                                     region_family = "gmri focus areas", 
                                     poly_name = "apershing gulf of maine")


# Plot 2012 and 2021
gom_oisst <- gom_oisst %>% 
  mutate(time = as.Date(time),
         yr = str_sub(time, 1, 4)) %>% 
  select(time, yr, sst, sst_anom) 


# Pull Heatwaves
gom_hw <- pull_heatwave_events(temperature_timeseries = gom_oisst, 
                               threshold = 90, 
                               clim_ref_period = c("1982-02-01", "2011-12-31"))


# Pull pertinent info
base_date <- as.Date("2000-01-01")
gom_21 <- gom_hw %>% 
  mutate(year = lubridate::year(time),
         yday = lubridate::yday(time),
         flat_date = as.Date(yday-1, origin = base_date),
         year = factor(year),
         month = lubridate::month(time)) %>% 
  filter(year == "2012" | year ==  "2021",
         month < 7)


clim <- filter(gom_21, year == "2012")

# Plot comparison
ggplot() +
  geom_line(data = gom_21, 
            aes(flat_date, sst, color = year, group = year, alpha = mhw_event)) +
  geom_line(data = clim, aes(flat_date, seas, color = "Climatology")) +
  geom_line(data = clim, aes(flat_date, mhw_thresh, color = "Heatwave Threshold"), linetype = 3) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.5)) +
  scale_color_manual(values = c("2012" = "darkred",
                                "2021" = "royalblue",
                                "Climatology" = "gray50",
                                "Heatwave Threshold" = "gray50")) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0,0)) +
  labs(color = "", 
       alpha = "Marine Heatwave",
       x = "Date", 
       y = expression("Sea Surface Temperature"~degree~C)) 


# Degrees above normal
gom_21 %>% 
  filter(year == "2012") %>% 
  ggplot(aes(flat_date, sst_anom)) +
  geom_line(aes(color = "2012")) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray50") +
  scale_color_gmri() +
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0,0)) +
  labs(color = "", 
       x = "Date", 
       y = expression("Sea Surface Temperature Anomaly"~degree~C)) +
  theme(legend.position = "bottom")

# Difference in Temperature
gom_21 %>% 
  select(flat_date, year, sst) %>% 
  pivot_wider(names_from = year, values_from = sst, names_prefix = "sst_") %>% 
  mutate(temp_diff = sst_2012 - sst_2021) %>% 
  drop_na(temp_diff) %>% 
  ggplot(aes(flat_date, temp_diff)) +
  geom_line(aes(color = "Temperature Difference 2012 - 2021")) +
  scale_color_gmri() +
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0,0)) +
  labs(color = "", 
       x = "Date", 
       y = expression("Sea Surface Temperature Difference"~degree~C)) +
  theme(legend.position = "bottom")


####  Cumulative Totals  ####

# Cumulative heatwave days
gom_21 <- gom_21 %>% 
  group_by(year) %>% 
  mutate(cum_hw_days = cumsum(mhw_event),
         yday = lubridate::yday(time),
         excess_degrees = cumsum(sst_anom)) %>% 
  ungroup()


hw_days <- gom_21 %>% 
  ggplot(aes(flat_date, cum_hw_days)) +
  geom_line(aes(color = year)) +
  geom_line(aes(flat_date, yday, color = "All Days Possible"), linetype = 3, size = 0.5) +
  scale_color_manual(values = c("2021" = as.character(gmri_cols("orange")),
                                "2012" = as.character(gmri_cols("gmri blue")),
                                "All Days Possible" = "gray40")) +
  labs(y = "Cumulative HW Days",
       x = "",
       color = "")


# Cumulative degrees above climatology
excess_temp <- gom_21 %>% 
  ggplot(aes(flat_date, excess_degrees)) +
  geom_line(aes(color = year)) +
  scale_color_manual(values = c("2021" = as.character(gmri_cols("orange")),
                                "2012" = as.character(gmri_cols("gmri blue")))) +
  labs(y = "Excess Temperature Above 'Norm'",
       x = "Date",
       color = "")


hw_days / excess_temp


####  Percentages  ####

# donut plot?
library(ggforce)
library(magrittr)

# maximum possible days
max_days <- filter(gom_21, year == 2021) %$% max(yday) 
max_2021 <- filter(gom_21, year == 2021) %$% max(cum_hw_days) 
rem_2021 <- max_days - max_2021
max_2012 <- filter(gom_21, year == 2012, yday <= max_days) %$% max(cum_hw_days) 
rem_2012 <- max_days - max_2012

# Build a dataframe for a donut plot
gom_percentages <- data.frame(
  "year" = c("2012", "2012", "2021", "2021"),
  "status" = rep(c("Heatwave", "Not Heatwave"), 2),
  "Total" = c(max_2012, rem_2012, max_2021, rem_2021),
  "focus" = rep(c(0, 0.2), 2)
)


# Plot the Doughnuts
gom_donuts <- gom_percentages %>% 
  ggplot() +
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1, 
                     amount = Total, 
                     fill = status, 
                     explode = focus),
                 #alpha = 0.6, 
                 stat = "pie") +
  #geom_mark_ellipse(aes(filter = status == "Not Heatwave")) +
  facet_wrap(~year) +
  theme_no_axes() +
  scale_fill_gmri(reverse = T) +
  labs(fill = "Surface Ocean State", subtitle = "Relative Amount of  Marine Heatwave Days to 'Normal Days'")


(hw_days / excess_temp) / gom_donuts

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
library(ggforce)
library(magrittr)

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
         month < 12)

# Pull out a single year to plot the climatology just once
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


# Monthly Summary
month_summs <- gom_21 %>% 
  group_by(year, month) %>% 
  summarise(
    avg_temp = mean(sst),
    avg_anom = mean(sst_anom),
    peak_anom = max(sst_anom),
    smallest_anom = min(sst_anom),
    n_hw_days = sum(mhw_event),
    deg_over = sum(sst_anom)
  )



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
         excess_degrees = cumsum(sst_anom),
         flat_date = as.Date(flat_date),
         hw_point_flag = ifelse(mhw_event == TRUE, 4, NA)) %>% 
  ungroup()



# Plot cumulative hw days
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

# # stack and plot
# hw_days / excess_temp


####  Percentages  ####

# donut plot?

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


# Stack and plot
(hw_days / excess_temp) / gom_donuts




####  Polar Plots  ####

#dataframe to place axis labels
label_df <- data.frame(flat_date = rep(as.Date("2000-01-01"), 6),
                       sst_anom  = seq(0,5, by = 1))
label_df <- bind_rows(mutate(label_df, year = "2012"),
                      mutate(label_df, year = "2021"))

# Plot on a circle
gom_21 %>% 
  ggplot() + 
  geom_segment(aes(x = flat_date, xend = flat_date, y = 0, yend = sst_anom, color = sst_anom)) +
  geom_label(data = label_df, aes(flat_date, sst_anom, label = sst_anom), size = 2) +
  coord_polar() +
  scale_color_distiller(palette = "OrRd", direction = 1) +
  facet_wrap(~year) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 6, by = 1)) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = "transparent"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        panel.grid.major.y = element_line(linetype = 1),
        legend.position = "bottom", strip.background = element_rect(color = "black"),
        plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_colorbar(title = "Temperature Anomaly from 1982-2011 Climate",
                                title.position = "top", 
                                title.hjust = 0.5,
                                barwidth = unit(4, "inches"), 
                                frame.colour = "black", 
                                ticks.colour = "black")) +
  labs(x = "", y = "", title = "Difference In Anomaly Patterns Between 2012 & 2021")


# Is it better not as a circle?
gom_21 %>% 
  ggplot() + 
  geom_segment(aes(x = flat_date, xend = flat_date, 
                   y = 0, yend = sst_anom, color = sst_anom),
               size = 1) +
  #geom_point(aes(flat_date, hw_point_flag, shape = "Heatwave Event"), size = 0.1) +
  scale_color_distiller(palette = "OrRd", direction = 1) +
  facet_wrap(~year, nrow = 2) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 6, by = 1)) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = "transparent"),
        panel.grid.major.y = element_line(linetype = 1),
        legend.position = "bottom", strip.background = element_rect(color = "black"),
        plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_colorbar(title = expression("Temperature Anomaly "~degree~C),
                                title.position = "top", 
                                title.hjust = 0.5,
                                barwidth = unit(3, "inches"), 
                                frame.colour = "black", 
                                ticks.colour = "black"),
         shape = guide_legend(title = "", 
                              label.theme = element_text(size = 11),
                              override.aes = list(size = 1), 
                              label.position = "top")) +
  labs(x = "", 
       y = expression("Temperature Anomaly "~degree~C), 
       title = "Difference In Anomaly Patterns Between 2012 & 2021",
       caption = "(Temperature Anomalies from 1982-2011 Climatology for the Gulf of Maine)")





# Yes, but columns may be better
gom_21 %>% 
  ggplot() + 
  geom_col(aes(x = flat_date, y = sst_anom, fill = sst_anom), width = 1) +
  scale_fill_distiller(palette = "OrRd", direction = 1) +
  facet_wrap(~year, nrow = 2) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 6, by = 1)) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = "transparent"),
        panel.grid.major.y = element_line(linetype = 1),
        legend.position = "bottom", strip.background = element_rect(color = "black"),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_colorbar(title = expression("Temperature Anomaly "~degree~C),
                               title.position = "top", 
                               title.hjust = 0.5,
                               barwidth = unit(4, "inches"), 
                               frame.colour = "black", 
                               ticks.colour = "black")) +
  labs(x = "", 
       y = expression("Temperature Anomaly "~degree~C), 
       title = "Contrasting Temperature Anomaly Events of 2012 & 2021",
       caption = "(Temperature Anomalies from 1982-2011 Climatology for the Gulf of Maine)")

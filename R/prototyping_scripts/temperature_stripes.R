# Show your stripes, Gulf of Maine


####  Packages  ####
library(lubridate)
library(scales)
library(gmRi)
library(tidyverse)


#box paths
oisst_path <- cs_path("res", "OISST/oisst_mainstays")

# What area is the report using?
group_name <- "gmri_sst_focal_areas"
region_name <- "apershing_gulf_of_maine"

# File paths for various extents based on "apershing_gulf_of_maine"
region_paths <- get_timeseries_paths(region_group = group_name, 
                                     mac_os = "mojave")

# Timeseries Path
timeseries_path <- region_paths[[region_name]][["timeseries_path"]]

# Adding Logo to plots
logo_path <- paste0(system.file("stylesheets", package = "gmRi"), "/gmri_logo.png")
lab_logo <- magick::image_read(logo_path)






####  Data Prep  ####

# Load the timeseries
region_timeseries <- read_csv(timeseries_path, 
                              col_types = cols(), 
                              guess_max = 1e6) %>% 
  filter(year(time) %in% c(1982:2021))



# Make Yearly Averages
yearly_temps <- region_timeseries %>% 
  group_by(year = year(time)) %>% 
  summarise(sst = mean(area_wtd_sst),
            anom = mean(area_wtd_anom))


# Make Monthly Averages
monthly_temps <- region_timeseries %>% 
  group_by(year = year(time),
           month = month(time)) %>% 
  summarise(sst = mean(area_wtd_sst),
            anom = mean(area_wtd_anom),
            .groups = "drop") %>% 
  mutate(time = as.Date(str_c(year, str_pad(month, width = 2, pad = "0", side = "left"), "01", sep = "-")))


####  Plot  ####
ggplot(yearly_temps, aes(x = year, y = 0)) +
  geom_tile(aes(fill = anom)) +
  scale_fill_distiller(palette = "RdBu") +
  theme_void()





# Plot monthly timeline
ggplot(monthly_temps, aes(x = time, y = 0)) +
  geom_tile(aes(fill = anom)) +
  scale_fill_distiller(palette = "RdBu", limits = c(-1.5,1.5), oob = oob_squish) +
  scale_y_continuous(expand = expansion(add = c(0,0.01))) +
  scale_x_date(expand = expansion(add = c(45,45))) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.125),
        panel.border = element_rect(color = "black", size = 1, fill = "transparent"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        plot.caption = element_text(size = 7.2, margin = margin(t = 20), color = "gray40")
        ) +
  labs(title = "Gulf of Maine - Sea Surface Temperature Anomalies",
       x = "",
       caption = "Monthly Averages from 1982-2021  |  Climatology Period 1982-2011"
       #subtitle = "Sea Surface Temperature Anomalies"
       )

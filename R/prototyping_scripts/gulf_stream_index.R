####
# Gulf Stream Index
# Relating GSI to Gulf of Maine Temperatures
# Motivation:
"
One major factor influencing the ocean temperatures along the East coast, 
and the Gulf of Maine in particular is the position of the Gulf Stream. 
The Gulf Stream transports large volumes of hot and salty water from the 
Gulf of Mexico Northward following the coast before heading East across 
the North Atlantic. Its relative position to the Gulf of Maine, 
Georges Bank, and the Scotian Shelf impacts the amount of warm water 
that advects into the region, as well as the amount of cold water that 
enters from the North via the Labrador current.
"

####

####  Libraries  ####
library(gmRi)
library(tidyverse)




####  EDA  ###

# Load GOM Data
region_paths <- get_timeseries_paths(region_group = "gmri_sst_focal_areas")
timeseries_path <- region_paths[["apershing_gulf_of_maine"]][["timeseries_path"]]
region_timeseries <- read_csv(timeseries_path, col_types = cols(), guess_max = 1e6) %>% 
  mutate(time = as.Date(time))
region_heatwaves <- pull_heatwave_events(region_timeseries, threshold = 90) 

# Load GSI Data
gsi <- readxl::read_xlsx(str_c(res_path, "Gulf_Stream_Indices/GSI_21points_CMEMS_1_degree.xlsx"))


# Clean up
gsi <- gsi %>% 
  mutate(month = str_pad(Month, width = 7, side = "right", pad = "0"),
         year = str_sub(Month, 1, 4),
         month = str_sub(Month, 6, -1)) %>% 
  select(-Month)

# Merge
regional_gsi <- region_heatwaves %>% 
  group_by(year(time), month(time)) %>% 
  summarise(sst = mean(sst),
            sst_anom = mean(sst_anom),
            .groups = "drop") %>%
  rename(year = `year(time)`, month = `month(time)`) %>% 
  mutate(year = as.character(year),
         month = str_pad(month, width = 2, side = "left", pad = "0"),
         month_date = as.Date(str_c(year, month, "15", sep = "/"))) %>%  
  left_join(gsi, by = c("year", "month"))

# Correlation
regional_gsi %>% 
  ggplot(aes(GSI, sst_anom)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", color = gmri_cols("gmri blue")) +
  stat_poly_eq(formula = y ~ x,
               color = gmri_cols("gmri blue"),
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = T)

# Plot overlaid patterns
regional_gsi %>% 
  mutate(anom_direction = ifelse(sst_anom < 0, "Negative Anomaly", "Positive Anomaly")) %>% 
  ggplot(aes(month_date, sst_anom)) +
  geom_col(aes(fill = anom_direction), width = 30 , color = "transparent", alpha = 0.5) +
  scale_fill_gmri() +
  # geom_line(aes(month_date, GSI, color = "Gulf Stream Index"), size = 0.5) +
  # scale_color_manual(values = c("Gulf Stream Index" = "gray10")) +
  labs(x = "", y = expression("Sea Surface Temperature Anomaly"~~degree~C), fill = "Temperature Anomaly") +
  theme(legend.position = "bottom")




# Comparing different GOM and NE Shelf timeseries
# Also a check for the timeseries functions



# Libraries
library(heatwaveR)
library(gmRi)
library(tidyverse)
library(lubridate)
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


# Functions
suppressPackageStartupMessages(source(here::here("R/oisst_support_funs.R")))
suppressPackageStartupMessages(source(here::here("R/temp_report_support.R")))


####  Load Timeseries



#### A. Gulf of Maine Comparisons  ####

# Gulf of Maine
andy_gom <- oisst_access_timeseries("gmri", poly_name = "apershing gulf of maine", box_location = "cloudstorage")
epu_gom <- oisst_access_timeseries(region_family = "epu", poly_name = "gom", box_location = "cloudstorage")

# Put together
goms <- bind_rows(list("Gulf of Maine Rectangle" = andy_gom,
                       "Gulf of Maine EPU" = epu_gom),
                  .id = "MaskShapefile")


# Plot
goms %>% 
  group_by(year = lubridate::year(time), MaskShapefile) %>% 
  summarise(anom = mean(area_wtd_anom, na.rm = T)) %>% 
  ggplot(aes(year, anom, color = MaskShapefile)) +
  geom_line()





####  B. Northeast US Shelf Comparisons  ####

# Ne Shelf
lme_shelf   <- oisst_access_timeseries(region_family = "lme", poly_name = "northeast us continental shelf", box_location = "cloudstorage")
nelme_shelf <- oisst_access_timeseries(region_family = "nelme regions", poly_name = "nelme", box_location = "cloudstorage")
trawl_shelf <- oisst_access_timeseries(region_family = "trawl", poly_name = "regions collection", box_location = "cloudstorage")

# Put together
shelfs <- bind_rows(list("Northeast Shelf - Large Marine Ecosystem" = lme_shelf,
                         "Northeast Shelf - Northeast Ecoregions" = nelme_shelf,
                         "Northeast Shelf - Northeast Shelf Trawl Area" = trawl_shelf),
                    .id = "MaskShapefile")


# Plot
shelfs %>% 
  group_by(year = lubridate::year(time), MaskShapefile) %>% 
  summarise(anom = mean(area_wtd_anom, na.rm = T)) %>% 
  ggplot(aes(year, anom, color = MaskShapefile)) +
  geom_line()





####  Large Marine Ecosystem Figure  ####


# NE Shelf Annual Summary
shelf_summary <- lme_shelf %>% 
  group_by(year = year(time)) %>% 
  temperature_summaries() %>% 
  mutate(yr_as_dtime = as.Date(paste0(year, "-07-02")),
         anom_direction = ifelse(area_wtd_anom > 0, "Hot", "Cold"))




# # Global Temperature Anomaly Rates
oisst_path <- cs_path("res", "OISST/oisst_mainstays")
global_anoms <- read_csv(
  paste0(oisst_path, "global_timeseries/global_anoms_1982to2011.csv"), 
  guess_max = 1e6,
  col_types = cols()) %>% 
  mutate(year = year(time)) %>% 
  filter(between(year, 1982, 2021))

# Pull the season, summarize by year
global_summary <- global_anoms %>% 
  group_by(year) %>% 
  temperature_summaries() %>% 
  mutate(yr_as_dtime = as.Date(paste0(year, "-07-02")),
         anom_direction = ifelse(area_wtd_anom > 0, "Hot", "Cold"))




# Get the global temperature warming rates
rate_data <- list(
  "NE Shelf C" = get_decadal_rates(temp_df = shelf_summary, 
                                   temp_col = "area_wtd_sst", 
                                   year_col = "year", 
                                   year_lim = c(1982, 2021), 
                                   area_name = "GoM", 
                                   degree_c = T),
  "Global C" = get_decadal_rates(temp_df = global_summary, 
                                 temp_col = "area_wtd_sst", 
                                 year_col = "year", 
                                 year_lim = c(1982, 2021), 
                                 area_name = "Global", 
                                 degree_c = T))







####  Make the Plot:  ####
library(scales)

# Plot the rate comparison:
global_rate_comparison(annual_summary_dat = shelf_summary, 
                       global_summary_dat = global_summary, 
                       eq_all = rate_data$`NE Shelf C`$eq_label, 
                       eq_global = rate_data$`Global C`$eq_label, 
                       temp_units = "C", 
                       region_label = "Northeast Shelf LME")



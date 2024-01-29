# Loading SST for areas that EDU needs for lobster lesson:


# Libraries
library(gmRi)
library(raster)


# Load SST for Providence, Mass Bay, and Maine


providence_sst <- oisst_window_load(
  data_window = data.frame(
    lon = c(-71.75, -71.0),
    lat = c(41.2, 41.6),
    time = as.Date(c("1982-01-01", "2023-12-31"))), 
  anomalies = F, 
  climate_ref = "1991-2020", 
  box_location = "cloudstorage")


# mass_sst <- oisst_window_load(
#   data_window = data.frame(
#     lon = c(-71.75, -71.0),
#     lat = c(41.25, 12.5),
#     time = as.Date(c("1982-01-01", "2023-12-31"))), 
#   anomalies = F, 
#   climate_ref = "1991-2020", 
#   box_location = "cloudstorage")
# 
# maine_sst <- oisst_window_load(
#   data_window = data.frame(
#     lon = c(-71.75, -71.0),
#     lat = c(41.25, 12.5),
#     time = as.Date(c("1982-01-01", "2023-12-31"))), 
#   anomalies = F, 
#   climate_ref = "1991-2020", 
#   box_location = "cloudstorage")

# Make them timeseries and add fahrenheit
library(tidyverse)
sst_ts <- providence_sst %>% 
  stack() %>% 
  cellStats(mean, na.rm = T) %>% 
  as.data.frame()

# minor adjustments
sst_providence_ts <- sst_ts %>% 
  rownames_to_column() %>% 
  setNames(c("time", "sst_c")) %>% 
  mutate(
    time = as.Date(str_sub(str_replace_all(time, "[.]", "-"),2,-1)),
    sst_f = as_fahrenheit(sst_c)
  )

# Plots
plot(providence_sst$`1982`$X1982.01.02, main = "1982-01-01 SST")
ggplot(sst_providence_ts, aes(time, sst_c)) + geom_line() + geom_hline(yintercept = 20, color = "darkred")



# Save locally for Eliza
write_csv(sst_providence_ts, here::here("data/unimportant_oneoffs/providence_sst_oisst.csv"))

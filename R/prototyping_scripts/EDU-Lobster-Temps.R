# Loading SST for areas that EDU needs for lobster lesson:


# Libraries
library(gmRi)
library(raster)
library(tidyverse)


# Tidy function for raster stack to timeseries
minor_adjustments <- function(sst_data){
  
  # get average over the area, get the values in F
  sst_data %>% 
    stack() %>% 
    cellStats(mean, na.rm = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    setNames(c("time", "sst_c")) %>% 
    mutate(
      time = as.Date(str_sub(str_replace_all(time, "[.]", "-"),2,-1)),
      sst_f = as_fahrenheit(sst_c))
  
}


# A. Load SST for Providence, Montauk, and Atlantic City
providence_sst <- oisst_window_load(
  data_window = data.frame(
    lon = c(-71.75, -71.0),
    lat = c(41.2, 41.6),
    time = as.Date(c("1982-01-01", "2023-12-31"))), 
  anomalies = F, 
  box_location = "cloudstorage")

montauk_sst <- oisst_window_load(
  data_window = data.frame(
    lon = c(-71.629701, 71.859207),
    lat = c(40.928657, 41.156543),
    time = as.Date(c("1982-01-01", "2023-12-31"))),
  anomalies = F,
  box_location = "cloudstorage")

atlanticcity_sst <- oisst_window_load(
  data_window = data.frame(
    lon = c(-74.296738, 73.888760),
    lat = c(39.077319, 39.437691),
    time = as.Date(c("1982-01-01", "2023-12-31"))),
  anomalies = F,
  climate_ref = "1991-2020",
  box_location = "cloudstorage")



# B. Make them timeseries and add fahrenheit:

# Prep the three regions
providence_ts <- minor_adjustments(providence_sst)
montauk_ts <- minor_adjustments(montauk_sst)
atlanticcity_ts <- minor_adjustments(atlanticcity_sst)



# Plots
plot(providence_sst$`1982`$X1982.01.02, main = "1982-01-01 SST")
ggplot(providence_ts, aes(time, sst_c)) + geom_line() + geom_hline(yintercept = 20, color = "darkred")
ggplot(montauk_ts, aes(time, sst_c)) + geom_line() + geom_hline(yintercept = 20, color = "darkred")
ggplot(atlanticcity_ts, aes(time, sst_c)) + geom_line() + geom_hline(yintercept = 20, color = "darkred")



# Save locally for Eliza
write_csv(providence_ts, here::here("local_data/EDU_sst/providence_sst_oisst.csv"))
write_csv(montauk_ts, here::here("local_data/EDU_sst/montauk_sst_oisst.csv"))
write_csv(atlanticcity_ts, here::here("local_data/EDU_sst/atlantic_city_sst_oisst.csv"))

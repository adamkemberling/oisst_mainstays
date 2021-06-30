####  Contrasting GOM/Shelf/Global  ####


####  Packages  ####
library(rnaturalearth)
library(sf)
library(stars)
library(lubridate)
library(gmRi)
library(here)
library(raster)
library(patchwork)
library(heatwaveR)
library(tidyverse)
library(ggforce)
library(gt)



#box paths
box_paths <- research_access_paths()
res_path <-  box_paths$res
oisst_path <-  box_paths$oisst_mainstays

# Support Functions
source(here("R/oisst_support_funs.R"))
source(here("R/temp_report_support.R"))

# set ggplot theme for figures
theme_set(theme_bw())


# OISST Data
gom_ts <- oisst_access_timeseries(oisst_path = box_paths$oisst_mainstays, 
                                  region_family = "gmri focus areas", 
                                  poly_name = "apershing gulf of maine")
# Need to fix this function
# shelf_ts <- oisst_access_timeseries(oisst_path = box_paths$oisst_mainstays, 
#                                     region_family = "NELME_regions", 
#                                     poly_name = "nelme")
shelf_ts <- read_csv(get_timeseries_paths("nelme_regions")$NELME$timeseries_path)
world_ts <- read_csv(str_c(oisst_path, "/global_timeseries/global_anoms_1982to2011.csv"))




# put them in a list 
area_list <- list(
  "Global Oceans" = world_ts,
  "Northeastern U.S. Shelf" = shelf_ts,
  "Gulf of Maine" = gom_ts)


####  Processing  ####

# make time a date
area_list <- map(area_list, ~ mutate(.x, time = as.Date(time)))


# Get Anomalies
area_hw <- map(area_list, pull_heatwave_events)



# 3. Get monthly averages
area_means <- map_dfr(area_hw, function(x){
  x %>%
    mutate(mnth = month(time, label = T, abbr = T),
           yr = year(time)) %>% 
    filter(yr == 2021) %>% 
    group_by(mnth) %>% 
    summarise(avg_temp  = round(mean(sst, na.rm = T), 2),
              avg_anom  = round(mean(sst_anom, na.rm = T), 2),
              n_hw_days = round(sum(mhw_event, na.rm = T)), 2) %>% 
    ungroup() 
  
  }, .id = "Region")



# 4. Get warming rates
area_rates <- map(area_hw, function(x){
  x <- x %>% 
    mutate(yr = year(time)) %>% 
    filter(between(yr, 1982, 2020)) %>% 
    group_by(yr) %>% 
    summarise(avg_temp = mean(sst, na.rm = T),
              avg_anom = mean(sst_anom, na.rm = T))
  
  temp_lm <- lm(avg_temp ~ yr, data = x)
  anom_lm <- lm(avg_anom ~ yr, data = x)
  return(
    list(
      temp = temp_lm,
      anom = anom_lm
    )
  )
  
})


# 5. Pull Rates
rate_table <- imap_dfr(area_rates, function(mod, area){
  data.frame("Region" = rep(area, 2),
             "unit" = c("sst", "sst_anom"),
             "intercept" = c(coef(mod$temp)[[1]], coef(mod$anom)[[1]]),
             "slope"     = c(coef(mod$temp)[[2]], coef(mod$anom)[[2]]))
})



####  Building Tables  ####

# Pivot
anom_avgs <- area_means %>% select(Region, mnth, avg_anom) %>% pivot_wider(names_from = mnth, values_from = avg_anom)# %>% column_to_rownames("area")
heat_avgs <- area_means %>% select(Region, mnth, n_hw_days) %>% pivot_wider(names_from = mnth, values_from = n_hw_days)# %>% column_to_rownames("area")

# 1. Anomalies
deg_c <- expression(~degree~C)
anom_avgs %>% 
  gt(rowname_col = "Region") %>% 
  tab_stubhead(label = "Region") %>% 
  tab_header(
    title = md("**Average Temperature Anomalies - 2021**"), 
    subtitle = paste("Degree Celsius Above Normal")) %>%
  tab_source_note(
    source_note = md("*Data Source: NOAA OISSTv2 Daily Sea Surface Temperature Data.*") ) %>% 
  tab_source_note(md("*Reference Climatolgy Period: 1982-2011.*"))



# 2. HW Days
heat_avgs %>% 
  gt(rowname_col = "Region") %>% 
  tab_stubhead(label = "Region") %>% 
  tab_header(
    title = md("**Number of Heatwave Days - 2021**")) %>%
  tab_source_note(md("*Data Source: NOAA OISSTv2 Daily Sea Surface Temperature Data.*") ) %>% 
  tab_source_note(md("*Reference Climatolgy Period: 1982-2011.*"))





####  Anomaly Maps  ####

# What data we want to load
data_window <- data.frame(lon = c(-180, 180),
                          lat = c(-90, 90),
                          time = as.Date(c("2021-01-01", "2021-06-30")))

# Load it up
oisst_daily <- oisst_window_load(oisst_path = oisst_path, data_window = data_window, anomalies = TRUE)
oisst_daily <- raster::stack(oisst_daily)

# Make it monthly
make_monthly <- function(daily_ras){
  # Months to subset with
  month_key <- str_pad(c(1:12), 2, "left", 0) %>% setNames(month.abb)

  # names to match index to
  layer_index <- names(daily_ras)
  month_index <- str_sub(layer_index, 7, 8)
  
  # Pull distinct months
  months_present <- unique(month_index)
  month_key <- month_key[which(month_key %in% months_present)]
  
  # Pull the indices that match, take means
  map(month_key, function(x){
    
    # Pull days in month
    days_in_month <- which(month_index == x)
    
    # Take mean of those days
    month_avg <- mean(daily_ras[[days_in_month]])
  }) %>% 
    #stack() %>% 
    setNames(names(month_key))
  
  }


# make it monthly
oisst_monthly <- make_monthly(oisst_daily)
monthly_stars <- map(oisst_monthly, ~ st_as_stars(raster::rotate(.x)))


# Crop areas
gom_shape <- get_timeseries_paths("gmri_sst_focal_areas")[["apershing_gulf_of_maine"]]["shape_path"]
gom_shape <- read_sf(gom_shape)
nelme_shape <- get_timeseries_paths("nelme_regions")$NELME$timeseries_path
nelme_shape <- read_sf(nelme_shape)




####  Make Maps  ####

# Polygons for mapping
new_england <- ne_states("united states of america") %>% st_as_sf(crs = 4326) 
canada      <- ne_states("canada") %>% st_as_sf(crs = 4326)
world       <- ne_countries() %>% st_as_sf(crs = 4326)
greenland   <- ne_states(country = "greenland") %>% st_as_sfc(crs = 4326)


# World base map
world_base <- ggplot() +
  geom_sf(data = world, fill = "gray90", size = .25) +
  map_theme


# Mapping Months
imap(monthly_stars, function(x, y){
  world_base +
    geom_stars(data = x) +
    labs(title = y)
  
})


# country base map
country_base <- ggplot() +
  geom_sf(data = new_england, fill = "gray90", size = .25) +
  geom_sf(data = canada, fill = "gray90", size = .25) +
  geom_sf(data = greenland, fill = "gray90", size = .25) +
  map_theme




# put crop extents in list
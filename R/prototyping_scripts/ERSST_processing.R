# Preparing ERSSTv5 Data to share externally with Others:

# Packages
library(gmRi)
library(tidyverse)
library(lubridate)
library(raster)
library(sf)
library(rnaturalearth)
library(stars)
library(here)

suppressPackageStartupMessages( source(here("R/temp_report_support.R"), verbose = FALSE) )


# Pre-Processed Timeline (updated Feb 2, 2022)
ersst_path <- cs_path("res", "ERSSTv5")
ersst_dat <- read_csv(str_c(ersst_path, "ERSSTv5_anom_apershing_gulf_of_maine.csv"))


# Tidying

# Notes:
# Data before 1940 unreliable
# Don't need area-adjusted and non-area-adjusted, just one
# Maybe don't need climatology either...

# Tidy it up
sst_tidy <- ersst_dat %>% 
  filter(year(time) >= 1940,
         year(time) <= 2021) %>% 
  select(time,
         surface_temp_c = area_wtd_sst,
         temp_anomaly_c = area_wtd_sst_anom) %>% 
  mutate(climate_avg_c  = surface_temp_c - temp_anomaly_c,
         surface_temp_f = as_fahrenheit(surface_temp_c, data_type = "temperature"),
         temp_anomaly_f = as_fahrenheit(temp_anomaly_c, data_type = "anomalies"),
         climate_avg_f  = as_fahrenheit(climate_avg_c, data_type = "temperature"))


# Plot to check

# Temp
ggplot(sst_tidy, aes(time, surface_temp_c)) + geom_line() 
ggplot(sst_tidy, aes(time, surface_temp_f)) + geom_line() 

# Anoms
ggplot(sst_tidy, aes(time, temp_anomaly_c)) + geom_line() 
ggplot(sst_tidy, aes(time, temp_anomaly_f)) + geom_line() 

# Climatology
ggplot(sst_tidy, aes(time, climate_avg_c)) +geom_line() 
ggplot(sst_tidy, aes(time, climate_avg_f)) + geom_line() 


# Export it:
out_name <- str_c(ersst_path, "/for_sharing/GulfOfMaine_ERSSTv5_1940to2021.csv")
write_csv(sst_tidy, file = out_name)


####_____####

# Map of the Study Area just for demonstration sake:

# # Coastline Shapes
new_england <- ne_states("united states of america", returnclass = "sf")
canada      <- ne_states("canada", returnclass = "sf")


# Load ERSST Grid
ersst_grid <- stack(str_c(ersst_path, "sst.mnmean_2022_02_02.nc"))


# Load shapefile for GOM
group_name <- "gmri_sst_focal_areas"
region_name <- "apershing_gulf_of_maine"
region_paths <- get_timeseries_paths(region_group = group_name, 
                                     mac_os = "mojave")

# Polygon Path
poly_path <- region_paths[[region_name]][["shape_path"]]
region_extent <- read_sf(poly_path)


# Pull extents for the region for crop
crop_x <- st_bbox(region_extent)[c(1,3)]
crop_y <- st_bbox(region_extent)[c(2,4)]

# Expand the area out to see the larger patterns
crop_x <- crop_x + c(-2.25, 2.25)
crop_y <- crop_y + c(-0.75, 0.75)

# Crop ersst grid
nc_extent <- make_cropbox(xlims = crop_x, ylims = crop_y)
ersst_masked <- mask_nc(ersst_grid$X2020.06.01, nc_extent)


# Full map of GOM
ggplot() +
  geom_stars(data = st_as_stars(ersst_masked)) +
  geom_sf(data = new_england, fill = "gray90", size = .25) +
  geom_sf(data = canada, fill = "gray90", size = .25) +
  geom_sf(data = region_extent, 
          color = "black", 
          fill = gmri_cols("gmri blue"), alpha = 0.2, linetype = 2, size = 0.5) +
  coord_sf(xlim = crop_x, 
           ylim = crop_y, expand = T) +
  scale_fill_distiller(palette = "RdBu", na.value = "transparent") +
  map_theme(legend.position = "bottom") +
  labs(fill = "ERSSTv5 June 2020, Temperature (C)")

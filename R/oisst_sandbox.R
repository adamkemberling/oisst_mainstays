#### Sandbox Script for testing/acccessing oisst_mainstay materials


####  Packages  ####
library(lubridate)
library(ncdf4)
library(raster)
library(rnaturalearth)
library(sf)
library(stars)
library(gmRi)
library(here)
library(janitor)
library(patchwork)
library(tidyverse)
library(heatwaveR)
library(plotly)

####  File Paths  ####

#box paths
box_paths <- research_access_paths()

# File Paths
mills_path <- box_paths$mills
res_path   <- box_paths$res
okn_path   <- box_paths$okn  


####  Themes  ####

# set ggplot theme for figures
theme_set(theme_bw())


# change theme up for maps
map_theme <- list(
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    plot.background = element_rect(color = "transparent", fill = "transparent"),
    line = element_blank(),
    axis.title.x = element_blank(), # turn off titles
    axis.title.y = element_blank(),
    legend.position = "bottom", 
    legend.title.align = 0.5))

# Polygons for mapping
new_england <- ne_states("united states of america") %>% st_as_sf(crs = 4326) 
canada <- ne_states("canada") %>% st_as_sf(crs = 4326)
world <- ne_countries() %>% st_as_sf(crs = 4326)
greenland <- ne_states(country = "greenland") %>% st_as_sfc(crs = 4326)



####  Mapping 82-2011 Climatology  ####

# Load for whatever day of the year it is
today <- Sys.Date()
today_label <- paste0("X", yday(today))
clim_82 <- stack(str_c(res_path, "OISST/oisst_mainstays/daily_climatologies/daily_clims_1982to2011.nc"))
todays_clim <- clim_82[[today_label]]

# attempt to change min value for color scale
todays_clim <- setMinMax(todays_clim)
minValue(todays_clim)

####__ Global Plot  ####
today_st <- st_as_stars(rotate(todays_clim))

ggplot() +
  geom_stars(data = today_st) +
  geom_sf(data = world, fill = "gray90") +
  scale_fill_distiller(palette = "RdBu") +
  #scale_fill_gradient2(low = "blue", mid = "white", high = "red", na.value = "transparent") +
  map_theme +
  guides("fill" = guide_colorbar(
    title = paste0("Sea Surface Temperature Climate Average - ", str_sub(today, -5, -1)),
    title.position = "top",
    title.hjust = 0.5,
    barwidth = unit(4, "in"),
    frame.colour = "black",
    ticks.colour = "black"))




####__ GOM Zoom  ####
# Clip Raster - Convert to stars

# Cpr Survey Extent ass an example
crop_x <- c(-70.875, -65.375)
crop_y <- c(40.375,   45.125)
shape_extent <- c(crop_x, crop_y)
region_ras <- crop(rotate(todays_clim), extent(shape_extent))
region_st <- st_as_stars(region_ras)

# Get crop bounds for coord_sf
crop_x <- st_bbox(region_st)[c(1,3)] 
crop_y <- st_bbox(region_st)[c(2,4)]


# Map Gulf of Maine area with
# Plot Gulf of Maine - wgs84
ggplot() +
  geom_stars(data = region_st) +
  geom_sf(data = new_england, fill = "gray90") +
  geom_sf(data = canada, fill = "gray90") +
  geom_sf(data = greenland, fill = "gray90") +
  scale_fill_distiller(palette = "RdBu") +
  map_theme +
  coord_sf(xlim = crop_x, ylim = crop_y, expand = T) +
  guides("fill" = guide_colorbar(
    title = paste0("Sea Surface Temperature Climate Average - ", str_sub(today, -5, -1)),
    title.position = "top",
    title.hjust = 0.5,
    barwidth = unit(4, "in"),
    frame.colour = "black",
    ticks.colour = "black"))




####  Warming Rate Figures  ####


# Function to plot the basic timeseries of a given area
plot_ts <- function(region_group, shape_name){
  
  #configure shape name
  shape_name <- tolower(str_replace_all(shape_name, " ", "_"))
  
  # Get Data
  file_path <- paste0(res_path, "OISST/oisst_mainstays/regional_timeseries/", region_group, "/OISSTv2_anom_", shape_name,".csv")
  region_ts <- read_csv(file_path, col_types = cols())
  
  # Clean the label
  location <- str_replace_all(shape_name, "_", " ")
  
  
  # SST
  p1 <- ggplot(region_ts, aes(time, sst)) +
    geom_line()  +
    labs(x = "Date", y = "Sea Surface Temperature",
         caption = location)
  
  # SST Anoms
  p2 <- ggplot(region_ts, aes(time, sst_anom)) +
    geom_line() +
    labs(x = "Date", y = "Temperature Anomaly",
         caption = location)
  
  # Yearly summary
  ann_summs <- region_ts %>% 
    mutate(year = year(time)) %>% 
    group_by(year) %>% 
    summarise(mean_sst = mean(sst, na.rm = T),
              mean_clim = mean(sst_clim, na.rm = T),
              mean_anom = mean(sst_anom, na.rm = T)) %>% 
    ungroup()
  
  # SST
  p3 <- ggplot(ann_summs, aes(year, mean_sst)) +
    geom_line() +
    labs(x = "", y = "Sea Surface Temperature",
         caption = location)
  
  # SST Anoms
  p4 <- ggplot(ann_summs, aes(year, mean_anom)) +
    geom_line() +
    labs(x = "", y = "Temperature Anomaly",
         caption = location)
  
  
  return(list("daily_temp" = p1,
       "daily_anoms" = p2,
       "yearly_temp" = p3,
       "yearly_anoms" = p4))

}


# Northeast Shelf
nes <- plot_ts("large_marine_ecosystems", "northeast_u.s._continental_shelf")

# Gulf of Maine - CPR
cpr_gom <- plot_ts("gmri_sst_focal_areas", "cpr_gulf_of_maine")

# Gulf of Maine - trawl
trawl_gom <- plot_ts("nmfs_trawl_regions", "gulf_of_maine")


nes$yearly_anoms / cpr_gom$yearly_anoms / trawl_gom$yearly_anoms




# check other shapefiles
nelme <- read_sf(paste0(res_path,"Shapefiles/NELME_regions/NELME_sf.shp"))
nes_lme <- read_sf(paste0(okn_path,"large_marine_ecosystems/LMEbb_07_all.geojson"))


p1 <- ggplot() +
  geom_sf(data = nelme, aes(fill = "Shapefiles/NELME_regions")) +
  coord_sf(xlim = c(-76.25, -65.4)) +
  labs(fill = "") +
  theme(legend.position = "bottom")

p2 <- ggplot() +
  geom_sf(data = nes_lme, aes(fill = "NSF OKN Demo Data/large_marine_ecosystems")) +
  coord_sf(xlim = c(-76.25, -65.4)) +
  labs(fill = "") +
  theme(legend.position = "bottom")

p1 / p2





####  Boken NETCDF Connections  ####
test_stack <- raster::stack("~/Box/RES_Data/OISST/oisst_mainstays/annual_observations/sst.day.mean.1982.v2.nc")
plot(test_stack$X1982.01.01)

# northeast large marine ecosystem, trawl area only
nelme_sf <- read_sf("~/Box/RES_Data/Shapefiles/NELME_regions/NELME_sf.shp")
p1 <- ggplot() +
  geom_sf(data = nelme_sf)

# nelme temperature timeline
nelme_temp <- read_csv("/Users/akemberling/Box/RES_Data/OISST/oisst_mainstays/regional_timeseries/NELME_regions/OISSTv2_anom_northeastern_us_shelf.csv")
p2 <- nelme_temp %>% 
  mutate(year = lubridate::year(time)) %>% 
  group_by(year) %>% 
  summarise(sst = mean(sst),
            sst_anom = mean(sst_anom)) %>% 
  ggplot(aes(year, sst_anom)) +
  geom_line() +
  geom_point() +
  labs(x = "", y = "Sea Surface Temperature Anomaly (degree C)")

# Compare to:

# northeast large marine ecosystem, actual LME with CB and BoF
nelme_true_sf <- read_sf("~/Box/NSF OKN Demo Data/large_marine_ecosystems/LMEbb_07_all.geojson")
p3 <- ggplot() +
  geom_sf(data = nelme_true_sf)

# nelme temperature timeline
nelme_true_temp <- read_csv("/Users/akemberling/Box/RES_Data/OISST/oisst_mainstays/regional_timeseries/large_marine_ecosystems/OISSTv2_anom_northeast_u.s._continental_shelf.csv")
p4 <- nelme_true_temp %>% 
  mutate(year = lubridate::year(time)) %>% 
  group_by(year) %>% 
  summarise(sst = mean(sst, na.rm = T),
            sst_anom = mean(sst_anom, na.rm = T)) %>% 
  ggplot(aes(year, sst_anom)) +
  geom_line() +
  geom_point() +
  labs(x = "", y = "Sea Surface Temperature Anomaly (degree C)")


# Side by side
(p1 / p2) | (p3 / p4)

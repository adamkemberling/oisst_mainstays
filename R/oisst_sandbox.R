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

#box paths
box_paths <- research_access_paths()

# File Paths
mills_path <- box_paths$mills
res_path   <- box_paths$res
okn_path   <- box_paths$okn  


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



####  Load 82-2011 Climatology  ####

# Load for whatever day of the year it is
today <- Sys.Date()
today_label <- paste0("X", yday(today))
clim_82 <- stack(str_c(okn_path, "oisst/daily_climatologies/daily_clims_82to2011.nc"))
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

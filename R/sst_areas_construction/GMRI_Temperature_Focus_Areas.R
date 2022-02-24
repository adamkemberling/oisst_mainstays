#### GMRI Temperature Focus Areas
#### These Regions are of particular interest to GMRI's regional focuses
#### This is the polygon creation code for their reference boundaries



####  Packages  ####
library(rnaturalearth)
library(sf)
library(gmRi)
library(here)
library(patchwork)
library(tidyverse)

#box paths
box_paths <- research_access_paths()
res_path <-  box_paths$res

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




####  Andy Pershing Gulf of Maine  ####
# This extent follows the oisst index boundaries used for Andy's Gulf of Maine Temperature Figures
# Netcdf lon indices: Jlon = 1157:1179.
# Netcdf lat indices: Ilat = 522:541
# Matching lon coordinates: -70.875 : -65.375
# Matching lat coordinates:  40.375 :  45.125
# Code documenting this can be found in the `oisst_mainstays/mat` subfolder.

# Bounding Box Extent
andy_extent <- tribble(
  ~"lon", ~"lat",
  -70.875,	40.375, 
  -65.375,	40.375, 
  -65.375,	45.125, 
  -70.875,	45.125, 
  -70.875,	40.375
) %>% as.matrix() %>% 
  list() %>% 
  st_polygon()

# as simple feature collection
gom_andy <- st_sf(area = "CPR Gulf of Maine", st_sfc(andy_extent), crs = 4326)

# Display Plot
ggplot() +
  geom_sf(data = new_england, fill = "gray90") +
  geom_sf(data = canada, fill = "gray90") +
  geom_sf(data = gom_andy, color = "black", fill = "transparent", linetype = 2) +
  coord_sf(xlim = c(-70.875, -65.375), 
           ylim = c(40.375, 45.125), expand = T) +
  map_theme


# Prep for save
gom_andy <- gom_andy %>% mutate(area = "Gulf of Maine",
                                author = "Andy Pershing")

out_path <- paste0(res_path, "Shapefiles/gmri_sst_focal_areas/")

# Save it 
st_write(gom_andy, dsn = paste0(out_path, "apershing_gulf_of_maine.geojson"))



#### CPR Gulf of Maine  ####

cpr_extent <- tribble(
  ~"lon", ~"lat",
  -70.0,	42.2, 
  -66.6,	42.2, 
  -66.6,	43.8, 
  -70.0,	43.8, 
  -70.0,	42.2
) %>% as.matrix() %>% 
  list() %>% 
  st_polygon()

# as simple feature collection
gom_cpr <- st_sf(area = "CPR Gulf of Maine", st_sfc(cpr_extent), crs = 4326) %>% 
  mutate(author = "Adam Kemberling")

# Display Plot
ggplot() +
  geom_sf(data = new_england, fill = "gray90") +
  geom_sf(data = canada, fill = "gray90") +
  geom_sf(data = gom_cpr, color = "black", fill = "transparent", linetype = 2) +
  coord_sf(xlim = c(-70.875, -65.375), 
           ylim = c(40.375, 45.125), expand = T) +
  map_theme



# Save it 
st_write(gom_cpr, dsn = paste0(out_path, "cpr_gulf_of_maine.geojson"))








####  Northwest Atlantic  ####
# This reference area is used to capture temperature fluctuations of the labrador current
# as it flows South to the West of Greenland
# Lon extent: 
# Lat extent:
# Display CRS: Custom CRS centered on this bounding box
# 


# Create a general extent to use for the area
nw_extent <- tribble(
  ~"lon",  ~"lat",
  -73,	40.5, 
  -40,	40.5, 
  -40,	70, 
  -73,	70, 
  -73,	40.5
) %>% as.matrix() %>% 
  list() %>% 
  st_polygon()

# create simple feature collection
nw_region <- st_sf(area = "Northwest Atlantic Testing", st_sfc(nw_extent), crs = 4326)



####  Project Coordinates
# new sterographic projection for this area, centered using lon_0
stereographic_north <- "+proj=stere +lat_0=90 +lat_ts=75 +lon_0=-57"
nw_north            <- st_transform(nw_region, crs = stereographic_north)
canada_north        <- st_transform(canada, stereographic_north)
newengland_north    <- st_transform(new_england, stereographic_north)
greenland_north     <- st_transform(greenland, stereographic_north)

# coord_sf Crop bounds in projection units for coord_sf
crop_x <- st_bbox(nw_north)[c(1,3)] 
crop_y <- st_bbox(nw_north)[c(2,4)]

# Lower the ymin a touch
crop_y <- crop_y - c(100000, 0)


# Display the region
ggplot() +
  geom_sf(data = newengland_north, fill = "gray90") +
  geom_sf(data = canada_north, fill = "gray90") +
  geom_sf(data = greenland_north, fill = "gray90") +
  geom_sf(data = nw_north, color = "black", fill = "transparent", linetype = 2) +
  coord_sf(crs = stereographic_north, xlim = crop_x, ylim = crop_y, expand = T)



# Prep to export
nw_region <- nw_region %>% 
  mutate(
    area         = "Northwest Atlantic",
    author       = "AAK",
    crs          = "WGS1984",
    display_proj = "stereographic north",
    display_crs  = "+proj=stere +lat_0=90 +lat_ts=75 +lon_0=-57")


# Save it
st_write(nw_region, paste0(out_path, "aak_northwest_atlantic.geojson"))





####____####
####  Validation of Apershing extent  ####
# This code was used to determine lat lon extent for Andy's temperature


# Access information and paths to netcdf on box
okn_path <- shared.path(group = "NSF OKN", folder = "oisst/annual_anomalies/")
nc_year <- "2020"
anom_path <- str_c(okn_path, "daily_anoms_", nc_year, ".nc")

# Open 2020 Anomalies netcdf
nc <- nc_open(anom_path)

# Set andy's extent index values for gulf of maine
lon_idx <- 1157:1179
lat_idx <- 522:541
time_idx <- 1 #Jan 1

# Grab Jan 1 from netcdf array
andy_clip <- ncdf4::ncvar_get(nc, varid = "sst")[lon_idx, lat_idx, 1]
andy_clip <- t(andy_clip) # transpose it

# Return the non-index values for dimensions
xvals <- nc$dim$lon$vals[lon_idx] - 360
yvals <- nc$dim$lat$vals[lat_idx]
time_label <- as.Date(nc$dim$time$vals[time_idx], origin = paste0('2020-01-01'), tz = "GMT")


# Make it a raster
andy_gom_anoms <- raster::raster(andy_clip,
                                 crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                                 xmn = min(xvals),
                                 xmx = max(xvals),
                                 ymn = min(yvals),
                                 ymx = max(yvals)) %>% 
  setNames(time_label)

# Rotate one last time...
andy_gom_anoms <- flip(andy_gom_anoms, 2)

# As stars
andy_stars <- st_as_stars(andy_gom_anoms)

# Plot
ggplot() +
  geom_stars(data = andy_stars)  # see GMRI_template_focus_areas.R
  
  
#### NW Atlantic Notes  ####
"I was not able to locate a shapefile for the Northwest Atlantic Region 
so I'm going to eyeball it for now off the image in the word doc. 
Good practice for making masks or shapes programmatically.

Andy's changes:   

 > I’d suggest we move the southern extent down to at least Long Island, 
 and we may want to move the eastern bound a bit further to pick up 
 the southern part of E Greenland (~over to 40 W long).
"
  

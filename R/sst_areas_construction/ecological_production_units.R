# Northeast Ecological Production Units
# Source: NEFSC and ecodata

# Libraries
library(tidyverse)
library(gmRi)
library(ecodata)
library(sf)

# Path prep
epu_path <- cs_path("res", "shapefiles/EPU/individual_epus")


# Polygon Prep
epu_sf <- ecodata::epu_sf %>% 
  mutate(source = "ecodata r-package")

# Transform
epu_wgs <- st_transform(epu_sf, crs = st_crs("EPSG:4326"))

# Map it
ggplot() + geom_sf(data = epu_wgs, aes(fill = EPU))

# list the names
epu_names <- unique(epu_wgs$EPU)

# filter them into a list
epu_list <- map(epu_names, function(x){dplyr::filter(epu_wgs, EPU == x)}) %>% setNames(epu_names)

# Map one
ggplot() + geom_sf(data = epu_list$GB, aes(fill = EPU))

# Export
iwalk(epu_list, function(poly, poly_name){
  st_write(obj = poly, dsn = str_c(epu_path, poly_name, ".geojson"))
  
})


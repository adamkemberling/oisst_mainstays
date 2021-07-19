####  Building LIS Shapefile
# make a box
# clip out Connecticut
# Add to GMRI Focal areas


####  Packages  ####
library(rnaturalearth)
library(sf)
library(gmRi)
library(here)
library(tidyverse)

#box paths
box_paths <- research_access_paths()
res_path <-  box_paths$res

# Support Functions
source(here("R/oisst_support_funs.R"))
source(here("R/temp_report_support.R"))


#### Long Island Sound  ####

# Polygons for mapping
new_england <- ne_states("united states of america") %>% st_as_sf(crs = 4326) 
state_mask <- filter(new_england, name %in% c("Connecticut", "New York"))



####  Long Island Sound  ####
# This extent was drawn manually using point coordinates from google earth

# Bounding Box Extent
shape_extent <- tribble(
  ~"lon",   ~"lat",
  -73.842,	40.693, 
  -73.154,	40.843, 
  -72.632,	40.945, 
  -72.039,	41.218, 
  -72.001,	41.380,
  -72.725,  41.382,
  -73.664,  41.115,
  -73.900,  40.802,
  -73.842,	40.693,
) %>% as.matrix() %>% 
  list() %>% 
  st_polygon()

# as simple feature collection
lis_box <- st_sf(area = "Long Island Sound", st_sfc(shape_extent), crs = 4326)

# Display Plot
ggplot() +
  geom_sf(data = new_england, fill = "gray90") +
  geom_sf(data = lis_box, color = "black", fill = "blue", linetype = 2, size = 1, alpha = 0.3) +
  coord_sf(xlim = c(-74.5, -71), 
           ylim = c(40.375, 42), expand = T)



# Remove coastlines from the shape
lis_clipped <- st_difference(lis_box, y = st_union(state_mask))

# Display Plot
ggplot() +
  geom_sf(data = new_england, fill = "gray90") +
  geom_sf(data = lis_clipped, color = "black", fill = "blue", linetype = 2, size = 1, alpha = 0.3) +
  coord_sf(xlim = c(-74.1, -71.9), 
           ylim = c(40.6, 41.5), expand = T)


# Prep for save
lis_out <- lis_clipped %>% mutate(area = "Long Island Sound",
                                author = "Adam Kemberling")

out_path <- paste0(res_path, "Shapefiles/gmri_sst_focal_areas/")

# Save it 
st_write(lis_out, dsn = paste0(out_path, "long_island_sound.geojson"))

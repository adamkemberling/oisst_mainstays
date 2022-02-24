####  Reprojecting to orthographic coordinates
# Taken out of annual report:
# Can't reproject the polygons correctly





# Mapping in orthographic projection:

# Function for making a bounding box
make_bbox <- function(xlim = c(-75, -60), ylim = c(32, 46), crs =  4326){
  # Build bbox to crop with
  crop_bbox <- st_bbox(
    c(xmin = xlim[1],
      ymin = ylim[1],
      xmax = xlim[2],
      ymax = ylim[2]),
    crs = st_crs(crs))
  return(crop_bbox)
}


#zoom_bbox in different CRS
make_bbox() %>% 
  st_transform(ortho)



# # Issues: The warping step  is having issues, try cropping first
# crop_grid_bydim <- function(in_ras, xlim = c(-80, -50), ylim = c(20, 60), crs = crs){
#   
#   
#   
#   st_crop(in_ras, crop_bbox)
# }
# 
# 
# # Warp it to orthographic projection
# warp_to_ortho <- function(in_grid_st, ortho_lat = 25, ortho_lon = -50){
#   # orthographic projection
#   lat <- ortho_lon
#   lon <- ortho_lon
#   ortho <- paste0('+proj=ortho +lat_0=', lat, ' +lon_0=', lon, ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')
#   
#   # Build grid in the crs you wish to warp to
#   projection_grid <- in_grid_st %>% 
#     st_transform(crs = ortho) %>% 
#     st_bbox() %>%
#     st_as_stars()
#   
#   # Warp to projection grid of chosen CRS
#   region_warp_ras <- in_grid_st %>% 
#     st_warp(projection_grid) 
#   
#   return(region_warp_ras)
#   
# }
# 
# 
# # Crop it down to a manageable area
# ranks_cropped <- crop_grid_bydim(in_ras = st_as_stars(rotate(ranks_21)), crs = 4326)
# 
# # use warping function:
# ranks_ortho <- warp_to_ortho(in_grid_st = ranks_cropped)


# source:
# https://gist.github.com/rafapereirabr/26965dd851debad32ad2e659024ba451


# Orthographic projection
ortho_lat <- 25
ortho_lon <- -60
ortho <- paste0('+proj=ortho +lat_0=', ortho_lat, ' +lon_0=', ortho_lon, ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')


# Can we just project the original?
ranks_ortho <- projectRaster(rotate(ranks_21), crs = ortho)
ortho_st <- st_as_stars(ranks_ortho) %>% 
  mutate(
    temp_rank = case_when(
      X2021 == 1 ~ "Coldest",
      X2021 == 2 ~ "2nd Coldest",
      X2021 == 3 ~ "3rd Coldest",
      X2021 == 4 ~ "4th Coldest",
      X2021 == 5 ~ "5th Coldest",
      X2021 == 36 ~ "5th Warmest",
      X2021 == 37 ~ "4th Warmest",
      X2021 == 38 ~ "3rd Warmest",
      X2021 == 39 ~ "2nd Warmest",
      X2021 == 40 ~ "Warmest",
      TRUE ~ "Not a Record Year"),
    temp_rank = fct_drop(temp_rank),
    temp_rank = factor(temp_rank, levels = c(
      "Warmest", "2nd Warmest", "3rd Warmest", "4th Warmest", "5th Warmest",
      "Not a Record Year",
      "5th Coldest", "4th Coldest", "3rd Coldest", "2nd Coldest", "Coldest")))


# Try to:
# Fix polygons so they don't get cut in ortho projection
world <- rnaturalearth::ne_countries(scale = 'small', returnclass = 'sf')
world_ortho <- st_cast(world, 'MULTILINESTRING') %>%
  st_cast('LINESTRING', do_split=TRUE) %>%
  mutate(npts = mapview::npts(geometry, by_feature = TRUE)) %>%
  st_cast('POLYGON') %>% 
  st_transform(ortho) %>% 
  filter( is.na(st_dimension(.)) == FALSE )



# and map
ggplot() +
  geom_stars(data = select(ortho_st, -X2021)) +
  scale_fill_brewer(palette = "RdBu",  na.value = "gray50") +
  coord_sf(crs = ortho, expand = F) +
  map_theme


# Preserving polygons in ortho view:
# https://gist.github.com/fzenoni/ef23faf6d1ada5e4a91c9ef23b0ba2c1

# checking geometries https://r-spatial.org/r/2017/03/19/invalid.html#empty-geometries

# Define the orthographic projection
# Choose lat_0 with -90 <= lat_0 <= 90 and lon_0 with -180 <= lon_0 <= 180
lat <- 45
lon <- -65
ortho <- paste0('+proj=ortho +lat_0=', lat, ' +lon_0=', lon, ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')


# Load the world outlines
world <- ne_states(returnclass = "sf")



# Define the polygon that will help you finding the "blade"
# to split what lies within and without your projection
circle <- st_point(x = c(0,0)) %>% 
  st_buffer(dist = 6371000) %>% 
  st_sfc(crs = ortho)

# Project this polygon in lat-lon
circle_longlat <- circle %>% st_transform(crs = 4326)

# circle_longlat cannot be used as it is
# You must decompose it into a string with ordered longitudes
# Then complete the polygon definition to cover the hemisphere
if(lat != 0) {
  circle_longlat <- st_boundary(circle_longlat)
  
  circle_coords <- st_coordinates(circle_longlat)[, c(1,2)]
  circle_coords <- circle_coords[order(circle_coords[, 1]),]
  circle_coords <- circle_coords[!duplicated(circle_coords),]
  
  # Rebuild line
  circle_longlat <- st_linestring(circle_coords) %>% st_sfc(crs = 4326)
  
  if(lat > 0) {
    rectangle <- list(rbind(circle_coords,
                            c(X = 180, circle_coords[nrow(circle_coords), 'Y']),
                            c(X = 180, Y = 90),
                            c(X = -180, Y = 90),
                            c(X = -180, circle_coords[1, 'Y']),
                            circle_coords[1, c('X','Y')])) %>% 
      st_polygon() %>% st_sfc(crs = 4326)
  } else {
    rectangle <- list(rbind(circle_coords,
                            c(X = 180, circle_coords[nrow(circle_coords), 'Y']),
                            c(X = 180, Y = -90),
                            c(X = -180, Y = -90),
                            c(X = -180, circle_coords[1, 'Y']),
                            circle_coords[1, c('X','Y')])) %>% 
      st_polygon() %>% st_sfc(crs = 4326)
  }
  
  circle_longlat <- st_union(st_make_valid(circle_longlat), st_make_valid(rectangle))
}

# This visualization shows the visible emisphere in red
ggplot() +
  geom_sf(data = world) +
  geom_sf(data = circle_longlat, color = 'red', fill = 'red', alpha = 0.3)



# A small negative buffer is necessary to avoid polygons still disappearing in a few pathological cases
# I should not change the shapes too much
visible <- st_intersection(st_make_valid(world),   
                           st_buffer(circle_longlat, -0.09)) %>%
  st_transform(crs = ortho)

# DISCLAIMER: This section is the outcome of trial-and-error and I don't claim it is the best approach 
# Resulting polygons are often broken and they need to be fixed
# Get reason why they're broken
broken_reason <- st_is_valid(visible, reason = TRUE)

# First fix NA's by decomposing them
# Remove them from visible for now
na_visible <- visible[is.na(broken_reason),]
visible <- visible[!is.na(broken_reason),]

# Open and close polygons
na_visible <- st_cast(na_visible, 'MULTILINESTRING') %>% 
  st_cast('LINESTRING', do_split=TRUE)
na_visible <- na_visible %>% 
  mutate(npts = mapview::npts(geometry, by_feature = TRUE))

# Exclude polygons with less than 4 points
na_visible <- na_visible %>%
  filter(npts >=4) %>%
  select(-npts) %>%
  st_cast('POLYGON')

# Fix other broken polygons
broken <- which(!st_is_valid(visible))
for(land in broken) {
  result = tryCatch({
    # visible[land,] <- st_buffer(visible[land,], 0) # Sometimes useful sometimes not
    visible[land,] <- st_make_valid(visible[land,]) %>% 
      st_collection_extract()  
  }, error = function(e) {
    visible[land,] <<- st_buffer(visible[land,], 0)
  })
}

# Bind together the two tables
visible <- rbind(visible, na_visible)

# Final plot
ggplot() +
  geom_sf(data = circle,
          #fill = 'aliceblue') + # if you like the color
          fill = NA) +
  geom_sf(data = st_collection_extract(visible)) +
  coord_sf(crs = ortho)



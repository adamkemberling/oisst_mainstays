####  Regional Prjoection Code  ####
#
#
# This code was removed from the temperature report because it was redundant
# wanted to hang on to it because it was helpful




####  NOTE: 
# transformation from rasters different than polygons 
# as cells need to stretch and bend  ####
# Resource: Tranformation vs. Warping 
# https://r-spatial.github.io/stars/articles/stars5.html



#### Candidate coordinate reference systems ####

# Robinson projection
robinson_proj <- "+proj=robin"

# Albers equal area: centered on -70 degrees
# The settings for lat_1 and lat_2 are the locations at which 
# the cone intersects the earth, so distortion is minimized at those latitudes
alb_70 <- "+proj=aea +lat_1=30 +lat_2=50 +lon_0=-70"

# custom sterographic projection for this nw atlantic, centered using lon_0
stereographic_north <- "+proj=stere +lat_0=90 +lat_ts=75 +lon_0=-57"

# equal earth projection - not working, don't think sf has functionality for it yet
eqearth_proj <- "+proj=eqearth"



####  Projection Toggle  ####

# Choose crs using parameter
projection_crs <- switch(
  tolower(params$region),
  "gulf of maine" = alb_70,
  "cpr gulf of maine" = alb_70,
  "northwest atlantic" = stereographic_north)




####  Transform Polygons  ####


# Transform all the polygons
region_projected        <- st_transform(region_extent, crs = projection_crs)
canada_projected        <- st_transform(canada, crs = projection_crs)
newengland_projected    <- st_transform(new_england, crs = projection_crs)
greenland_projected     <- st_transform(greenland, crs = projection_crs)

# coord_sf Crop bounds in projection units for coord_sf
crop_x <- st_bbox(region_projected)[c(1,3)] 
crop_y <- st_bbox(region_projected)[c(2,4)]


# Zoom out for cpr extent, same as Andy's GOM
if(tolower(params$region) == "cpr gulf of maine"){
  crop_x <- c(-73185.13, 386673.34)
  crop_y <- c(4269044,   4813146)}

# Lower the ymin a touch for NW Atlantic
if(tolower(params$region) == "northwest atlantic"){crop_y <- crop_y - c(100000, 0)}

####  warping Grid  ####

# Warp to grid of chosen CRS
projection_grid <- region_st %>% 
  st_transform(projection_crs) %>% 
  st_bbox() %>%
  st_as_stars()
region_warp_ras <- region_st %>% 
  st_warp(projection_grid) 


#### Plot  ####

# Plot everything together
ggplot() +
  geom_stars(data = region_warp_ras) +
  geom_sf(data = newengland_projected, fill = "gray90") +
  geom_sf(data = canada_projected, fill = "gray90") +
  geom_sf(data = greenland_projected, fill = "gray90") +
  coord_sf(crs = projection_crs, 
           xlim = crop_x, ylim = crop_y, expand = T) +
  map_theme +
  scale_fill_distiller(palette = "RdBu", na.value = "transparent") +
  guides("fill" = guide_colorbar(title = "Average Sea Surface Temperature Anomaly",
                                 title.position = "top", 
                                 title.hjust = 0.5,
                                 barwidth = unit(4, "in"), 
                                 frame.colour = "black", 
                                 ticks.colour = "black")) +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    plot.background = element_rect(color = "transparent", fill = "transparent"),
    axis.title.x = element_blank(), # turn off titles
    axis.title.y = element_blank(),
    legend.position = "bottom", 
    legend.title.align = 0.5)
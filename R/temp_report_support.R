####
####  Temperature Repost Support
####  4/20/2021
####  Goal:
#### Move plotting and tidying steps here to reduce clutter and redundancy


library(boaR)
library(raster)
library(tidyverse)



####  Belkin Oreailly Fronts  ####
# remotes::install_github("galuardi/boaR", 
#                         force = T, 
#                         build = T, 
#                         dependencies = "ask",
#                         upgrade = "ask")

get_belkin_fronts <- function(in_ras){
  
  
  
  # Convert to matrix
  front_matrix <- t(as.matrix(in_ras))
  
  # Getting coordinates from raster
  test_coords <- raster::coordinates(in_ras)
  xcoords <- sort(unique(test_coords[,"x"]))
  ycoords <- sort(unique(test_coords[,"y"]))
  rownames(front_matrix) <- xcoords
  colnames(front_matrix) <- ycoords
  
  
  
  # Getting Belkin O'Reilly Fronts
  sst_fronts <- boaR::boa(lon = xcoords, 
                          lat = ycoords, 
                          ingrid = front_matrix, 
                          nodata = NA, 
                          direction = TRUE)
  
  # Back to correct lon/lat
  sst_fronts <- raster::flip(sst_fronts$front, direction = "y")
  
  return(sst_fronts)
  
}





# Warping Rasters
warp_grid_projections <- function(in_grid_st, projection_crs = c("world robinson", "albert conical", "stereo north")){
  
  ####  Projections
  
  # 1. Robinson projection
  robinson_proj <- st_crs("+proj=robin") # or st_crs(54030)
  
  # 2. Albers equal area: centered on -70 degrees
  # The settings for lat_1 and lat_2 are the locations at which 
  # the cone intersects the earth, so distortion is minimized at those latitudes
  alb_70 <- st_crs("+proj=aea +lat_1=30 +lat_2=50 +lon_0=-70")
  
  # 3. Custom sterographic projection for this nw atlantic, centered using lon_0
  stereographic_north <- st_crs("+proj=stere +lat_0=90 +lat_ts=75 +lon_0=-57")
  
  # # 4. equal earth projection - not working, don't think sf has functionality for it yet
  # eqearth_proj <- st_crs("+proj=eqearth")
  
  # 5. World Mollweide
  # When mapping the world while preserving area relationships, the Mollweide projection is a good choice
  world_moll <- st_crs("+proj=moll")
  
  projection_crs <- switch(projection_crs,
                           "world robinson" = robinson_proj,
                           "albert conical" = alb_70,
                           "stereo north"   = sterographic_north,
                           "world moll"     = world_moll)
  
  
  # Build grid in the crs you wish to warp to
  projection_grid <- in_grid_st %>% 
    st_transform(projection_crs) %>% 
    st_bbox() %>%
    st_as_stars()
  
  # Warp to projection grid of chosen CRS
  region_warp_ras <- in_grid_st %>% 
    st_warp(projection_grid) 
  
  return(region_warp_ras)
  
}

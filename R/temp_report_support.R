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

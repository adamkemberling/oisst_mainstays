#### Gulf of Maine Rayshader Map



####  Libraries  ####
library(tidyverse)
library(gmRi)
library(raster)
library(rayshader)






####  Elevation Data  ####
b_path <- cs_path("res", "Bathy/gom03_v1_0asc")
gom_dem <- raster(str_c(b_path, "gom03_v1_0.asc"))


#And convert it to a matrix:
elmat = raster_to_matrix(gom_dem)


####  Making the Map  ####


gom_shadow = ray_shade(elmat, zscale = 50, lambert = FALSE)
gom_amb = ambient_shade(elmat, zscale = 50)
elmat %>%
  sphere_shade(zscale = 10, texture = "imhof1") %>%
  add_shadow(gom_shadow, 0.5) %>%
  add_shadow(gom_amb, 0) %>%
  plot_3d(elmat, 
          zscale = 50, 
          fov = 0, 
          theta = -45, phi = 45, 
          windowsize = c(1000, 800), 
          zoom = 0.6,
          water = TRUE, 
          waterdepth = 0, 
          wateralpha = 0.5, 
          watercolor = "lightblue",
          waterlinecolor = "white", 
          waterlinealpha = 0.5)
Sys.sleep(0.2)
render_snapshot(clear = TRUE)




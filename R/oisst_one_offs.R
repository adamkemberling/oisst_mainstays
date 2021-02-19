#### Getting one-off pulls of OISST:
#### 2/1/2021



####  Packages  ####
library(lubridate)
library(ncdf4)
library(raster)
library(rnaturalearth)
library(sf)
library(here)
library(janitor)
library(patchwork)
library(tidyverse)
library(heatwaveR)
library(gmRi)


# OISST File Paths
oisst_path <- shared.path(group = "RES_Data", folder = "OISST/oisst_mainstays/")


####  Gulf of Maine area OISST  ####

# General area for the Gulf of Maine
gom_window <- data.frame(lon = c(-71, -66), lat = c(41, 44.5), time = as.Date(c("1981-08-01", "2020-12-31")))

# load data as raster stack for particular area
gom_oisst <- oisst_window_load(oisst_path = oisst_path,
                               data_window = gom_window, 
                               anomalies = FALSE)


# Make annual averages
gom_oisst_yrs <- map(gom_oisst, function(yr_stack){ yr_avg <- mean(yr_stack) }) %>% stack()

#### Save it as Raster for Mackenzie  ####
# 
#one_off_path <- paste0(box_paths$res, "OISST/oisst_one_off_area_clips/oisst_gom_general_area_02042021.grd")

# Set the dates as the z dimmension before saving
gom_oisst_save <- gom_oisst %>% 
  map(function(ras_stack) {ras_out <- setZ(ras_stack, names(ras_stack))}) %>% 
  stack()

# raster::writeRaster(gom_oisst_save, 
#                     filename = one_off_path, 
#                     format="raster")

# # yearly stack
# writeRaster(gom_oisst_yrs, 
#             filename = paste0(box_paths$res,  "OISST/oisst_one_off_area_clips/oisst_gom_yrly_02042021.grd"))

# # Processing it as a table
# # Load to ensure names saved
# oisst_check <- stack(one_off_path)
# 
# #check names
# names(gom_oisst_save)[1:5] == names(oisst_check)[1:5]
# 
# # make it a table
# gom.df.wide <- map_dfr(gom_oisst, function(x) {as.data.frame(x, xy = TRUE)})
# rm(gom_oisst)
# 
# # Make it a crazy long-form table
# gom.df.long <- gom.df.wide %>% 
#   pivot_longer(names_to = "stack_name", values_to = "sst", cols = c(3:ncol(gom.df.wide)))
# rm(gom.df.wide)
# 
# # Clean up columns
# gom.df.long <- gom.df.long %>% 
#   mutate(
#     stack_name = str_replace(stack_name, "X", ""),
#     stack_name = str_replace_all(stack_name, "[.]", "-")) %>% 
#   rename(oisst_date = stack_name)
# 
# 
# # Test Plot
# gom.df.long %>% 
#   filter(oisst_date == "2020-03-09") %>% 
#   ggplot(aes(x, y, fill = sst)) +
#   geom_tile() 



####  Save for Mackenzie  ####

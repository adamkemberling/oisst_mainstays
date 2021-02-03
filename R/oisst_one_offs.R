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


####  File Paths  ####

# General box paths
box_paths <- research_access_paths(os.use = "unix")

# OISST File Paths
oisst_path <- shared.path(group = "RES_Data", folder = "OISST/oisst_mainstays/")




####  Gulf of Maine OISST as a table  ####

# General area for the Gulf of Maine
gom_window <- data.frame(lon = c(-71, -66), lat = c(41, 44.5), time = as.Date(c("1981-08-01", "2020-12-31")))

# load data as raster stack for particular area
gom_oisst <- oisst_window_load(oisst_path = oisst_path,
                               data_window = gom_window, 
                               anomalies = FALSE)


# make it a table
gom.df.wide <- map_dfr(gom_oisst, function(x) {as.data.frame(x, xy = TRUE)})
rm(gom_oisst)

# Make it a crazy long-form table
gom.df.long <- gom.df.wide %>% 
  pivot_longer(names_to = "stack_name", values_to = "sst", cols = c(3:ncol(gom.df.wide)))
rm(gom.df.wide)

# Clean up columns
gom.df.long <- gom.df.long %>% 
  mutate(
    stack_name = str_replace(stack_name, "X", ""),
    stack_name = str_replace_all(stack_name, "[.]", "-")) %>% 
  rename(oisst_date = stack_name)


# Test Plot
gom.df.long %>% 
  filter(oisst_date == "2020-03-09") %>% 
  ggplot(aes(x, y, fill = sst)) +
  geom_tile() 

####  Formatting Marine Physio Regions  ####
# Goal: Break up the aggregate area, pull out regions we care about and rename
# Needed to include bay of fundy with GOM Regions of interest

####  Libraries  ####
library(sf)
library(tidyverse)
library(gmRi)
library(patchwork)

# file paths
shape_paths <- box_path(box_group = "RES_Data", subfolder = "Shapefiles/GulfOfMainePhysioRegions")

####  Data  ####

# physio region collection
p_regions <- read_sf(str_c(shape_paths, "PhysioRegions_WGS84.shp"))

# map them
ggplot() +
  geom_sf(data = p_regions, aes(fill = Region)) +
  theme(legend.position = "bottom")

# Pull lookup table apart from shapes
p_lookup <- st_drop_geometry(p_regions)

# Shapefile Re-saving
region_maps <- p_regions %>% 
  split(.$Region) %>% 
  imap(function(physio_region, Region){
    
    # Visual Check
    full_p <- ggplot() +
      geom_sf(data = physio_region) +
      labs(title = Region)
    return(full_p)
    
  })



####  Export Shapes into sub-folder  ####
p_regions %>% 
  split(.$Region) %>% 
  walk(function(physio_region){
    
    # Pull components
    Region         <- physio_region$Region[1]
    RegionNum      <- physio_region$RegionNum[1]
    
    # Prepare save name
    out_name  <- str_replace_all(Region, " ", "_")
    out_name  <- str_replace_all(out_name, "-", "_")
    out_name  <- str_replace_all(out_name, "[.]", "")
    out_name  <- str_replace_all(out_name, "___", "_")
    out_name  <- str_replace_all(out_name, "__", "_")
    out_name  <- tolower(out_name)
    save_name <- paste0(shape_paths, "single_regions/", out_name, ".geojson")
    
    # Print to check out names
    print(paste("Saving", Region))
    print(paste0("as ", save_name))
    
    
    # # # Save Physio-Region
    # st_write(obj = physio_region,
    #          dsn = save_name,
    #          driver = "GeoJSON")
    
    
    
  })

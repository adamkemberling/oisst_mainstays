####
# Re-saving LME Shapes as named shapefiles in RES_Data
# 4/9/2021
# Adam A. Kemberling
# Purpose: got tired of looking up the names using a table,
# streamlined it and moved them to the shared folder



####  Libraries  ####
library(sf)
library(tidyverse)
library(gmRi)
library(patchwork)

# file paths
lme_path     <- shared.path("nsf okn", "large_marine_ecosystems")
lme_res_path <- shared.path("RES_Data", "Shapefiles/large_marine_ecosystems")

####  Data  ####

# Lookup table
lme_lookup <- read_csv(paste0(lme_path, "lme_names_key.csv"))
glimpse(lme_lookup)


# Shapefile Re-saving
lme_lookup %>% 
  split(.$lme_name) %>% 
  walk(function(lme_record){
    
    # pull components
    lme_name         <- lme_record$lme_name[1]
    lme_num          <- lme_record$lme_number[1]
    full_poly_file   <- lme_record$full_poly_file[1]
    poly_bound_file  <- lme_record$outer_bound_file[1]
    
    #prepare save name
    out_name  <- str_replace_all(lme_name, " ", "_")
    out_name  <- str_replace_all(out_name, "-", "_")
    out_name  <- str_replace_all(out_name, "[.]", "")
    out_name  <- str_replace_all(out_name, "___", "_")
    out_name  <- str_replace_all(out_name, "__", "_")
    save_name <- paste0(lme_res_path, out_name)
    save_name <- tolower(save_name)
    
    #check out names
    print(paste("Saving", lme_name))
    print(paste0("as ", save_name))
    
    # Load shapes:
    lme_full  <- st_read(paste0(lme_path, full_poly_file))
    lme_bound <- st_read(paste0(lme_path, poly_bound_file))
    
    
    # # Visual Check
    # full_p <- ggplot() +
    #   geom_sf(data = lme_full) +
    #   labs(title = lme_name)
    # 
    # bound_p <- ggplot() +
    #   geom_sf(data = lme_bound)
    # 
    # side_by_plot <- (full_p + bound_p)
    
    
    # # Save multi-polygon shape
    full_path <- paste0(save_name, "_full.geojson")
    st_write(obj = lme_full,
             dsn = full_path,
             driver = "GeoJSON")
    
    # # Save exterior without islands
    boundary_path <- paste0(save_name, "_exterior.geojson")
    st_write(obj = lme_bound,
             dsn = boundary_path,
             driver = "GeoJSON")
    
    
  })


# Make new lookup
new_lookup <- lme_lookup %>% 
  mutate(
    name_clean = tolower(lme_name),
    name_clean = str_replace_all(name_clean, " ", "_"),
    name_clean = str_replace_all(name_clean, "[.]", ""),
    name_clean = str_replace_all(name_clean, "-", "_"),
    name_clean = str_replace_all(name_clean, "___", "_"),
    name_clean = str_replace_all(name_clean, "__", "_"),
    full_poly_file = str_c(name_clean, "_full.geojson"),
    outer_bound_file = str_c(name_clean, "_exterior.geojson") ) %>% 
  select(lme_number, lme_name, name_clean, full_poly_file, outer_bound_file)


write_csv(new_lookup, paste0(lme_res_path, "lme_file_key.csv"))

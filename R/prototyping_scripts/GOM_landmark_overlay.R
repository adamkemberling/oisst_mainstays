# Making Georges Bank Relief for Maps



####  Libraries  ####
library(raster)
library(rnaturalearth)
library(metR)
library(sf)
library(stars)
library(gmRi)
library(patchwork)
library(tidyverse)

theme_set(theme_bw())


####  Shapefiles  ####

# File paths for various extents based on "apershing_gulf_of_maine"
region_paths <- get_timeseries_paths(region_group = "gmri_sst_focal_areas", 
                                     mac_os = "mojave")

# Polygon Path
poly_path <- region_paths[["apershing_gulf_of_maine"]][["shape_path"]]

#  Gulf of Maine
region_extent <- st_read(poly_path, quiet = TRUE)

# Load country borders
new_england_sf <- ne_states("united states of america", returnclass = "sf")
canada_sf      <- ne_states("canada", returnclass = "sf")
greenland_sf   <- ne_states(country = "greenland", returnclass = "sf")






####  Bathymetry  ####

# Add the bottom contours:
bathy <- raster("~/Documents/Repositories/Points_and_contours/NEShelf_Etopo1_bathy.tiff")

# Contours for geom_contour()
bathy_df <- as.data.frame(raster::coordinates(bathy))
bathy_df$depth <- raster::extract(bathy, bathy_df)
bathy_df$depth <- bathy_df$depth * -1
contours_make <- c(50, 100, 250)






####  Map 1: Contours w/ Labels  ####

# Pull extents for the region for crop
crop_x <- st_bbox(region_extent)[c(1,3)]
crop_y <- st_bbox(region_extent)[c(2,4)]

# Expand the area out to see the larger patterns
crop_x <- crop_x + c(-2.25, 2.25) 
crop_y <- crop_y + c(-0.75, 0.75)


####  Annotation Locations  ####
area_labs<- data.frame(
  "label" = c("Georges\nBank", 
              "Jordan\nBasin", 
              "Wilkinson\nBasin", 
              "Northeast\nChannel",
              "Scotian\nShelf",
              "Mid-Atlantic\nBight"),
  "lat"   = c(41.3, 
              43.5, 
              42.275, 
              42.25,
              43.7,
              39.8),
  "lon"   = c(-67.7, 
              -67.5, 
              -69.2, 
              -66.1,
              -63.5,
              -72.8),
  "angle" = c(0, 
              0, 
              0, 
              315,
              0,
              0)
)


# Full map of GOM
ggplot() +
  geom_sf(data = new_england_sf, fill = "gray90", size = .25) +
  geom_sf(data = canada_sf, fill = "gray90", size = .25) +
  geom_contour(data = bathy_df, aes(x, y, z = depth),
               breaks = contours_make,
               color = "gray80") +
  geom_sf(data = region_extent, 
          color = gmri_cols("gmri blue"), 
          fill = gmri_cols("gmri blue"), alpha = 0.2, linetype = 2, size = 0.5) +
  coord_sf(xlim = crop_x, 
           ylim = crop_y, expand = T) +
  geom_text(data = area_labs, aes(lon, lat, label = label, angle = angle), 
            size = 3, color = "black") +
  map_theme() +
  labs(title = "Annotated Regions")





####  Coloured Relief Setup  ####

# 1. Crop to speed things up/ only show contours where we care:

# First Mask using the polygon, rotate if needed
m1 <- mask(bathy, region_extent)
# Then Crop
m1 <- crop(m1, region_extent)


# 2.  Reclassify the contours




#' @title Reclassify Bathymetry Raster in Set Increments
#'
#' @param bathy_ras 
#' @param depth_increments 
#' @param min_elev 
#' @param max_elev 
#'
#' @return
#' @export
#'
#' @examples
reclassify_bathy <- function(bathy_ras = NULL, 
                             depth_increments = -200,
                             min_elev = -1000,
                             max_elev = 200,
                             include_lower = FALSE){
  
  # 1. create classification matrix
  # Gets converted to matrix: pattern is [left_lim:right_lim:label]
  
  # Get the left and right limits using increments
  l_lims <- seq(from = max_elev + depth_increments, to = min_elev, by = depth_increments)
  r_lims <- seq(from = max_elev, to = (min_elev - depth_increments), by = depth_increments)
  
  # Get the factor order that go with them (need numeric valus b/c matrix)
  lnums  <- as.numeric(fct_rev(as.factor(l_lims))) 
  
  # Build text labels as a key for viz later
  llabs  <- map2_chr(l_lims, r_lims, ~ str_c(.y, " to ", .x)) 
  
  # Make greater than if including lower than limit
  if(include_lower){
    llabs[length(llabs)] <- str_c("Greater than ", min_elev)  
  }
  
  # Build the dataframe that becomes matrix
  reclass_df <- data.frame("left" = l_lims, "right" = r_lims, "bin_num" = lnums)
  
  # reshape the df into a matrix with columns and rows
  reclass_m <- as.matrix(reclass_df)
  
  # Add labels to the df for exporting
  reclass_df$bin_labs <- llabs
  reclass_df <- mutate(reclass_df, bin_labs = fct_reorder(bin_labs, bin_num, min))
  
  # Truncate to limits of the range we want
  bathy_ras <- clamp(bathy_ras, min_elev, max_elev, useValues = TRUE)
  
  # 2. reclassify the raster using the reclass object - reclass_m
  bathy_classified <- reclassify(bathy_ras,
                                 reclass_m, 
                                 include.lowest = include_lower)
  
  # Return Both the Raster and its key
  return(
    list(
      "ras" = bathy_classified,
      "labs" = reclass_df))
}



# Reclassify to discrete
bathy_reclass <- reclassify_bathy(m1, depth_increments = -100, min_elev = -600, max_elev = 0)
bathy_reclass <- reclassify_bathy(bathy, depth_increments = -100, min_elev = -600, max_elev = 0)
reclass_ras <- bathy_reclass$ras
reclass_labs  <- bathy_reclass$labs


### Convert to stars/dataframe  ####
# FROM: https://bookdown.org/mcwimberly/gdswr-book/raster-data-discrete-variables.html

# Convert a raster to dataframe
rasterdf <- function(x, aggregate = 1) {
  resampleFactor <- aggregate
  inputRaster <- x
  inCols <- ncol(inputRaster)
  inRows <- nrow(inputRaster)
  # Compute numbers of columns and rows in the new raster for mapping
  resampledRaster <- raster(ncol=(inCols / resampleFactor),
                            nrow=(inRows / resampleFactor))
  # Match to the extent of the original raster
  extent(resampledRaster) <- extent(inputRaster)
  # Resample data on the new raster
  y <- resample(inputRaster,resampledRaster,method='ngb')

  # Extract cell coordinates
  coords <- xyFromCell(y, seq_len(ncell(y)))
  dat <- stack(as.data.frame(getValues(y)))
  
  # Add names - 'value' for data, 'variable' to indicate different raster layers
  # in a stack
  names(dat) <- c('value', 'variable')
  dat <- cbind(coords, dat)
  dat
}


# Convert to DF, add labels from reclassification ey
bathy_rclass_df <- rasterdf(reclass_ras) %>% 
  select(x, y, bin_num = value) %>% 
  left_join(reclass_labs, by = "bin_num")



####  Map Color Contours  ####

# Map colours with white labels
ggplot() +
  geom_raster(data = bathy_rclass_df, aes(x,y, fill = bin_labs)#, alpha = 0.9
              ) +
  scale_fill_brewer(palette = "Blues", 
                    # To remove NA's from legend:
                    # na.translate = FALSE, 
                    # To set their color for NA
                    # Get fill from brewer: RColorBrewer::brewer.pal(n = 6, "Blues")[6]
                    na.value = "#08519C",
                    labels = c(levels(bathy_df$bin_labs), "Greater than -600"),
                    name = "Depth")  +
  geom_sf(data = new_england_sf, fill = "gray90", size = .25) +
  geom_sf(data = canada_sf, fill = "gray90", size = .25) +
  geom_contour(data = bathy_df, aes(x, y, z = depth),
               breaks = contours_make,
               color = "gray80") +
  geom_sf(data = region_extent, 
          color = "gray10", 
          fill = "transparent", alpha = 0.2, linetype = 2, size = 0.5) +
  coord_sf(xlim = crop_x, 
           ylim = crop_y, expand = T) +
  geom_text(data = area_labs, aes(lon, lat, label = label, angle = angle), 
            size = 3, color = "black") +
  map_theme(legend.position = "none") +
  labs(title = "Colored Depth + Annotated Regions")

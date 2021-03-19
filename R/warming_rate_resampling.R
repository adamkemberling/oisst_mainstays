####  Comparing Warming Rates Through Resampling
####  3/2/2021
####  Objectives:
####  Build an array that allows you to resample a given pixel sized area with overflow around edges
####  Randomly select pixels and get area around that pixel to get warming rates


####  Building array

# basic idea is to mirror the warming rates raster so that:
# East and West continues to flow, so repeat original raster R:R:R
# For North South do the same, but invert
# If original is O and upside down is U, end with a 3x3 array (unit is original raster)
#
# U:U:U
# O:0:O
# U:U:U

# For resampling procedure randomly select pixel within 0. Then expend necessary directions around it
# If a certain amount of NA values exist then redraw
# If past a certain latitude throw a warning, possibly redraw
# should allow boxes to creep around map



####  Load Packages  ####
library(raster)
library(rnaturalearth)
library(sf)
library(stars)
library(gmRi)
library(here)
library(janitor)
library(patchwork)
library(tidyverse)

# Paths
box_paths <- research_access_paths()
res_path <- box_paths$res
oisst_path <- paste0(res_path, "OISST/oisst_mainstays/")

# Access information to warming rate netcdf files on box
rates_path <- paste0(oisst_path, "warming_rates/annual_warming_rates")

# Set theme up for maps
map_theme <- list(
  theme(
    panel.border       = element_rect(color = "black", fill = NA),
    plot.background    = element_rect(color = "transparent", fill = "transparent"),
    line               = element_blank(),
    axis.title.x       = element_blank(), # turn off titles
    axis.title.y       = element_blank(),
    legend.position    = "bottom", 
    legend.title.align = 0.5))

# World Poly for Plotting
world <- ne_countries() %>% st_as_sf(crs = 4326)

#  color palette for quick raster displays
temp_pal <- rev(RColorBrewer::brewer.pal(n = 10, name = "RdBu"))

####  Load Warming Rate Raster  ####

# 1982-2020
rates_stack_all <- stack(str_c(rates_path, "1982to2020.nc"), 
                         varname = "annual_warming_rate")
ranks_stack_all <- stack(str_c(rates_path, "1982to2020.nc"), 
                         varname = "rate_percentile")


# Plot one
plot(rates_stack_all, main = "Global Warming Rates", col = temp_pal)


####  Assemble Mosaic

# Pick center raster
center <- rates_stack_all

# Get Range of dims
wide <- dim(center)[1]
tall <- dim(center)[2]

# How big it will end up
out_wide <- wide * 3
out_tall <- tall * 3


# Build the empty canvas
canvas <- raster(nrows = out_tall, 
                 ncols = out_wide, 
                 xmn = -360, 
                 xmx = 720,
                 ymn = -270,
                 ymx = 270, resolution = c(0.25, 0.25),
                 vals = NA)


# Can you overlay with mosaic
test <- mosaic(canvas, center, fun = max)
plot(test, main = "Test Overlay", col = temp_pal)


#### Repeat on Sides ####

# If we make it a matrix it will be easier to
# use indices to subset the center, and
# essentially paste it and spin it as needed
test_mat <- as.matrix(center)

# convert
as(center, 'SpatialGridDataFrame')

dim(test_mat)











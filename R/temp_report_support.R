####
####  Temperature Repost Support
####  4/20/2021
####  Goal:
#### Move plotting and tidying steps here to reduce clutter and redundancy


# Packages
library(raster)
library(tidyverse)

# Unicode for degrees
deg_sym <- "\u00b0"
deg_c <- "\u00b0C"
deg_f <- "\u00b0F"


####  Theming  ####


# Set ggplot theme for figures
theme_set(theme_bw() + 
            theme(
              # Titles
              plot.title = element_text(hjust = 0, face = "bold", size = 14),
              plot.subtitle = element_text(size = 9),
              plot.caption = element_text(size = 8, margin = margin(t = 20), color = "gray40"),
              legend.title = element_text(size = 9),
              legend.text = element_text(size = 9),
              # Axes
              axis.line.y = element_line(color = "black"),
              axis.ticks.y = element_line(), 
              axis.line.x = element_line(color = "black"),
              axis.ticks.x = element_line(), 
              axis.text = element_text(size = 11),
              axis.title = element_text(size = 12),
              rect = element_rect(fill = "transparent", color = "black"),
              # Facets
              strip.text = element_text(color = "white", 
                                        face = "bold",
                                        size = 11),
              strip.background = element_rect(
                color = "#00736D", 
                fill = "#00736D", 
                size = 1, 
                linetype="solid")))



# Building a GMRI theme based on Wall street Journal and NYTimes theme
# base settings from {ggthemes}
theme_gmri <- function(base_size = 10, 
                       bg_color = "lightblue", 
                       base_family = "sans", 
                       title_family = "sans",
                       facet_color = "teal") {
  # Color from gmRi palette, sets background color
  #colorhex <- gmRi::gmri_cols()[bg_color]
  facet_hex <- gmri_cols()[facet_color]
  
  # Set up theme
  theme_foundation(
    base_size = base_size, 
    base_family = base_family) + 
    theme(
      
      # Major Elements
      line = element_line(linetype = 1, colour = "black"), 
      rect = element_rect(fill = "transparent", 
                          linetype = 0, 
                          colour = NA), 
      text = element_text(colour = "black"), 
      title = element_text(family = title_family, size = 12), 
      
      # Axis elements
      axis.text.x = element_text(colour = NULL), 
      axis.text.y = element_text(colour = NULL), 
      axis.ticks = element_line(colour = NULL), 
      axis.ticks.y = element_blank(), 
      axis.ticks.x = element_line(colour = NULL), 
      axis.line = element_line(), 
      axis.line.y = element_blank(), 
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12),
      
      # Legend Elements
      legend.background = element_rect(), 
      legend.position = "top", 
      legend.direction = "horizontal", 
      legend.box = "vertical", 
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9),
      
      # Panel/Grid Setup
      panel.grid = element_line(colour = NULL, linetype = 3, color = "gray80"), 
      panel.grid.major = element_line(colour = "black"), 
      panel.grid.major.x = element_blank(), 
      panel.grid.minor = element_blank(), 
      
      # Title and Caption Details
      plot.title = element_text(hjust = 0, face = "bold", size = 14),
      plot.subtitle = element_text(size = 9),
      plot.caption = element_text(size = 7.2, margin = margin(t = 20), color = "gray40"),
      plot.margin = unit(c(1, 1, 1, 1), "lines"), 
      
      # Facet Details
      strip.text = element_text(color = "white", face = "bold", size = 11),
      strip.background = element_rect(
        color = "white", 
        fill = facet_hex, 
        size = 1, 
        linetype="solid"))
}



# Set theme up for maps
map_theme <- list(
  theme(
    panel.border       = element_rect(color = "black", fill = NA),
    plot.background    = element_rect(color = "transparent", fill = "transparent"),
    line               = element_blank(),
    axis.title.x       = element_blank(), # turn off titles
    axis.title.y       = element_blank(),
    legend.title.align = 0.5))



####________####



# Load ERSSTv5
load_annual_ersst <- function(area = pershing_gom){
  
  # Updated 2022-02-4
  ersst_path <- cs_path("res", "ERSSTv5")
  
  
  # Timeseries for GOM
  ersst_tl <- read_csv(str_c(ersst_path, "ERSSTv5_anom_apershing_gulf_of_maine.csv"),
                       guess_max = 1e6, col_types = cols()) 
  
  # format
  ersst_tl <- ersst_tl %>% 
    rename(Date = time) %>% 
    mutate(yr = year(Date),
           sst_clim = sst - sst_anom,
           area_wtd_clim = area_wtd_sst - area_wtd_sst_anom)
  
  
  # Get Yearly Average
  ersst_yr <- ersst_tl %>% 
    group_by(yr) %>% 
    summarise(sst = mean(sst, na.rm = T), 
              sst_anom = mean(sst_anom, na.rm = T),
              sst_f = as_fahrenheit(sst, "temperature"),
              anom_f = as_fahrenheit(sst_anom, "anomalies"),
              .groups = "drop") %>% 
    mutate(yr = as.numeric(yr)) %>% 
    filter(yr <= 2021) 
  
  return(ersst_yr)
}


# Add different temporal columns using date column
supplement_season_info <- function(regional_timeseries){
  regional_timeseries %>% 
    mutate(
      time       = as.Date(time),
      yr         = year(time),
      year       = year(time),
      month_num  = month(time),
      month      = month(time, label = T, abbr = T),
      week       = lubridate::week(time),
      doy        = yday(time),
      season     = metR::season(month_num, lang = "en"),
      #Set up correct year for winters, they carry across into next year
      season_eng = case_when(
        season == "SON" ~ "Fall",
        season == "DJF" ~ "Winter",
        season == "MAM" ~ "Spring",
        season == "JJA" ~ "Summer"),
      season_yr = ifelse( (season_eng == "Winter" & month_num %in% c(1,2)), yr - 1, yr))
  
}


# Return Temperature and Anomaly info in both C and F
temperature_summaries <- function(x){
  x %>% 
    summarise(
      sst             = mean(sst, na.rm = T),
      sst_anom        = mean(sst_anom, na.rm = T), 
      sst_f           = as_fahrenheit(sst, "temperature"),
      anom_f          = as_fahrenheit(sst_anom, "anomalies"),
      area_wtd_sst    = mean(area_wtd_sst, na.rm = T),
      area_wtd_anom   = mean(area_wtd_anom, na.rm = T),
      area_wtd_f      = as_fahrenheit(area_wtd_sst, "temperature"),
      area_wtd_anom_f = as_fahrenheit(area_wtd_anom, "anomalies"),
      .groups         = "drop")
}



# Add Fahrenheit details and date info to heatwave outputs
supplement_hw_data <- function(region_hw_data){
  region_hw_data %>% 
    mutate(
      sst_f        = as_fahrenheit(sst, "temperature"),
      seas_f       = as_fahrenheit(seas, "temperature"),
      anom_f       = as_fahrenheit(sst_anom, "anomalies"),
      hwe_f        = as_fahrenheit(hwe, "temperature"),
      mhw_thresh_f = as_fahrenheit(mhw_thresh, "temperature"),
      time         = as.Date(time),
      yr           = year(time),
      month_num    = month(time),
      month        = month(time, label = T, abbr = T),
      week         = lubridate::week(time),
      doy          = yday(time),
      season       = metR::season(month_num, lang = "en"),
      season_eng   = case_when(
        season == "SON" ~ "Fall",
        season == "DJF" ~ "Winter",
        season == "MAM" ~ "Spring",
        season == "JJA" ~ "Summer"),
      #Set up correct year for winters, they carry across into next year
      season_yr = ifelse(
        (season_eng == "Winter" & month_num %in% c(1,2)), yr - 1, yr))
  
}






# This should all be a function:
get_decadal_rates <- function(temp_df, 
                              temp_col = "area_wtd_sst", 
                              year_col = "year",                              
                              year_lim = c(1982,2021), 
                              area_name = "GoM",
                              degree_c = T){
  # Filter years
  year_sym <- sym(year_col)
  temp_df_filtered <- dplyr::filter(temp_df, {{year_sym}} %in% seq(year_lim[1], year_lim[2], by = 1))
  
  
  # Do regression on years
  lm_form <- as.formula(paste0(temp_col, "~", year_col))
  annual_lm <- lm(formula = lm_form, 
                  data = temp_df_filtered) 
  
  # Pull Coefficients
  ann_coef <- annual_lm %>% 
    coef() %>% 
    round(3)
  
  # Multiply by 10 for the decadal change
  decade_rate <- ann_coef[year_col] * 10
  
  # Build out equation in english
  temp_unit <- ifelse(degree_c, "C", "F")
  eq_lab <- paste0(area_name, " ", year_lim[1], "-", year_lim[2], ": ", decade_rate, "\u00b0", temp_unit, " / Decade")
  
  
  # Generate a smoothed temperature line using splines
  yearly_temp_smooth <-  spline(x = temp_df_filtered %>% pull(year_col),
                                y = temp_df_filtered %>% pull(temp_col)) %>% 
    as.data.frame()
  
  
  # Spit out everything useful
  return_list <- list(
    "ann_lm"        = annual_lm,
    "decadal_rate"  = decade_rate,
    "eq_label"      = eq_lab,
    "yearly_smooth" = yearly_temp_smooth)
  return(return_list)

}
# End function





# Masking Function to clip to the study area
mask_nc <- function(ras_obj, mask_shape, rotate = TRUE){
  
  # First Mask using the polygon, rotate if needed
  if(rotate == TRUE){
    m1 <- mask(rotate(ras_obj), mask_shape)
  } else {
    m1 <- mask(ras_obj, mask_shape)
  }
  
  # Then Crop
  m1 <- crop(m1, mask_shape)
  return(m1)
}








# Get the average warming rates for each area
get_masked_vals <- function(masked_ranks, masked_rates, in_fahrenheit = F){
  
  # Get the average ranks
  m1 <- masked_ranks
  rank_mean <- cellStats(m1, mean)
  rank_min  <- cellStats(m1, min)
  rank_max  <- cellStats(m1, max)
  
  
  # Get stats from rates
  m2 <- masked_rates
  rate_mean <- cellStats(m2, mean)
  rate_min  <- cellStats(m2, min)
  rate_max  <- cellStats(m2, max)
  
  
  # Note the units
  deg_unit <- "C"
  
  
  # Convert to F
  if(in_fahrenheit == T){
    rate_mean <- as_fahrenheit(rate_mean, data_type = "anomalies")
    rate_min <- as_fahrenheit(rate_min, data_type = "anomalies")
    rate_max <- as_fahrenheit(rate_max, data_type = "anomalies")
    deg_unit <- "F"
  }
  
  
  
  
  # Put in table
  table_out <- tibble("Mean Rank" = rank_mean,
                      "Min Rank"  = rank_min,
                      "Max Rank"  = rank_max,
                      "Mean Rate" = rate_mean,
                      "Min Rate"  = rate_min,
                      "Max Rate"  = rate_max,
                      "Temp Unit" = deg_unit) %>% 
    mutate(across(where(is.numeric), round, 3))
  
  # spit them out
  return(table_out)
}





####________####


####  Belkin Oreailly Fronts  ####
# remotes::install_github("galuardi/boaR", 
#                         force = T, 
#                         build = T, 
#                         dependencies = "ask",
#                         upgrade = "ask")

get_belkin_fronts <- function(in_ras){
  
  library(boaR)
  
  
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

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
              plot.margin = unit(c(1, 1, 1, 1), "cm"),
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
                linewidth = 1, 
                linetype="solid")))



# # Building a GMRI theme based on Wall street Journal and NYTimes theme
# # base settings from {ggthemes}
# theme_gmri <- function(base_size = 10, 
#                        bg_color = "lightblue", 
#                        base_family = "sans", 
#                        title_family = "sans",
#                        facet_color = "teal") {
#   # Color from gmRi palette, sets background color
#   #colorhex <- gmRi::gmri_cols()[bg_color]
#   facet_hex <- gmri_cols()[facet_color]
#   
#   # Set up theme
#   theme_foundation(
#     base_size = base_size, 
#     base_family = base_family) + 
#     theme(
#       
#       # Major Elements
#       line = element_line(linetype = 1, colour = "black"), 
#       rect = element_rect(fill = "transparent", 
#                           linetype = 0, 
#                           colour = NA), 
#       text = element_text(colour = "black"), 
#       title = element_text(family = title_family, size = 12), 
#       
#       # Axis elements
#       axis.text.x = element_text(colour = NULL), 
#       axis.text.y = element_text(colour = NULL), 
#       axis.ticks = element_line(colour = NULL), 
#       axis.ticks.y = element_blank(), 
#       axis.ticks.x = element_line(colour = NULL), 
#       axis.line = element_line(), 
#       axis.line.y = element_blank(), 
#       axis.text = element_text(size = 11),
#       axis.title = element_text(size = 12),
#       
#       # Legend Elements
#       legend.background = element_rect(), 
#       legend.position = "top", 
#       legend.direction = "horizontal", 
#       legend.box = "vertical", 
#       legend.title = element_text(size = 9),
#       legend.text = element_text(size = 9),
#       
#       # Panel/Grid Setup
#       panel.grid = element_line(colour = NULL, linetype = 3, color = "gray80"), 
#       panel.grid.major = element_line(colour = "black"), 
#       panel.grid.major.x = element_blank(), 
#       panel.grid.minor = element_blank(), 
#       
#       # Title and Caption Details
#       plot.title = element_text(hjust = 0, face = "bold", size = 14),
#       plot.subtitle = element_text(size = 9),
#       plot.caption = element_text(size = 7.2, margin = margin(t = 20), color = "gray40"),
#       #plot.margin = unit(c(1, 1, 1, 1), "lines"), 
#       plot.margin = unit(c(1, 1, 2, 1), "lines"),
#       
#       # Facet Details
#       strip.text = element_text(color = "white", face = "bold", size = 11),
#       strip.background = element_rect(
#         color = "white", 
#         fill = facet_hex, 
#         size = 1, 
#         linetype="solid"))
# }



# # Set theme up for maps
# map_theme <- function(...){
#   list(
#     theme(
#       panel.border       = element_rect(color = "black", fill = NA),
#       plot.background    = element_rect(color = "transparent", fill = "transparent"),
#       line               = element_blank(),
#       axis.title.x       = element_blank(), # turn off titles
#       axis.title.y       = element_blank(),
#       legend.title.align = 0.5,
#       ...)
#   )
# }






# # Logo placement testing:
# # stackoverflow sources:
# # https://stackoverflow.com/questions/41919023/ggplot-adding-image-on-top-right-in-two-plots-with-different-scales

# 
# # Example Plot
# (test_fig <-  ggplot(mtcars, aes(mpg, cyl)) +
#   geom_point() + labs(title = "Tester ggplot"))


# Get x and y min/max coordinates using relative positions, returned as units from plotting scale:
get_relative_xy <- function(p, position = "bot_left", xmin = 0.01, ymin = -0.3, nudge_x = 0, nudge_y = 0){
  
  # Use switch to toggle some preset positions:
  position <- switch(position,
                     "bot_left"  = data.frame(xmin = -0.1, xmax = 0.25, ymin = -0.5, ymax = -0.325),
                     "bot_right" = data.frame(xmin = 0.725, xmax = 1.075, ymin = -0.5, ymax = -0.325),
                     "top_right" = data.frame(xmin = 0.725, xmax = 1.075, ymin = 0.8, ymax = 0.975),
                     "top_left"  = data.frame(xmin = -0.05, xmax = 0.3, ymin = 0.8, ymax = 0.975),
                     "manual"    = data.frame(xmin = xmin, xmax = xmax+.35, ymin = ymin, ymax = ymax +.175))
  
  
  # Get the x and y scale ranges
  # y-range, x_range:
  y_range <- layer_scales(p)$y$range$range
  x_range <- layer_scales(p)$x$range$range
  plot_dims <- data.frame(
    xmin = x_range[1],
    xmax = x_range[2],
    ymin = y_range[1],
    ymax = y_range[2])
  
  # Get ranges
  plot_dims <- plot_dims %>% 
    mutate(
      xrange = xmax - xmin,
      yrange = ymax - ymin)
  
  # Set the positions by working outward from x and y minima
  new_dims <- plot_dims %>% 
    mutate(
      new_xmin = xmin + (position$xmin * xrange) + (nudge_x * xrange),
      new_xmax = xmin + (position$xmax * xrange) + (nudge_x * xrange),
      new_ymin = ymin + (position$ymin * yrange) + (nudge_y * yrange),
      new_ymax = ymin + (position$ymax * yrange) + (nudge_y * yrange)) %>% 
    select(new_xmin, new_xmax, new_ymin, new_ymax)
  
  # Return relative positions
  return(new_dims)
}

# # Get min max in relative values
# (test_rel_dims <- get_relative_xy(test_fig, position = "bot_left"))


# Place GMRI logo using relative spacing for placement
add_gmri_logo <- function(p, position = "bot_left", relative_xy = NULL, nudge_x = 0, nudge_y = 0){
  
  # Get the positioning if it isn't provided
  if(is.null(relative_xy) == TRUE){
    relative_xy <- get_relative_xy(p, position = position, nudge_x = nudge_x,  nudge_y = nudge_y)  }
  
  # Path to logo from package
  logo_path <- paste0(system.file("stylesheets", package = "gmRi"), "/gmri_logo.png")
  
  # load a ggplot grob of the logo
  gmri_logo <- grid::rasterGrob(png::readPNG(logo_path))
  
  # Add and position the logo geom
  logo_grob <- list(
    annotation_custom(gmri_logo, 
                      xmin = relative_xy$new_xmin, 
                      xmax = relative_xy$new_xmax, 
                      ymin = relative_xy$new_ymin, 
                      ymax = relative_xy$new_ymax))
  
  if(str_detect(position, pattern = 'bot_')){
    # Turn off clipping outside the margins
    logo_grob[[3]] <- coord_cartesian(clip = "off")
    # Expand the margin at bottom so there is always room for it:
    logo_grob[[4]] <- theme(plot.margin = unit(c(1, 1, 2, 1), "lines"))
    
  }
    
  # Add to plot
  plot_out <- p + logo_grob
  return(plot_out)
  
}



# # # Test it (plots using units of the plot window...)
# Placing using relative position
# add_gmri_logo(test_fig, position = "top_right")
# 
# # Or Manual Placement
# test_fig +
#   geom_gmri_logo(relative_xy = get_relative_xy(test_fig, position = "manual", xmin = 0.75, xmax = 0.85, ymin = 0.75, ymax = 0.85))







####________####

####  Standard Data Processing  ####


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





# Get details around meteorological seasons for text editing:
season_text_details <- function(season_op, year_op){
  
  # Set color for the season based on the parameter:
  season_col <- switch(season_op,
                       "Spring" = gmri_cols("teal"),
                       "Summer" = gmri_cols("green"),
                       "Fall"   = gmri_cols("orange"),
                       "Winter" = "lightblue")
  
  # Configure edges/centers of each season for label positioning
  # Center Date for the 3-month period
  season_cent <- switch(
    EXPR = season_op,
    "Spring" = "-04-15",
    "Summer" = "-07-15",
    "Fall"   = "-10-15",
    "Winter" = "-01-15")
  
  # Left-hand date for polar plot geom
  season_left <- switch(
    EXPR = season_op,
    "Spring" = "-03-01",
    "Summer" = "-06-01",
    "Fall"   = "-09-01",
    "Winter" = "-12-01")
  
  # Right-hand date for polar plot geom
  season_right <- switch(
    season_op,
    "Spring" = "-05-31",
    "Summer" = "-08-31",
    "Fall"   = "-11-30",
    "Winter" = "-01-28")
  
  
  # Make names for the months to pull based on the season and current year
  season_months <- switch(
    EXPR = season_op,
    "Spring" = str_c("X", year_op, ".", c("03", "04", "05")),
    "Summer" = str_c("X", year_op, ".", c("06", "07", "08")),
    "Fall"   = str_c("X", year_op, ".", c("09", "10", "11")),
    "Winter" = map2(c(year_op-1, year_op, year_op), 
                    c("12","01","02"), ~ str_c("X", .x, ".", .y)))
  
  
  # Names of start and end month for text:
  season_range <- switch(
    EXPR = season_op,
    "Spring" = "March - May",
    "Summer" = "June - August",
    "Fall"   = "September - November",
    "Winter" = "December - February")
  
  
  # Season End cutoff using day of the year
  season_end <- switch(
    EXPR = season_op,
    "Winter" = yday("2000-02-28"),
    "Spring" = yday("2000-05-31"),
    "Summer" = yday("2000-08-31"),
    "Fall"   = yday("2000-11-30")
  )
  
  
  # Make a list:
  season_details <- list(
    month_range    = season_months,
    left_lim       = season_left,
    center_date    = season_cent,
    right_lim      = season_right,
    range_label    = season_range,
    season_color   = season_col,
    season_end_doy = season_end
    )
  
  # Return the lists
  return(season_details)
  
  
  
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





# Make a box to use when cropping based on an xlim and ylim pair
make_cropbox <- function(xlims, ylims){
  sfc <- st_sfc(st_polygon(list(
    rbind(c(xlims[[1]], ylims[[1]]),  
          c(xlims[[1]], ylims[[2]]), 
          c(xlims[[2]], ylims[[2]]), 
          c(xlims[[2]], ylims[[1]]), 
          c(xlims[[1]], ylims[[1]])))))
  sfc <- st_as_sf(sfc)
  return(sfc)
}







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




# Convert a raster to dataframe
rasterdf <- function(x, aggregate = 1) {
  
  # FROM: https://bookdown.org/mcwimberly/gdswr-book/raster-data-discrete-variables.html
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


####____####

####  Specific/Standard  Plots  ####




# Function to map  Gulf of Maine, with region of interest overlaid
#' @title Minimal Map of Study area
#'
#' @param region_extent 
#' @param x_buffer 
#' @param y_buffer 
#' @param new_england_sf 
#' @param canada_sf 
#' @param greenland_sf 
#' @param area_labs 
#' @param plot_title 
#' @param shape_linetype 
#'
#' @return
#' @export
#'
#' @examples
map_study_area <- function(region_extent,
                           x_buffer =  c(-2.25, 2.25),
                           y_buffer = c(-0.75, 0.75),
                           new_england_sf = NULL,
                           canada_sf = NULL,
                           greenland_sf = NULL,
                           area_labs = NULL,
                           plot_title = NULL,
                           shape_linetype = 2){
  
  
  # Load country borders if NULL
  if(is.null(new_england_sf)){
    new_england_sf <- ne_states("united states of america", returnclass = "sf")
  }
  if(is.null(canada_sf)){
    canada_sf      <- ne_states("canada", returnclass = "sf")
  }
  if(is.null(greenland_sf)){
    greenland_sf   <- ne_states(country = "greenland", returnclass = "sf")
  }
  if(is.null(area_labs)){
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
                  0,
                  0,
                  0)
    )
  }
  
  
  # Pull extents for the region for crop
  crop_x <- st_bbox(region_extent)[c(1,3)]
  crop_y <- st_bbox(region_extent)[c(2,4)]
  
  # Expand the area out to see the larger patterns
  crop_x <- crop_x + x_buffer
  crop_y <- crop_y + y_buffer
  
  #
  # Add the bottom contours:
  bathy <- raster("~/Documents/Repositories/Points_and_contours/NEShelf_Etopo1_bathy.tiff")
  
  # Contours for geom_contour()
  bathy_df <- as.data.frame(raster::coordinates(bathy))
  bathy_df$depth <- raster::extract(bathy, bathy_df)
  bathy_df$depth <- bathy_df$depth * -1
  contours_make <- c(50, 100, 250)
  
  
  # Full map of GOM
  gom_extent_p <- ggplot() +
    geom_sf(data = new_england, 
            fill = "gray90", 
            linewidth = .25) +
    geom_sf(data = canada, 
            fill = "gray90", 
            linewidth = .25) +
    geom_sf(data = greenland, 
            fill = "gray90", 
            linewidth = .25) +
    geom_contour(data = bathy_df, aes(x, y, z = depth),
                 breaks = contours_make,
                 color = "gray80") +
    geom_sf(data = region_extent, 
            color = gmri_cols("gmri blue"), 
            fill = gmri_cols("gmri blue"), 
            alpha = 0.2, 
            linetype = shape_linetype, 
            linewidth = 0.5) +
    geom_text(data = area_labs, aes(lon, lat, label = label, angle = angle), 
              size = 3, color = "black") +
    coord_sf(xlim = crop_x, 
             ylim = crop_y, 
             expand = T) +
    map_theme() +
    labs(title = plot_title)
  
  
  # Show figure
  return(gom_extent_p)
  
}






#' @title Colored Map of Study area
#'
#' @param region_extent 
#' @param x_buffer 
#' @param y_buffer 
#' @param new_england_sf 
#' @param canada_sf 
#' @param greenland_sf 
#' @param plot_title 
#' @param area_labs 
#' @param shape_linetype 
#'
#' @return
#' @export
#'
#' @examples
map_study_area_color <- function(region_extent,
                                 x_buffer =  c(-2.25, 2.25),
                                 y_buffer = c(-0.75, 0.75),
                                 new_england_sf = NULL,
                                 canada_sf = NULL,
                                 greenland_sf = NULL,
                                 plot_title = NULL,
                                 area_labs = NULL,
                                 shape_linetype = 2){
  
  
  
    
    
    # Load country borders if NULL
    if(is.null(new_england_sf)){
      new_england_sf <- ne_states("united states of america", returnclass = "sf")
    }
    if(is.null(canada_sf)){
      canada_sf      <- ne_states("canada", returnclass = "sf")
    }
    if(is.null(greenland_sf)){
      greenland_sf   <- ne_states(country = "greenland", returnclass = "sf")
    }
    if(is.null(area_labs)){
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
                    0, #315,
                    0,
                    0)
      )
    }
    
    
    # Pull extents for the region for crop
    crop_x <- st_bbox(region_extent)[c(1,3)]
    crop_y <- st_bbox(region_extent)[c(2,4)]
    
    # Expand the area out to see the larger patterns
    crop_x <- crop_x + x_buffer
    crop_y <- crop_y + y_buffer
    
    #
    # Add the bottom contours:
    bathy <- raster("~/Documents/Repositories/Points_and_contours/NEShelf_Etopo1_bathy.tiff")
    
    # # Contours for geom_contour()
    # bathy_df <- as.data.frame(raster::coordinates(bathy))
    # bathy_df$depth <- raster::extract(bathy, bathy_df)
    # bathy_df$depth <- bathy_df$depth * -1
    # contours_make <- c(50, 100, 250)
    
    # Add the bottom contours as color
    # Reclassify to discrete
    bathy_reclass <- reclassify_bathy(bathy, 
                                      depth_increments = -100, 
                                      min_elev = -600, 
                                      max_elev = 0)
    reclass_ras <- bathy_reclass$ras
    reclass_labs  <- bathy_reclass$labs
    
    
    # Convert to DF, add labels from reclassification ey
    bathy_rclass_df <- rasterdf(reclass_ras) %>% 
      select(x, y, bin_num = value) %>% 
      left_join(reclass_labs, by = "bin_num")
    
    
    
    # Full map of GOM
    gom_extent_p <- ggplot() +
      geom_raster(data = bathy_rclass_df, aes(x,y, fill = bin_labs), alpha = 0.9) +
      scale_fill_brewer(palette = "Blues", 
                        # To remove NA's from legend:
                        # na.translate = FALSE, 
                        # To set their color for NA
                        # Get fill from brewer: RColorBrewer::brewer.pal(n = 6, "Blues")[6]
                        na.value = "#08519C",
                        labels = c(levels(bathy_rclass_df$bin_labs), "Greater than -600"),
                        name = "Depth")  +
      geom_sf(data = new_england, 
              fill = "gray90", 
              linewidth = .25) +
      geom_sf(data = canada, 
              fill = "gray90", 
              linewidth = .25) +
      geom_sf(data = greenland, 
              fill = "gray90", 
              linewidth = .25) +
      # geom_contour(data = bathy_df, aes(x, y, z = depth),
      #              breaks = contours_make,
      #              color = "gray80") +
      geom_text(data = area_labs, aes(lon, lat, label = label, angle = angle), 
                size = 3, color = "black") +
      geom_sf(data = region_extent, 
              color = "gray10", 
              fill = "transparent", alpha = 0.2, 
              linetype = shape_linetype, 
              linewidth = 0.5) +
      coord_sf(xlim = crop_x, 
               ylim = crop_y, 
               expand = T) +
      map_theme(legend.position = "none") +
      labs(title = plot_title,
           caption = "Depth contours colored at 100m intervals to a maximum of 600m")
    
    
    # Show figure
    return(gom_extent_p)
  
}




#' @title Plot Meteorological Seasons
#'
#' @param season_highlight Name of season to have highlighted. 
#' Moves it out in the ring and adds a contrast color.
#'
#' @return
#' @export
#'
#' @examples
plot_met_seasons <- function(season_highlight){
  
  # make a dataframe of where the seasons align
  met_seasons_df <- data.frame(
    startdate  = as.Date(c("2016-03-03", "2016-06-03", "2016-09-03", "2015-12-02=3")),
    labeldate = as.Date(c("2016-04-16", "2016-07-16", "2016-10-16", "2016-01-16")),
    finishdate = as.Date(c("2016-05-29", "2016-08-29", "2016-11-28", "2016-02-28")),
    season = c("Spring", "Summer", "Fall", "Winter"),
    y = rep(5,4)) %>% 
    mutate(y_orig = y,
           y = ifelse(season == season_highlight, y + 0.5, y),
           y_high = y + 1.5,
           label_y = y_orig + 4,
           current_season = ifelse(season == season_highlight, season_col, "gray70"))
  
  
  
  
  # Plot
  ggplot(met_seasons_df) +
    geom_rect(aes(xmin = startdate, 
                  xmax = finishdate,
                  ymin = y,
                  ymax = y_high),
              fill = met_seasons_df$current_season) +
    geom_textpath(aes(x = labeldate, y = label_y, label = season), 
                  color = met_seasons_df$current_season, size = 6) +
    scale_x_date(date_breaks = "1 month", labels = date_format("%b"), 
                 limits = range(as.Date(c("2015-12-01", "2016-11-30")))) +
    ylim(0,13) +
    coord_curvedpolar() +
    labs(title = "Meteorological Seasons", 
         x = "", y = "") +
    theme(panel.background = element_blank(),
          plot.title = element_text(vjust = 4),
          axis.text.x = element_text(size = 16),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          title = element_text(size = 14, face = "bold"),
          plot.margin = unit(c(0.5,1,0,1), "cm"))
}



###  ERSST  ####



ersst_yearly_plot <- function(ersst_yr, temp_units){
  
  # Handling Temperature Units:
  temp_ops <- switch (EXPR = temp_units,
                      "C" = list(temp_col = expr(sst),
                                 temp_suff = " \u00b0C"),
                      "F" = list(temp_col = expr(sst_f),
                                 temp_suff = " \u00b0F")
  )
  temp_col <- temp_ops$temp_col
  
  
  
  # Benchmark the averages temp and maximum temps
  long_avg <- mean( filter(ersst_yr, between(yr, 1860, 2000)) %>% pull(sst) , na.rm = T)
  long_max <- max( filter(ersst_yr, between(yr, 1860, 2000)) %>% pull(sst) , na.rm = T)
  
  # Make Splines
  ersst_smooth <- as.data.frame(spline(ersst_yr$yr, ersst_yr$sst))
  ersst_smooth_f <- as.data.frame(spline(ersst_yr$yr, ersst_yr$sst_f))
  
  # switch units
  smooth_dat <- ifelse(temp_units == "F", ersst_smooth_f, ersst_smooth)
  
  
  # Fahrenheit Plot
  ersst_long_term_p <- ggplot(data = ersst_yr, aes(yr, {{temp_col}})) +
    geom_line(group = 1, alpha = 0.4, linetype = 1, color = gmri_cols("gmri blue")) +
    geom_point(size = 1, color = gmri_cols("gmri blue")) +
    geom_point(data = filter(ersst_yr, sst > long_max), 
               aes(yr, {{temp_col}}), color = "darkred", size = 1.5) +
    geom_mark_ellipse(aes(yr, {{temp_col}}, 
                          filter = sst > long_max,
                          description = "Temperatures in 6 of the 10 Last Years Above 1951 Record",
                          label = "Recent Extremes"), label.fontsize = 9,
                      color = "darkred", label.fill = "white", label.colour = "black", con.colour = "black") +
    geom_mark_ellipse(aes(yr, {{temp_col}}, 
                          filter = sst == long_max, 
                          label = str_c(yr,": ", round({{temp_col}}, 2), temp_ops$temp_suff), 
                          description = "1850-2000 Warmest Temperature on Record"), 
                      label.fontsize = 9,  label.buffer = unit(0.25, "lines"),
                      color = "darkred", linetype = 2) +
    scale_color_gmri() +
    theme_gmri() +
    scale_x_continuous(expand = expansion(add = c(2,10))) +
    scale_y_continuous(expand = expansion(add = c(0.5,5)),
                       labels = number_format(suffix = temp_ops$temp_suff)) +
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5))  +
    theme(legend.position = "bottom") +
    labs(x = "", y = "Sea Surface Temperature", 
         color = "Temperature Data Record",
         title = "Long-Term Temperature Record for the Gulf of Maine",
         subtitle = "Current temperatures above 150-year highs",
         caption = "Data Source: ERSSTv5 Monthly Sea Surface Temperature")
  
  
  # return plot
  return(ersst_long_term_p)
  
  
}




####  OISST  ####



#' @title Annotated Annual Averages + Trendline
#'
#' @param annual_avgs 
#' @param all_years_average 
#' @param rate_data 
#' @param focal_yr 
#' @param temp_units 
#'
#' @return
#' @export
#'
#' @examples
annual_avgs_annotated <- function(annual_avgs, all_years_average, rate_data, focal_yr, temp_units){
  
  # Build label for focal year
  anom_2021 <- annual_summary %>% filter(year == focal_yr) %>% 
    select(area_wtd_anom, area_wtd_anom_f)
  anom_label <- str_c(round(anom_2021[1], 2), " \u00b0C  |  ", round(anom_2021[2], 2), " \u00b0F")
  
  
  # Swap columns for units
  temp_ops <- switch(
    EXPR = temp_units,
    "C" = list(
      temp_col = expr(area_wtd_sst),
      anom_col = expr(area_wtd_anom),
      all_yr_temp = all_years_average[["avg_temp_c"]],
      all_yr_anom = all_years_average[["avg_anom_c"]],
      temp_suff = deg_c),
    "F" = list(
      temp_col = expr(area_wtd_f),
      anom_col = expr(area_wtd_anom_f),
      all_yr_temp = all_years_average[["avg_temp_f"]],
      all_yr_anom = all_years_average[["avg_anom_f"]],
      temp_suff = deg_f))
  
  # Assign to shorter names
  temp_col <- temp_ops$temp_col
  anom_col <- temp_ops$anom_col
  temp_suff <- temp_ops$temp_suff
  
  #
  
  
  
  # Assemble plot highlighting 2021 characteristics
  annual_temps_plot <- ggplot(annual_avgs, aes(year, {{ temp_col }})) +
    geom_hline(yintercept = temp_ops$all_yr_temp, color = "darkblue", linetype = 2) +
    geom_line(linetype = 3, size = 0.5, alpha = 0.5) +
    geom_smooth(formula = y ~ x, 
                linetype = 1,
                method = "lm", 
                color = "darkred", 
                se = FALSE) +
    geom_point(color = "gray20", size = 1, alpha = 0.8) +
    geom_mark_ellipse(
      data = data.frame(x = 2016, y = temp_ops$all_yr_temp),
      aes(x, y, label = all_year_avgs$temp_label), 
      description = "1982-2021 Average Temperature", 
      label.colour = "darkblue",
      color = "transparent", 
      con.colour = "darkblue", 
      label.fill = "transparent", 
      label.fontsize = 9, 
      label.buffer = unit(0.25, "cm")) +
    geom_mark_ellipse(
      aes(filter = year == 1994, 
          label = str_c(rate_data$`GoM All F`$decadal_rate, " \u00b0F / Decade")), 
      color = "transparent", 
      description = str_c("40-Year Warming Trend"), 
      label.colour = "darkred",
      con.colour =  "darkred", 
      label.buffer = unit(1.35, "cm"), 
      label.fill = "transparent", 
      label.fontsize = 9) +
    geom_mark_ellipse(
      aes(filter = year == focal_yr, 
          label = anom_label),
      color = "darkred",
      linetype = 2,
      description = str_c("Temperature Above\n1982-2011 climatological average"), 
      label.colour = "darkred",
      con.colour =  "darkred",
      label.fill = "transparent", 
      label.buffer = unit(0.15, "cm"), 
      label.fontsize = 9) +
    scale_y_continuous(limits = c(48, 56),
                       labels =  number_format(suffix = str_c(" ", temp_suff))) +
    labs(x = "Year", y = "Average Annual Temperature")
  
  
  # Display plot
  return(annual_temps_plot)
  
  
  
}




#' @title Plot Annual Temperature Comparison Vs. Global Rate
#'
#' @param annual_summary_dat Annual mean temperatures for a region of interest
#' @param global_summary_dat Annual mean temperatures for the world
#' @param eq_all Text label describing the region and warming rate information
#' @param eq_global Global counterpart to eq_all
#' @param temp_units C or F to indicate display units
#' @param String to replace "GoM" when using a region other than Gulf of Maine, Sets the plot title as well
#'
#' @return
#' @export
#'
#' @examples
global_rate_comparison <- function(
    annual_summary_dat,
    global_summary_dat,
    eq_all,
    eq_global,
    temp_units = "F",
    region_label = "Gulf of Maine"
  ){
  
  # Change Region Label:
  eq_all <- str_replace(eq_all, "GoM", region_label)
  
  
  # Handling Temperature Units:
  temp_ops <- switch (EXPR = temp_units,
    "C" = list(temp_col = expr(area_wtd_anom),
               temp_suff = " \u00b0C"),
    "F" = list(temp_col = expr(area_wtd_anom_f),
               temp_suff = " \u00b0F")
  )
  temp_col <- temp_ops$temp_col
  
  # Adding warming rate equation to data
  annual_summary_dat <- annual_summary_dat %>% 
    mutate(`Warming Rate` = eq_all)
  global_summary_dat <- global_summary_dat %>% 
    mutate(`Warming Rate` = eq_global)
  
  
  # Line color formatting, to match equations
  line_colors <- setNames(
    as.character(c(
      gmri_cols("gmri blue"), 
      gmri_cols("gmri green"))),  
    c(eq_all, eq_global))
  
  
  
  # Single Line Plot
  temp_simplified <- ggplot(data = annual_summary_dat, 
                            aes(year, {{ temp_col }})) +
    
    # Overlay yearly means
    geom_line(color = "gray10", size = 1.5, linetype = 1) +
    geom_point(color = "gray10", size = 1.5) +
    # Warming rates
    geom_smooth(
      method = "lm",
      formula = y ~ x, se = F,
      aes(color = `Warming Rate`),
      color = gmri_cols("gmri blue"),
      alpha = 0.90,
      size = 1.5,
      linetype = 2) + 
    geom_smooth(
      data = global_summary_dat,
      aes(color = `Warming Rate`),
      method = "lm",
      formula = y ~ x, se = F,
      alpha = 0.90,
      size = 1.5,
      linetype = 2) +
    scale_color_manual(values = line_colors) +
    scale_x_continuous(limits = c(1982, 2021), expand = expansion(add = c(4,2))) +
    scale_y_continuous(labels =  number_format(suffix = temp_ops$temp_suff)) +
    labs(title = str_c(region_label, ":"),
         subtitle = "Annual Sea Surface Temperature Anomalies",
         x = "Year", y = "Sea Surface Temperature Anomaly",
         caption = "Anomalies calculated using 1982-2011 reference period.") +
    theme(legend.position = c(0.25, 0.85),
          legend.background = element_rect(color = "black", fill = "white"))
  
  
  # add logo
  # temp_simplified <- add_gmri_logo(temp_simplified, position = "bot_left")
  temp_simplified
  
  
  
  
  
  
  
}






#' @title Plot Heatwave Status Trend for Last Year
#'
#' @param year_hw_dat Heatwave status information with hwe details
#' @param temp_units Flag for "C" or "F" for unit swapping
#'
#' @return
#' @export
#'
#' @examples
year_hw_temps <- function(
    year_hw_dat,
    temp_units = "F"){
  
  
  # Handling Temperature Units:
  temp_ops <- switch (EXPR = temp_units,
                      "C" = list(temp_col = expr(sst),
                                 anom_col = expr(sst_anom),
                                 seas_col = expr(seas),
                                 hw_thresh = expr(mhw_thresh),
                                 hw_temp = expr(hwe),
                                 temp_suff = " \u00b0C"),
                      "F" = list(temp_col = expr(sst_f),
                                 anom_col = expr(anom_f),
                                 seas_col = expr(seas_f),
                                 hw_thresh = expr(mhw_thresh_f),
                                 hw_temp = expr(hwe_f),
                                 temp_suff = " \u00b0F"))
  
  
  # Assign to shorter names
  temp_col <- temp_ops$temp_col
  anom_col <- temp_ops$anom_col
  clim_col <- temp_ops$seas_col
  hw_thresh_col <- temp_ops$hw_thresh
  hw_temp_col <- temp_ops$hw_temp
  
  
  # Set colors by name
  color_vals <- c(
    "Sea Surface Temperature" = "#0571B0",
    "Heatwave Event"          = "darkred",
    "MHW Threshold"           = "coral3",
    "Daily Climatology"       = "gray30")
  
  # Set linetypes manually
  linetype_key <- c(
    "Sea Surface Temperature Anomaly" = 1,
    "Heatwave Event"                  = 1,
    "MHW Threshold"                   = 3,
    "Daily Climatology"               = 2)
  
  
  
  # Plot the last 365 days
  hw_temp_p <- ggplot(year_hw_dat, aes(x = time)) +
    geom_segment(aes(x = time, 
                     xend = time, 
                     y = {{ clim_col }}, 
                     yend = {{ temp_col }}), 
                 color = "#0571B0", alpha = 0.25) +
    geom_segment(aes(x = time, 
                     xend = time, 
                     y = {{ hw_thresh_col }}, 
                     yend = {{ hw_temp_col }}), 
                 color = "darkred", alpha = 0.25) +
    geom_line(aes(y = {{ temp_col }}, color = "Sea Surface Temperature")) +
    geom_line(aes(y = {{ hw_temp_col }}, color = "Heatwave Event")) +
    geom_line(aes(y = {{ hw_thresh_col }}, color = "MHW Threshold"), lty = 3, size = .5) +
    geom_textpath(aes(y = {{ clim_col }}), color = "gray30", label = "Climatological Mean", hjust = 0.5, lty = 2) +
    scale_color_manual(values = color_vals) +
    #scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = expansion(mult = c(0,0))) +
    scale_x_date(date_labels = "%b %y", date_breaks = "1 month", expand = expansion(mult = c(0,0))) +
    scale_y_continuous(labels =  number_format(suffix = temp_ops$temp_suff)) +
    guides(color = guide_legend(override.aes = list(linetype = linetype_key), nrow = 2)) +
    theme(legend.title = element_blank(),
          legend.position = "bottom", legend.title = element_text(face = "bold")) +
    labs(x = NULL, 
         y = "Sea Surface Temperature")
  
  
  # Plot with logo
  # add_gmri_logo(hw_temp_p, position = "top_left")
  return(hw_temp_p)
  
  
}




#' @title Plot Heatwave Status Trend of Anomalies for Last Year
#'
#' @param year_hw_dat 
#' @param temp_units 
#'
#' @return
#' @export
#'
#' @examples
year_hw_anoms <- function(year_hw_dat = this_yr, temp_units = "F"){
  
  # Handling Temperature Units:
  temp_ops <- switch (EXPR = temp_units,
                      "C" = list(temp_col = expr(sst),
                                 anom_col = expr(sst_anom),
                                 seas_col = expr(seas),
                                 hw_thresh = expr(mhw_thresh),
                                 hw_temp = expr(hwe),
                                 temp_suff = " \u00b0C"),
                      "F" = list(temp_col = expr(sst_f),
                                 anom_col = expr(anom_f),
                                 seas_col = expr(seas_f),
                                 hw_thresh = expr(mhw_thresh_f),
                                 hw_temp = expr(hwe_f),
                                 temp_suff = " \u00b0F"))
  
  
  # Assign to shorter names
  temp_col <- temp_ops$temp_col
  anom_col <- temp_ops$anom_col
  clim_col <- temp_ops$seas_col
  hw_thresh_col <- temp_ops$hw_thresh
  hw_temp_col <- temp_ops$hw_temp
  
  # Set colors by name
  color_vals <- c(
    "Sea Surface Temperature" = "#0571B0",
    "Heatwave Event"          = "darkred",
    "MHW Threshold"           = "coral3",
    "Daily Climatology"       = "gray30")
  
  # Set linetypes manually
  linetype_key <- c(
    "Sea Surface Temperature Anomaly" = 1,
    "Heatwave Event"                  = 1,
    "MHW Threshold"                   = 3,
    "Daily Climatology"               = 2)
  
  
  # Plot the last 365 days - anomaly scale
  hw_anom_p <- year_hw_dat %>% 
    mutate(
      anom_thresh = {{ hw_thresh_col }} - {{ clim_col }},
      anom_hwe = {{ hw_temp_col }} - {{ clim_col }}) %>% 
    ggplot(aes(x = time)) +
    geom_segment(aes(x = time, xend = time, 
                     y = 0, yend = {{ anom_col }}), 
                 color = "#0571B0", alpha = 0.25) +
    geom_segment(aes(x = time, 
                     xend = time, 
                     y = anom_thresh, 
                     yend = anom_hwe), 
                 color = "darkred", alpha = 0.25) +
    geom_line(aes(y = {{ anom_col }}, color = "Sea Surface Temperature Anomaly")) +
    geom_line(aes(y = anom_hwe, color = "Heatwave Event")) +
    geom_line(aes(y = anom_thresh, color = "MHW Threshold"), lty = 3, size = .5) +
    geom_line(aes(y = 0, color = "Daily Climatology"), lty = 2, size = .5) +
    scale_color_manual(values = color_vals) +
    scale_linetype_manual(values = linetype_key, guide = "none") +
    # scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = expansion(mult = c(0,0))) +
    scale_x_date(date_labels = "%b %y", date_breaks = "1 month", expand = expansion(mult = c(0,0))) +
    guides(color = guide_legend(override.aes = list(linetype = linetype_key), nrow = 2)) +
    theme(#legend.title = element_blank(),
          legend.position = "bottom") +
    scale_y_continuous(labels =  number_format(suffix = temp_ops$temp_suff)) +
    labs(x = NULL, 
         y = "Temperature Anomaly", 
         caption = paste0("Climate reference period :  1982-2011"))
  
  # Plot with logo
  return(hw_anom_p)
  
}






#' @title Plot Heatwave Status Trend for Last Year
#'
#' @param year_hw_dat Heatwave status information with hwe details
#' @param temp_units Flag for "C" or "F" for unit swapping
#'
#' @return
#' @export
#'
#' @examples
year_hw_temps_two <- function(
    year_hw_dat,
    temp_units = "F"){
  
  
  # Augment Data
  year_hw_dat <- mutate(year_hw_dat, 
                        cs_thresh_f = as_fahrenheit(mcs_thresh),
                        mhw_event_txt = if_else(mhw_event, "Marine Heatwave", "Non-Event"))
  
  
  # Handling Temperature Units:
  temp_ops <- switch (EXPR = temp_units,
                      "C" = list(temp_col = expr(sst),
                                 anom_col = expr(sst_anom),
                                 seas_col = expr(seas),
                                 hw_thresh = expr(mhw_thresh),
                                 cs_thresh = expr(mcs_thresh),
                                 hw_temp = expr(hwe),
                                 temp_suff = " \u00b0C"),
                      "F" = list(temp_col = expr(sst_f),
                                 anom_col = expr(anom_f),
                                 seas_col = expr(seas_f),
                                 hw_thresh = expr(mhw_thresh_f),
                                 cs_thresh = expr(cs_thresh_f),
                                 hw_temp = expr(hwe_f),
                                 temp_suff = " \u00b0F"))
  
  
  # Assign to shorter names
  temp_col <- temp_ops$temp_col
  anom_col <- temp_ops$anom_col
  clim_col <- temp_ops$seas_col
  hw_thresh_col <- temp_ops$hw_thresh
  cs_thresh_col <- temp_ops$cs_thresh
  hw_temp_col <- temp_ops$hw_temp
  
  
  # Set colors by name
  color_vals <- c(
    "Non-Event"       = "#0571B0",
    "Marine Heatwave" = "darkred")
  
  
  
  # Build the Plot
  hw_p <- ggplot(year_hw_dat) +
    geom_path(aes(x = time, y = {{temp_col}}, color = mhw_event_txt, group = 1)) +
    geom_segment(aes(x = time, xend = time, y = {{clim_col}}, yend = {{temp_col}}, color = mhw_event_txt), alpha = 0.25) +
    geom_textpath(aes(x = time, y = {{hw_thresh_col}}), color = "gray10", label = "Heatwave Threshold", hjust = .05, lty = 1 ) + 
    geom_textpath(aes(x = time, y = {{clim_col}}), color = "gray10", label = "Climatological Mean", hjust = 0.5, lty = 1) +
    geom_textpath(aes(x = time, y = {{cs_thresh_col}}), color = "gray10", label = "Cold Spell Threshold", hjust = 0.95, lty = 1) +
    scale_color_manual(values = color_vals) +
    scale_x_date(date_labels = "%b %y", 
                 date_breaks = "1 month", 
                 expand = expansion(mult = c(0,0))) +
    scale_y_continuous(labels =  number_format(suffix = temp_ops$temp_suff)) +
    theme(legend.position = "bottom", legend.title = element_text(face = "bold")) +
    labs(x = NULL, 
         color = "Daily Temperature Heatwave Status:",
         y = "Sea Surface Temperature")
  
  return(hw_p)
  
  
}



#' @title Plot Heatwave Status Trend of Anomalies for Last Year
#'
#' @param year_hw_dat Heatwave status information with hwe details
#' @param temp_units Flag for "C" or "F" for unit swapping
#'
#' @return
#' @export
#'
#' @examples
year_hw_anoms_two <- function(
    year_hw_dat,
    temp_units = "F"){
  
  
  # Augment Data
  year_hw_dat <- mutate(year_hw_dat, 
                        cs_thresh_f = as_fahrenheit(mcs_thresh),
                        mhw_event_txt = if_else(mhw_event, "Marine Heatwave", "Non-Event"))
  
  
  # Handling Temperature Units:
  temp_ops <- switch (EXPR = temp_units,
                      "C" = list(temp_col = expr(sst),
                                 anom_col = expr(sst_anom),
                                 seas_col = expr(seas),
                                 hw_thresh = expr(mhw_thresh),
                                 cs_thresh = expr(mcs_thresh),
                                 hw_temp = expr(hwe),
                                 temp_suff = " \u00b0C"),
                      "F" = list(temp_col = expr(sst_f),
                                 anom_col = expr(anom_f),
                                 seas_col = expr(seas_f),
                                 hw_thresh = expr(mhw_thresh_f),
                                 cs_thresh = expr(cs_thresh_f),
                                 hw_temp = expr(hwe_f),
                                 temp_suff = " \u00b0F"))
  
  
  # Assign to shorter names
  temp_col <- temp_ops$temp_col
  anom_col <- temp_ops$anom_col
  clim_col <- temp_ops$seas_col
  hw_thresh_col <- temp_ops$hw_thresh
  cs_thresh_col <- temp_ops$cs_thresh
  hw_temp_col <- temp_ops$hw_temp
  
  
  # Set colors by name
  color_vals <- c(
    "Non-Event"       = "#0571B0",
    "Marine Heatwave" = "darkred")
  
  
  
  # Build the Plot
  hw_p <- year_hw_dat %>% 
    mutate(
      hw_anom_thresh = {{ hw_thresh_col }} - {{ clim_col }},
      cs_anom_thresh    = {{ cs_thresh_col }} - {{ clim_col }}) %>% 
    ggplot() +
    geom_path(aes(x = time, y = {{anom_col}}, color = mhw_event_txt, group = 1)) +
    geom_segment(aes(x = time, xend = time, y = 0, yend = {{anom_col}}, color = mhw_event_txt), alpha = 0.25) +
    geom_path(aes(x = time, y = hw_anom_thresh), color = "gray10", lty = 3) + 
    # geom_textpath(aes(x = time, y = hw_anom_thresh), color = "gray10", label = "Heatwave Threshold", hjust = .33, lty = 1) + 
    geom_textpath(aes(x = time, y = 0), color = "gray10", label = "Climatological Mean", hjust = 0.5, lty = 1) +
    #geom_textpath(aes(x = time, y = cs_anom_thresh), color = "gray10", label = "Cold Spell Threshold", hjust = 0.95, lty = 1) +
    scale_color_manual(values = color_vals) +
    scale_x_date(date_labels = "%b %y", 
                 date_breaks = "1 month", 
                 expand = expansion(mult = c(0,0))) +
    scale_y_continuous(labels =  number_format(suffix = temp_ops$temp_suff)) +
    theme(legend.position = "bottom", legend.title = element_text(face = "bold")) +
    labs(x = NULL, 
         color = "Daily Temperature Heatwave Status:",
         y = "Temperature Anomaly")
  
  return(hw_p)
  
  
}






#' @title Plot Yearly Heatwave and Temperature Metrics
#'
#' @param temp_dat 
#' @param temp_units 
#' @param year_focus 
#'
#' @return
#' @export
#'
#' @examples
yearly_metric_plots <- function(temp_dat, temp_units, year_focus = "2021"){
  
  # Swap columns for units
  temp_ops <- switch(
    EXPR = temp_units,
    "C" = list(
      temp_col = expr(sst),
      anom_col = expr(sst_anom),
      temp_suff = deg_c),
    "F" = list(
      temp_col = expr(sst_f),
      anom_col = expr(anom_f),
      temp_suff = deg_f)
  )
  temp_col <- temp_ops$temp_col
  anom_col <- temp_ops$anom_col
  temp_suff <- temp_ops$temp_suff
  
  
  
  
  
  # Assemble metrics 
  year_mets <- temp_dat %>% 
    filter(year != "2022") %>% 
    group_by(year) %>% 
    summarise(
      total_days                         = n(),
      `Average Temperature`              = round(mean({{ temp_col }}, na.rm = T), 2),
      `Temperature Above Average`        = round(mean({{ anom_col }}, na.rm = T), 2),
      `Cumulative Degrees Above Average` = round(sum({{ anom_col }}, na.rm = T), 0),
      hw_days                            = sum(mhw_event, na.rm = T),
      hw_events                          = n_distinct(mhw_event_no),
      peak_temp                          = max({{ temp_col }}),
      peak_anom                          = max({{ anom_col }}))
  
  # Supplement with some derived columns
  year_mets <- year_mets %>% 
    mutate(
      hw_day_pct = round((hw_days/total_days) * 100, 2),
      `Average Heatwave Length` = round(hw_days/hw_events,  1),
      yr_focus = ifelse(year == year_focus, TRUE, FALSE)) %>% 
    pivot_longer(names_to = "Var", values_to = "Metric", 
                 cols = c("Average Temperature", "Temperature Above Average", "Average Heatwave Length", "Cumulative Degrees Above Average"))
  
  
  # Build Seperately:
  rplot_1 <- year_mets %>% 
    filter(Var == "Average Temperature") %>% 
    group_by(Var) %>% 
    slice_max(n = 5, order_by = Metric) %>% 
    mutate(year = factor(year),
           year = fct_reorder(year, Metric, median)) %>% 
    ggplot(aes(Metric, year, fill = yr_focus, color = yr_focus)) +
    directlabels::geom_dl(aes(label = str_c(Metric, deg_sym)), 
                          method = list(directlabels::dl.trans(x = x + 1), rot = 0)) +
    geom_col(show.legend = FALSE) + 
    scale_x_continuous(expand = expansion(mult = c(0, 0.15)), labels = number_format(suffix = temp_suff)) +
    scale_color_gmri() +
    scale_fill_gmri() +
    theme(panel.grid = element_blank()) +
    labs(y = "",
         x = "Average Temperatures")
  
  
  rplot_2 <- year_mets %>% 
    filter(Var == "Temperature Above Average") %>% 
    group_by(Var) %>% 
    slice_max(n = 5, order_by = Metric) %>% 
    mutate(year = factor(year),
           year = fct_reorder(year, Metric, median)) %>% 
    ggplot(aes(Metric, year, fill = yr_focus, color = yr_focus)) +
    directlabels::geom_dl(aes(label = str_c(Metric, deg_sym)), 
                          method = list(directlabels::dl.trans(x = x + 1), rot = 0)) +
    geom_col(show.legend = FALSE) + 
    scale_x_continuous(expand = expansion(mult = c(0, 0.15)), labels = number_format(suffix = temp_suff)) +
    scale_color_gmri() +
    scale_fill_gmri() +
    theme(panel.grid = element_blank()) +
    labs(y = "",
         x = "Temperature Above Average",
         caption = "Anomalies calculated using 1982-2011 climatology.")
  
  
  # Replace the facet option
  hottest_yr_panels <- rplot_1 / rplot_2 + plot_annotation(title = "How 2021 Stacks Up:")
  
  # Display plot
  hottest_yr_panels
  
  
}



#' @title Heatwave Anomaly Heatmap
#'
#' @param hw_dat 
#' @param temp_units 
#'
#' @return
#' @export
#'
#' @examples
heatwave_heatmap_plot <- function(hw_dat, temp_units = "C", start_yr = 1981, end_yr = 2021){
  
  base_date <- as.Date("2000-01-01")
  
  # Set unit controls
  temp_ops <- switch(
    EXPR = temp_units,
    "C" = list(anom_col = expr(sst_anom),
               temp_suff = "\u00b0C",
               temp_limits = c(-3,3)),
    "F" = list(anom_col = expr(anom_f),
               temp_suff = "\u00b0F",
               temp_limits = c(-5,5)))
  temp_limits <- temp_ops$temp_limits
  anom_col <- temp_ops$anom_col
  temp_suff <- temp_ops$temp_suff
  
  # Color limit for palettes
  temp_breaks <- c(temp_limits[1], temp_limits[1]/2,  0, temp_limits[2]/2, temp_limits[2])
  temp_labels <- str_c(c(str_c("< ", temp_limits[1]), temp_limits[1]/2, 0, temp_limits[2]/2, str_c("> ", temp_limits[2])), temp_suff)
  
  
  
  
  # Assemble heatmap plot
  heatwave_heatmap <- ggplot(hw_dat, 
                             aes(x = flat_date, y = year)) +
    
    # background box fill for missing dates
    geom_rect(xmin = base_date, 
              xmax = base_date + 365, 
              ymin = min(hw_dat$year) - .5, 
              ymax = max(hw_dat$year) + .5, 
              fill = "gray75", color = "transparent") +
    
    # tile for sst colors
    geom_tile(aes(fill = {{ anom_col }})) +
    # points for heatwave events
    geom_point(data = filter(hw_dat, mhw_event == TRUE),
               aes(x = flat_date, y = year), size = .25)  +
    # # Points if over threshold only
    # geom_point(data = filter(grid_data, sst > mhw_thresh),
    #            aes(x = flat_date, y = year), size = .25)  +
    scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = expansion(add = c(0,0))) +
    scale_y_continuous(limits = c(start_yr + .5, end_yr + .5), expand = expansion(add = c(0,0))) +
    scale_fill_distiller(palette = "RdBu", 
                         na.value = "transparent", 
                         limit = temp_limits, 
                         oob = scales::squish,
                         breaks = temp_breaks, 
                         labels = temp_labels) +
    
    #5 inches is default rmarkdown height for barheight
    guides("fill" = guide_colorbar(title = "Sea Surface Temperature Anomaly", 
                                   title.position = "right", 
                                   title.hjust = 0.5,
                                   barheight = unit(3.5, "inches"), 
                                   frame.colour = "black", 
                                   ticks.colour = "black")) +  
    theme(legend.title = element_text(angle = 90)) +
    labs(title = "Temperature Anomalies and Marine Heatwave Events",
         x = "", 
         y = "",
         "\nClimate reference period : 1982-2011",
         caption = "Heatwave event dates have been overlayed with black points for distinction.")
  
  
  # Assemble pieces
  return(heatwave_heatmap)
  
  
  
}






#' @title Monthly ranking heatmap
#'
#' @param hw_dat Temperature data that can be grouped on yr and month for ranking
#' @param temp_units "F" or "C"
#' @param no_dates_before String indicating the end date to include, entered as "yyyy-mm-dd"
#'
#' @return
#' @export
#'
#' @examples
month_rank_heatmap <- function(hw_dat, temp_units = "F", no_dates_before = "2022-09-01"){
  
  
  # Format the data to rak the months
  month_hw_ranks <- hw_dat %>% 
    filter(time < as.Date(no_dates_before)) %>% 
    group_by(yr, month) %>% 
    summarise(temp_c = mean(sst),
              temp_f = mean(sst_f),
              anom_c = mean(sst_anom),
              anom_f = mean(anom_f),
              .groups = "drop") %>% 
    group_by(month) %>% 
    arrange(desc(temp_c)) %>% 
    mutate(month_rank = row_number()) %>% 
    ungroup() %>% 
    mutate(month = factor(month, levels = month.abb),
           month = fct_rev(month))
  
  
  # Set unit controls
  temp_ops <- switch(
    EXPR = temp_units,
    "C" = list(anom_col = expr(anom_c),
               temp_suff = "\u00b0C",
               temp_limits = c(-3,3)),
    "F" = list(anom_col = expr(anom_f),
               temp_suff = "\u00b0F",
               temp_limits = c(-5,5)))
  
  # Pull the parts to insert into plot
  temp_limits <- temp_ops$temp_limits
  anom_col <- temp_ops$anom_col
  temp_suff <- temp_ops$temp_suff
  
  
  
  # Color limit for palettes
  temp_breaks <- c(temp_limits[1], temp_limits[1]/2,  0, temp_limits[2]/2, temp_limits[2])
  temp_labels <- str_c(c(str_c("< ", temp_limits[1]), temp_limits[1]/2, 0, temp_limits[2]/2, str_c("> ", temp_limits[2])), temp_suff)
  
  
  
  # Build the plot
  month_heatmap <- ggplot(month_hw_ranks, 
                          aes(yr, month, label = month_rank)) +
    geom_tile(aes(fill = {{anom_col}})) +
    geom_text(size = 3) +
    scale_x_continuous(expand = expansion(add = c(0,0))) +
    scale_fill_distiller(palette = "RdBu", 
                         na.value = "transparent", 
                         limit = temp_limits, 
                         oob = scales::squish,
                         breaks = temp_breaks, 
                         labels = temp_labels) +
    #5 inches is default rmarkdown height for barheight
    guides("fill" = guide_colorbar(title = "Sea Surface Temperature Anomaly", 
                                   title.position = "top", 
                                   title.hjust = 0.5,
                                   barwidth = unit(4, "inches"), 
                                   frame.colour = "black", 
                                   ticks.colour = "black")) +  
    theme(legend.position = "bottom",
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(-10,-10,-10,-10)) +
    labs(title = "Ranking Monthly Sea Surface Temperatures - Gulf of Maine",
         x = "", 
         y = "",
         "\nClimate reference period : 1982-2011",
         caption = "Relative rankings of each month displayed in their respective tiles.")
  
  
  return(month_heatmap)
  
}






#' @title Temperature Anomaly Horizon Plot
#'
#' @param grid_data 
#' @param origin 
#' @param scale_cutpoints 
#' @param labels 
#'
#' @return
#' @export
#'
#' @examples
anom_horizon_plot <- function(grid_data, 
                              origin = ori, 
                              scale_cutpoints = sca, 
                              labels = sca_labels){
  
  grid_data %>% 
    mutate(year = fct_rev(factor(year))) %>% 
    ggplot() +
    geom_horizon(aes(flat_date, 
                     sst_anom,
                     fill = ..Cutpoints..), 
                 origin = ori, 
                 horizonscale = sca) +
    scale_fill_hcl(palette = 'RdBu', reverse = T, labels = sca_labels) +
    facet_grid(year~.) +
    theme(
      #strip.background = element_rect(fill = "#36454F", color = "#36454F"),
      panel.grid = element_blank(),
      panel.spacing.y=unit(0.1, "lines"),
      strip.text.y = element_text(size = 7, angle = 0, hjust = 0),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.border = element_blank(),
      legend.position = "left") +
    scale_x_date(expand=c(0,0), 
                 date_breaks = "1 month", 
                 date_labels = "%b") +
    labs(x = '', 
         fill = "Temperature Anomaly") 
  
  
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






####________####


####  Misc Plot Elements:  ####
# If we want contours as sf:

 
# # Add the bottom contours:
# bathy <- raster("~/Documents/Repositories/Points_and_contours/NEShelf_Etopo1_bathy.tiff")
# contours_make <- c(-50, -100, -250)
# bathy_contours <- rasterToContour(bathy, levels = contours_make) %>% st_as_sf()
# bathy_contours$level <- factor(bathy_contours$level, levels = as.character(contours_make))

# Put in plot:
# metR::geom_text_contour(data = bathy_df, aes(x, y, z = depth),
#                   breaks = contours_make,
#                   color = "gray40",
#                   size = 1.4,
#                   min.size = 10,
#                   stroke = 0.2,
#                   rotate = FALSE,
#                   check_overlap = TRUE) +





# Gulf of Maine Warming n isolation
 
# # Fahrenheit
# gom_f <- ggplot(data = annual_summary, aes(year, area_wtd_anom_f)) +
# 
#   # Overlay yearly means
#   geom_line(color = "gray10", size = 1.5, linetype = 1) +
#   geom_point(color = "gray10", size = 1.5) +
#   geom_smooth(
#     data = annual_summary %>% 
#       mutate(`Warming Rate` = eq_all),
#     method = "lm",
#     formula = y ~ x, se = F,
#     aes(color = `Warming Rate`),
#     alpha = 0.90,
#     size = 1.5,
#     linetype = 2) + 
#    scale_color_gmri() +
#    scale_y_continuous(labels =  number_format(suffix = " \u00b0F")) +
#    labs(title = "Gulf of Maine:",
#         subtitle = "Annual Sea Surface Temperature Anomalies",
#         x = "Year", y = "Sea Surface Temperature Anomaly",
#         caption = "Anomalies calculated using 1982-2011 reference period.")  +
#    theme(legend.position = c(0.25, 0.85),
#          legend.background = element_rect(color = "black", fill = "white"))
# 
# #gom_f <- add_gmri_logo(gom_f, position = "bot_left")
# 
# # Celsius
# gom_c <- ggplot(data = annual_summary, aes(year, area_wtd_anom)) +
# 
#   # Overlay yearly means
#   geom_line(color = "gray10", size = 1.5, linetype = 1) +
#   geom_point(color = "gray10", size = 1.5) +
#   geom_smooth(
#     data = annual_summary %>% 
#       mutate(`Warming Rate` = eq_all_c),
#     method = "lm",
#     formula = y ~ x, se = F,
#     aes(color = `Warming Rate`),
#     alpha = 0.90,
#     size = 1.5,
#     linetype = 2) + 
#    scale_color_gmri() +
#    scale_y_continuous(labels =  number_format(suffix = " \u00b0C")) +
#    labs(title = "Gulf of Maine:",
#         subtitle = "Annual Sea Surface Temperature Anomalies",
#         x = "Year", y = "Sea Surface Temperature Anomaly",
#         caption = "Anomalies calculated using 1982-2011 reference period.")  +
#    theme(legend.position = c(0.25, 0.85),
#          legend.background = element_rect(color = "black", fill = "white"))
# 
# #gom_c <- add_gmri_logo(gom_c, position = "bot_left")
# 
# 
# 
# # save them
# ggsave(plot = gom_f, filename = str_c(save_location, "gom_annual_f.png"),
#        dpi = "retina", bg = "white")
# ggsave(plot = gom_c, filename = str_c(save_location, "gom_annual_c.png"),
#        dpi = "retina", bg = "white")

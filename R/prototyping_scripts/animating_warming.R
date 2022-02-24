#####  Animating Warming Trends


####  Libraries  ####
library(gmRi)
library(raster)
library(sf)
library(stars)
library(rnaturalearth)
library(tidyverse)
# library(gganimate)
# library(animation)
library(magick)
library(scales)
library(metR)

# Polygons for mapping
new_england <- ne_states("united states of america") %>% st_as_sf(crs = 4326) 
canada      <- ne_states("canada") %>% st_as_sf(crs = 4326)
world       <- ne_countries() %>% st_as_sf(crs = 4326)
greenland   <- ne_states(country = "greenland") %>% st_as_sfc(crs = 4326)

# Set theme up for maps
map_theme <- list(
  theme_bw() +
  theme(
    panel.border       = element_rect(color = "black", fill = NA),
    plot.background    = element_rect(color = "transparent", fill = "transparent"),
    line               = element_blank(),
    axis.title.x       = element_blank(), # turn off titles
    axis.title.y       = element_blank(),
    legend.position    = "bottom", 
    legend.title.align = 0.5))


# Turn on the gmri font for plots - doesn't really connect to gmri font
showtext::showtext_auto()


####  Load SST Data  ####
oisst_path <- box_path("res", "OISST/oisst_mainstays")
gom_window <- data.frame(
  lon = c(-79, -59.5),
  lat = c(36.75, 45.75),
  time = as.Date(c("2000-01-01", "2020-12-31")))

# load oisst
regional_oisst <- oisst_window_load(oisst_path = oisst_path, data_window = gom_window, anomalies = F)
# load anoms
regional_anoms <- oisst_window_load(oisst_path = oisst_path, data_window = gom_window, anomalies = T)






####  Make Monthly  ####
make_monthly <- function(sst_data, sst_year){
  
  # Pull the dates from layers, single out months
  dates <- names(sst_data)
  month_dates <- str_sub(dates, 7, 8)
  
  
  # Make vector of months to loop through
  mnths <- str_pad(c(1:12), width = 2, side = "left", pad = "0")
 
 
  # Get monthly averages
  month_means <- purrr::map(mnths, function(mnth){
    dates_in_month <- which(str_detect(month_dates, mnth))
    month_data <- raster::calc(sst_data[[dates_in_month]], mean, na.rm = T)
    return(month_data)
  })
  
  # Set Names
  month_labs <- str_c(sst_year, mnths, "15", sep = ".")
  names(month_means) <- month_labs
  
  # re-stack the year
  year_data <- raster::stack(month_means)
  return(year_data)
}



# get monthly averages
sst_monthly <- imap(regional_oisst, make_monthly)
anoms_monthly <- imap(regional_anoms, make_monthly)



####  Blueprint Image  ####

# stack all the months
all_months_sst <- stack(sst_monthly)
all_months <- stack(anoms_monthly)

# start and end points for animation
start_index <- which(names(all_months) == "X2004.01.15")
end_index   <- which(names(all_months) == "X2020.12.15")




# Set crop
crop_x <- range(gom_window$lon)
crop_y <- range(gom_window$lat)

# Set global limits for color scale

# Overall Mean for start/end period
period_mean <- mean(getValues(all_months[[start_index:end_index]]), na.rm = T)
# limit  <- max(maxValue(all_months)) * c(-1,1) # for equal ranges
# limit  <- c(min(minValue(all_months)), max(maxValue(all_months))) # for using actual range
period_limit  <- c(period_mean - 4, period_mean + 4)  #For squishing to smaller range

####  Month Plot  ####
# Function to plot a month
plot_month <- function(month_data, temp_limits = period_limit){

  # Prep labels - pull year, format month
  date_label <- names(month_data)
  date_label <- str_replace_all(date_label, "[.]", "-") %>% str_sub(2, -4)
  year_lab <- str_sub(date_label, 1, 4)
  month_num <- str_sub(date_label, -2, -1) %>% as.numeric()
  month_lab <- month.abb[month_num]
  month_label <- str_c(year_lab, month_lab, sep = " - ")
  
  # Make stars Object
  month_st <- st_as_stars(month_data)
  
  # Build Plot
  month_map <- ggplot() +
    geom_stars(data = month_st) +
    geom_sf(data = new_england, fill = "gray90", size = .25) +
    geom_sf(data = canada, fill = "gray90", size = .25) +
    geom_sf(data = greenland, fill = "gray90", size = .25) +
    # # Standard Palette
    # scale_fill_distiller(palette = "RdBu",
    #                      na.value = "transparent",
    #                      limit = temp_limits,
    #                      oob = scales::squish) +
    # metR scale - for brewer pals use:https://colorbrewer2.org/#type=diverging&scheme=RdYlBu&n=3
    scale_fill_divergent(low = "#67a9cf",
                         mid = "white",
                         high = "#fc8d59",
                         limits = temp_limits,
                         midpoint = 0.64, 
                         oob = squish, 
                         na.value = "transparent") +
    annotate(x = -61.5, y = 37.15, geom = "text", label = month_label, size = 4) +
    map_theme +
    coord_sf(xlim = crop_x, ylim = crop_y, expand = F) +
    guides("fill" = guide_colorbar(
      title = expression("Sea Surface Temperature Anomaly"~~degree~C),
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(5, "in"),
      frame.colour = "black",
      ticks.colour = "black")) +
    theme(#axis.text = element_text(size = 12), 
          axis.text = element_blank(),
          legend.text = element_text(size = 12), 
          legend.position = "none")
    
  # Return Plot
  return(month_map)
}



####  Plotting any month  ####



# Plot start and end of animation of Anomalies
plot_month(all_months[[start_index]]) 
plot_month(all_months[[end_index-11]]) 


# Regular SST 
# period_mean_sst <- mean(getValues(all_months_sst[[start_index:end_index]]), na.rm = T)#overall  mean
sst_limit  <- c(0, max(maxValue(all_months_sst))-3)
plot_month(all_months_sst[[start_index]], temp_limits = sst_limit) 
plot_month(all_months_sst[[end_index-4]], temp_limits = sst_limit) 






# Idea to highlight warming better:
# use actual temperatures, scale on min/max temps?

# Alternative option: Change scale to pinch the range white values appear
# scale_color_gradientn(colours = c("blue", "lightblue", "white", "red", "darkred"),
#                       values = c(-4, -.5, 0, 0.5, 4))




####  Saving Individual Images as Frames  ####

# Save All the Frames
for(i in start_index:end_index){
  
  # Save info
  save_num <- i - (start_index - 1)
  save_num <- str_pad(save_num, width = 3, side = "left", pad = "0")
  save_name <- paste0(save_num, ".png")
  print(paste("Saving: ", save_name))
  
  # Build plot
  p <- plot_month(all_months[[i]])
  
  # Save Image
  ggsave(filename = save_name,
         plot = p,
         path = here::here("animations/frames/"), device = "png",
         height = unit(4.7, "in"),
         width  = unit(8, "in"),
         dpi = 300)
}







####  Making Animation using magick  ####


# # file names 001.jpg
# 
# # Using Magick to build animation
# img <- image_graph(1920, 1080, res = 72)
# 
# #Create a loop the does the plotting
# for(i in start_index:end_index){
#   p <- plot_month(all_months[[i]], limit)
#   print(p) }#close the for loop
# 
# # Turn off graphics devices
# dev.off()
# 
# # build animation from the plots
# animation <- image_animate(img, fps = 1)
# print(animation)
# 
# # Save Gif
# image_write(animation, path = here::here("animations/2010_2020.gif"))





# # Custom breaks attempt not showing colors
# scale_color_gradientn(colours = c("blue", "lightblue", "white", "red", "darkred"),
#                       values = c(-4, -3, 0, 3, 4),
#                       na.value = "transparent", 
#                       limit = limit, 
#                       oob = scales::squish) +
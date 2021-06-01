#####  Animating Warming Trends


####  Libraries  ####
library(gmRi)
library(raster)
library(sf)
library(stars)
library(rnaturalearth)
library(tidyverse)
library(gganimate)

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



####  Load Data  ####
oisst_path <- box_path("res", "OISST/oisst_mainstays")
gom_window <- data.frame(
  lon = c(-79, -59.5),
  lat = c(36.75, 45.75),
  time = as.Date(c("1982-01-01", "2020-12-31")))

# load temp
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
all_months <- stack(anoms_monthly)
#month_data <- all_months[[1]]

# get global limits for scale
limit  <- max(maxValue(all_months)) * c(-1,1)

# function to plot a month
plot_month <- function(month_data, temp_limits){

  # Prep labels
  month_label <- names(month_data)
  month_label <- str_replace_all(month_label, "[.]", "-") %>% str_sub(2, -1)
  
  # Make stars Object
  month_st <- st_as_stars(month_data)
  
  # Set crop
  crop_x <- range(gom_window$lon)
  crop_y <- range(gom_window$lat)
  
  
  # Build Plot
  month_map <- ggplot() +
    geom_stars(data = month_st) +
    geom_sf(data = new_england, fill = "gray90", size = .25) +
    geom_sf(data = canada, fill = "gray90", size = .25) +
    geom_sf(data = greenland, fill = "gray90", size = .25) +
    scale_fill_distiller(palette = "RdYlBu", na.value = "transparent", limit = limit) +
    annotate(x = -61.5, y = 37, geom = "text", label = month_label, size = 6) +
    map_theme +
    coord_sf(xlim = crop_x, ylim = crop_y, expand = F) +
    guides("fill" = guide_colorbar(
      title = expression("Sea Surface Temperature Anomaly"~~degree~C),
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(6, "in"),
      frame.colour = "black",
      ticks.colour = "black")) +
    theme(axis.text = element_text(size = 14), 
          legend.text = element_text(size = 12))
    
  # Return Plot
  return(month_map)
}



####  Plotting any month  ####
plot_month(all_months[[60]])
plot_month(all_months[[400]])


####  Making Animation  ####
library(animation)
library(magick)

# file names 001.jpg

# ## The animation as a simple GIF
# saveGIF({
#   
#   #create a loop the does the plotting
#   for(i in 1:20){
#     p <- plot_month(all_months[[i]], limit)
#     print(p)
#   }#close the for loop
#   
# }, convert = 'convert', 
# movie.name = 'animations/warming_animation.gif') #close the animation builder


# Using Magick
img <- image_graph(1920, 1080, res = 120)


#create a loop the does the plotting
for(i in 1:15){
  p <- plot_month(all_months[[i]], limit)
  print(p)
}#close the for loop

dev.off()
animation <- image_animate(img, fps = 4)
print(animation)

# Save Gif
image_write(animation, path = "animations/month_test.gif")


# tiger <- image_read_svg('http://jeroen.github.io/images/tiger.svg', width = 350)
# print(tiger)

####  Ranking Warming Among LME's and Gulf of Maine
# and: lme warming stripe geofacets

####  Packages  ####
library(gmRi)
library(here)
library(lubridate)
library(geofacet)
library(patchwork)
library(heatwaveR)
library(tidyverse)
library(ggforce)
library(scales)

# # Support Functions
source(here("R/oisst_support_funs.R"))
# source(here("R/temp_report_support.R"))

# set ggplot theme for figures
theme_set(theme_bw())


####  Load OISST Data from Cloud  ####

# Gulf of Maine
gom_oisst <- oisst_access_timeseries(region_family = "gmri focus areas", 
                                     poly_name = "apershing gulf of maine", 
                                     mac_os = "mojave")

gom_oisst <- mutate(gom_oisst, time = as.Date(time)) %>% 
  drop_na()


# Large Marine Ecosystems

# Get the list of names for the different LME's
lme_names <- get_region_names("lme") 

# Load the path information for the group
lme_paths <- get_timeseries_paths("lme", mac_os = "mojave") # paths


# Read the temperature csv files for each LME
lme_oisst <- map(.x = lme_names, function(x){

  read_csv(lme_paths[[.x]][["timeseries_path"]],
           guess_max = 1e5,
           col_types = cols())
  
  })

# Do some additional tidying:
lme_oisst <- map(lme_oisst, ~ mutate(.x, time = as.Date(time)) %>% drop_na())
lme_oisst <- setNames(lme_oisst, lme_names)


# Add Gulf of Maine to the LME list
lme_oisst[["gulf_of_maine"]] <- gom_oisst

# Run heatwave events check on each of them:
lme_oisst <- map(.x = lme_oisst,
                 .f = pull_heatwave_events, 
                 threshold = 90, 
                 clim_ref_period = c("1982-01-01", "2011-12-31"))



####  Calculate LME Warming Rates  ####

lme_8220 <- map(lme_oisst, ~ mutate(.x, year = year(time)) %>% 
                  filter(year %in% c(1982:2021)))

# Get warming rates
lme_rates <- map_dfr(lme_8220, function(lme_data){
  lme_yearly <- group_by(lme_data, year) %>% summarise(sst = mean(sst, na.rm = T))
  yrly_warming <- lm(sst ~ year, data = lme_yearly)
  mod_details <- broom::tidy(yrly_warming)
  yrly_rate <- mod_details[2,"estimate"]
  names(yrly_rate) <- "annual_warming_rate_C"
  yrly_rate
}, .id = "Region") %>% 
  arrange(desc(annual_warming_rate_C))

# Top ten warming Rates
lme_rates %>% slice(1:10)





####  LME Warming Stripes  ####
# reference: temperature_stripes.R


#### 1. Make monthly timeseries of them


# a. Function to do it
make_monthly <- function(region_timeseries){
  
  # Group by year & month, average temp and anomalies
  monthly_temps <- region_timeseries %>% 
    group_by(year = year(time),
             month = month(time)) %>% 
    summarise(sst = mean(sst),
              anom = mean(sst_anom),
              .groups = "drop") %>% 
    mutate(time = as.Date(str_c(year, str_pad(month, width = 2, pad = "0", side = "left"), "01", sep = "-")))
  
  return(monthly_temps)
  
}


# b. Perform on list:
monthly_lmes <- map(lme_oisst, make_monthly)



#### 2. Make Monthly Plots

# a. function to do it
plot_monthly_stripes <- function(monthly_temps, lme_name){
  
  name_tidy <- str_replace_all(lme_name, "_", " ")
  name_tidy <- str_to_title(name_tidy)
  
  ggplot(monthly_temps, aes(x = time, y = 0)) +
    geom_tile(aes(fill = anom)) +
    scale_fill_distiller(palette = "RdBu", limits = c(-1.5,1.5), oob = oob_squish) +
    scale_y_continuous(expand = expansion(add = c(0,0.01))) +
    scale_x_date(expand = expansion(add = c(45,45))) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          plot.subtitle = element_text(hjust = 0.125),
          panel.border = element_rect(color = "black", size = 1, fill = "transparent"),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          panel.grid = element_blank(),
          plot.caption = element_text(size = 7.2, margin = margin(t = 20), color = "gray40")
    ) +
    labs(title = str_c(name_tidy, " - Sea Surface Temperature Anomalies"),
         x = "",
         caption = "Monthly Averages from 1982-2021  |  Climatology Period 1982-2011"
         #subtitle = "Sea Surface Temperature Anomalies"
    )
  
  
}


# b. Perform on list
lme_stripes <- imap(monthly_lmes, plot_monthly_stripes)






####  Geofacet Warming Stripes  ####

# same idea as individual ones, just need to put them all in one table
# add lme id to facet, and the coordinate system for how to arrange them


# Put lme monthly into big table
# clean up the facet id
lme_df <- bind_rows(monthly_lmes, .id = "lme") %>% 
  mutate(lme_name = str_replace_all(lme, "_", " "),
         lme_name = str_to_title(lme_name))





# some sort of coordinate system for geofacet
# https://john-joseph-horton.com/extending-geofacet-to-algorithmically-generate-grids-from-lat-long-data/
# https://rpubs.com/cyclemumner/278962

library(raster)
library(tabularaster)
library(sf)



## choose a background grid
rgrid <- raster(extent(-180, 180, -90, 90), 
                crs = 4326, 
                res = 5)


# Load the shapefiles to get their center points

# Gulf of Maine
gom_sf_path <- get_timeseries_paths(
  region_group = "gmri_sst_focal_areas", 
  mac_os = "mojave")[["apershing_gulf_of_maine"]][["shape_path"]]
gom_sf <- read_sf(gom_sf_path)  


# LME Polygons
lme_paths <- get_timeseries_paths("lme", mac_os = "mojave") # paths
lme_shapes <- map(lme_names, ~ read_sf(lme_paths[[.x]][["shape_path"]])) # data
lme_shapes <- setNames(lme_shapes, lme_names)


# Add Gulf of Maine to lme list
lme_shapes$gulf_of_maine <- gom_sf



# Get average coordinates of all of them:
sf_center_coord <- function(sf_obj) {
  bbox_obj <- st_bbox(sf_obj)
  xmin <- as.numeric(bbox_obj[1])
  ymin <- as.numeric(bbox_obj[2])
  xmax <- as.numeric(bbox_obj[3])
  ymax <- as.numeric(bbox_obj[4])
  
  xavg <- mean(c(xmin, xmax))
  yavg <- mean(c(ymin, ymax))
  
  bbox_df <- tribble(
    ~"lon", ~"lat",
    xavg,   yavg) %>% 
    st_as_sf(crs = 4326, coords = c("lon", "lat"), remove = FALSE)
  
}

# Get centers
lme_centers <- map(lme_shapes, sf_center_coord)

# Get their grid cells
## identify the coordinate of the input in the background grid, 
## and obtain the row, col, and label
## (remember to flip the row)
lme_cells <- map_dfr(lme_centers, ~ cellnumbers(rgrid, .x), .id = "lme") %>% 
  mutate(
    row = nrow(rgrid) - rowFromCell(rgrid, cell_) + 1, # Flip the row
    col = colFromCell(rgrid, cell_)) %>% 
  dplyr::select(-cell_, -object_)

# Check placement
ggplot(lme_cells, aes(col, row, label = lme)) +
  geom_label()



#### Geofacet them

# Pass row/column information to monthly average data
lme_geofacet_dat <- left_join(lme_df, lme_cells, by = c("lme"))



# Make a "grid" for geofacet, dumb step
# I used: grid_design(data = distinct(lme_geofacet_dat, row, col, name = lme_name, code = lme))

mygrid <- data.frame(
  row = c(4, 10, 10, 10, 11, 12, 13, 13, 13, 14, 14, 15, 15, 16, 16, 18, 19, 19, 19, 20, 20, 20, 21, 21, 22, 22, 23, 23, 23, 23, 24, 24, 25, 25, 25, 26, 26, 27, 27, 27, 27, 27, 28, 28, 29, 29, 29, 29, 29, 30, 30, 30, 30, 31, 31, 32, 32, 32, 33, 33, 33, 33, 33, 33, 34, 35, 35),
  col = c(36, 71, 24, 65, 61, 22, 67, 27, 59, 43, 39, 66, 60, 29, 63, 61, 36, 26, 46, 57, 17, 61, 55, 59, 49, 22, 34, 19, 3, 44, 61, 14, 12, 63, 21, 40, 61, 43, 35, 22, 63, 23, 67, 24, 2, 35, 8, 25, 67, 40, 3, 37, 70, 35, 21, 22, 33, 37, 43, 11, 69, 33, 52, 3, 61, 23, 36),
  name = c("Antarctica", "New Zealand Shelf", "Patagonian Shelf", "Southeast Australian Shelf", "South West Australian Shelf", "Humboldt Current", "East Central Australian Shelf", "South Brazil Shelf", "West Central Australian Shelf", "Agulhas Current", "Benguela Current", "Northeast Australian Shelf", "Northwest Australian Shelf", "East Brazil Shelf", "North Australian Shelf", "Indonesian Sea", "Guinea Current", "North Brazil Shelf", "Somali Coastal Current", "Gulf Of Thailand", "Pacific Central American Coastal", "Sulu Celebes Sea", "Bay Of Bengal", "South China Sea", "Arabian Sea", "Caribbean Sea", "Canary Current", "Gulf Of Mexico", "Insular Pacific Hawaiian", "Red Sea", "East China Sea", "Gulf Of California", "California Current", "Kuroshio Current", "Southeast Us Continental Shelf", "Mediterranean Sea", "Yellow Sea", "Black Sea", "Iberian Coastal", "Northeast Us Continental Shelf", "Sea Of Japan", "Gulf Of Maine", "Oyashio Current", "Scotian Shelf", "Aleutian Islands", "Celtic Biscay Shelf", "Gulf Of Alaska", "Labrador Newfoundland", "Sea Of Okhotsk", "Baltic Sea", "East Bering Sea", "North Sea", "West Bering Sea", "Faroe Plateau", "Hudson Bay Complex", "Canadian Eastern Arctic West Greenland", "Iceland Shelf And Sea", "Norwegian Sea", "Barents Sea", "Beaufort Sea", "East Siberian Sea", "Greenland Sea", "Kara Sea", "Northern Bering Chukchi Seas", "Laptev Sea", "Canadian High Arctic North Greenland", "Central Arctic"),
  code = c("antarctica", "new_zealand_shelf", "patagonian_shelf", "southeast_australian_shelf", "south_west_australian_shelf", "humboldt_current", "east_central_australian_shelf", "south_brazil_shelf", "west_central_australian_shelf", "agulhas_current", "benguela_current", "northeast_australian_shelf", "northwest_australian_shelf", "east_brazil_shelf", "north_australian_shelf", "indonesian_sea", "guinea_current", "north_brazil_shelf", "somali_coastal_current", "gulf_of_thailand", "pacific_central_american_coastal", "sulu_celebes_sea", "bay_of_bengal", "south_china_sea", "arabian_sea", "caribbean_sea", "canary_current", "gulf_of_mexico", "insular_pacific_hawaiian", "red_sea", "east_china_sea", "gulf_of_california", "california_current", "kuroshio_current", "southeast_us_continental_shelf", "mediterranean_sea", "yellow_sea", "black_sea", "iberian_coastal", "northeast_us_continental_shelf", "sea_of_japan", "gulf_of_maine", "oyashio_current", "scotian_shelf", "aleutian_islands", "celtic_biscay_shelf", "gulf_of_alaska", "labrador_newfoundland", "sea_of_okhotsk", "baltic_sea", "east_bering_sea", "north_sea", "west_bering_sea", "faroe_plateau", "hudson_bay_complex", "canadian_eastern_arctic_west_greenland", "iceland_shelf_and_sea", "norwegian_sea", "barents_sea", "beaufort_sea", "east_siberian_sea", "greenland_sea", "kara_sea", "northern_bering_chukchi_seas", "laptev_sea", "canadian_high_arctic_north_greenland", "central_arctic"),
  stringsAsFactors = FALSE
)


#### Build Plot, faceting each lme ###
# At this stage we need a dataframe of monthly anomalies for all LMEs
# And a factor column indicating which LME the data is from to be able to facet

# Color limit and labels for temperature palette
temp_suff <- "\u00b0C" # Unit
temp_limits <- c(-1.5, 1.5) # anomaly range
temp_breaks <-  seq(from = temp_limits[1], to = temp_limits[2], by = .5) # breaks

# make the break labels, with < & > to show truncation
temp_labels <- str_c(c(str_c("< ", temp_limits[1]), temp_breaks[2:(length(temp_breaks)-1)], str_c("> ", temp_limits[2])), temp_suff)


# Build plot
lme_geofacet_dat %>% 
  #filter(lme %in% lme_names[1:10])  %>%  # for testing a subset
  rename(name = lme_name, code = lme) %>% # needed to match geofacet grid
  ggplot(aes(x = time, y = 0)) +
  geom_tile(aes(fill = anom)) +
  scale_fill_distiller(
    palette = "RdBu", 
    limits = temp_limits, 
    oob = oob_squish,
    breaks = temp_breaks,
    labels = temp_labels) +
  scale_y_continuous(expand = expansion(add = c(0,0.01))) +
  scale_x_date(expand = expansion(add = c(45,45))) +
  # # Option 1: Facet wrap
  facet_wrap(~name) +
  # # Option 2: Geofacet - too many to display correctly
  # facet_geo(~ name, grid = mygrid) +
  theme_minimal() +
  # Theme Formatting
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14, color = "#36454F"),
        plot.subtitle = element_text(hjust = 0.125),
        panel.border = element_rect(color = "transparent", size = 1, fill = "transparent"),
        strip.text = element_text(
          size = 6, 
          color = "#36454F", 
          face = "bold"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        #plot.background = element_rect(fill = "#36454F"),
        plot.caption = element_text(size = 7.2, 
                                    margin = margin(t = 20), 
                                    color = "gray10")) +
  # Legend Formatting
  guides(
    fill = guide_colorbar(
      barwidth = unit(4.5, "in"), 
      barheight = unit(0.3, "cm"),
      nbin = 5, 
      frame.colour = "#36454F",
      ticks.colour = element_line(color = "#36454F"),
      title = "Surface Temperature Anomalies",
      title.position = "top", 
      title.hjust = 0.5, 
      title.theme = element_text(color = "#36454F", size = 8, face = "bold"),
      label.theme = element_text(color = "#36454F", size = 7)))  +
  # Labels
  labs(
    title = "Rising Temperatures of the World's Large Marine Ecosystems",
    x = "",
    caption = "Monthly Averages from 1982-2021  |  Climatology Period 1982-2011 | Gulf of Maine not an official LME*")



####  TweetR  ####

install.packages()

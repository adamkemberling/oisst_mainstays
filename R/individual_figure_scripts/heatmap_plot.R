####  Marine Heatwave Heatmap  
# Gulf of Maine




####  Packages  ####
{
  library(lubridate)
  library(raster)
  library(here)
  library(scales)
  library(gmRi)
  library(heatwaveR)
  library(gt)
  library(gtExtras)
  library(tidyverse)
  library(ggforce)
  library(geomtextpath)
  library(showtext)
  library(gtable)
  library(ggpubr)
  library(ragg)
}


conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")
conflicted::conflict_prefer("rotate", "raster")



# Support Functions
source(here("R/oisst_support_funs.R"), verbose = FALSE)
source(here("R/temp_report_support.R"), verbose = FALSE)

# Box paths
oisst_path <- cs_path("res", "OISST/oisst_mainstays")


# File paths for various extents based on "apershing_gulf_of_maine"
region_paths <- get_timeseries_paths(region_group = "gmri_sst_focal_areas", 
                                     box_location = "cloudstorage")


# --------- Theme  -----------




# Set ggplot theme for figures
theme_set(
  theme_bw() + 
    theme(
      # Titles
      text = element_text(family = "Avenir"),
      plot.title = element_text(hjust = 0, face = "bold", size = 18),
      plot.subtitle = element_text(size = 16),
      plot.caption = element_text(size = 8, margin = margin(t = 20), color = "gray40"),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 12),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      #panel lines
      panel.grid = element_blank(),
      # Axes
      axis.line.y = element_line(color = "black"),
      axis.ticks.y = element_line(), 
      axis.line.x = element_line(color = "black"),
      axis.ticks.x = element_line(), 
      rect = element_rect(fill = "transparent", color = "black"),
      # Facets
      strip.text = element_text(
        color = "white", 
        face = "bold",
        size = 11),
      strip.background = element_rect(
        color = "#00736D", 
        fill = "#00736D", 
        linewidth = 1, 
        linetype="solid")
    )
)




# --------  Fonts  ---------


# Path to the directory containing the font file (replace with your actual path)
font_dir <- paste0(system.file("stylesheets", package = "gmRi"), "/GMRI_fonts/Avenir/")

# Register the font
font_add(
  family = "Avenir",
  file.path(font_dir, "LTe50342.ttf"),
  bold = file.path(font_dir, "LTe50340.ttf"),
  italic = file.path(font_dir, "LTe50343.ttf"),
  bolditalic = file.path(font_dir, "LTe50347.ttf"))

# Load the font
showtext::showtext_auto()






#--------  Data Loading  -----------

# Timeseries Path +
timeseries_path <- region_paths[["apershing_gulf_of_maine"]][["timeseries_path"]]

# Load timeseries of SST for Region
region_timeseries <- read_csv(
  timeseries_path, 
  col_types = cols(), 
  guess_max = 1e5) 

# Format timeseries for group estimates
region_timeseries <- region_timeseries %>% 
  mutate(
    time = as.Date(time),
    area_wtd_f = as_fahrenheit(area_wtd_sst),
    anom_f     = as_fahrenheit(area_wtd_anom, "anomalies")) %>% 
  distinct(time, .keep_all = T) %>% 
  supplement_season_info() %>% 
  filter(year %in% c(1982:2024))


####  Get heatwave statuses for each day:

# Uses area weighted sst by default
region_hw <- pull_heatwave_events(
  temperature_timeseries = region_timeseries,
  threshold = 90, 
  clim_ref_period = c("1991-01-01", "2020-12-31")) %>% 
  supplement_hw_data() %>% 
  filter(doy != 366) # handle leap years 


# Put dates on the same year to use date formatting on axis
base_date <- as.Date("2000-01-01")
grid_dat <- region_hw %>% 
  # grid_dat <- sst_new %>% 
  mutate(
    year = year(time),
    yday = yday(time),
    flat_date = as.Date(yday-1, origin = base_date),
    yr_rev = factor(year),
    yr_rev = fct_rev(yr_rev)) 


#------ The Plot  -----------



# All years:
heatmap_p <- grid_dat %>% 
  filter(yr %in% c(1982:2023)) %>% 
  heatwave_heatmap_plot(temp_units = "F", start_yr = 1981, end_yr = 2023) +
  labs(
    title = "Marine Heatwaves in the Gulf of Maine",
    y = "Year",
    x = "Month",
    caption = NULL) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(angle = 0, size = 11, face = "bold"),
    plot.margin = margin(b = 0, t = 20, l = 5, r = 5)) +
  guides(fill = guide_colorbar(
    title = "Sea Surface Temperature Anomaly",
    title.position = "top", 
    title.hjust = 0.5, barwidth = unit(4, "in"), 
    barheight = unit(0.5, "cm"), 
    frame.colour = "black", 
    ticks.colour = "black")) #+
  # coord_cartesian(clip = "off") +
  # geom_logo(x_npc = 0.85, y_npc = 1.05, logo_height = 1, height_units = "cm")

#Remove caption
heatmap_p








####  Saving  ####

# Prevents tiny fonts when saving
showtext::showtext_opts(dpi=300) 


# heatwave heatmap -  Normal Size
local_fig_folder <- here::here("local_data/standard_figs/")



ggsave(
  plot = heatmap_p,
  filename = str_c(local_fig_folder, "Heatwave_Heatmap.png"),
  height = unit(4, "in"),
  width = unit(4, "in"),
  dpi = "retina",
  bg = "white",
  scale = 2)








#####  Poster Sized  ######


# Saving
ragg::agg_png(
  filename = str_c(local_fig_folder, "Heatwave_Heatmap_Big.png"),
  res = 300,
  height = 8,
  width = 8,
  units = "in")
heatmap_p
dev.off()








#### Frequency, intensity, duration questions  

yr_hw_days <- region_hw %>% 
  group_by(yr) %>% 
  summarise(
    n_days = sum(mhw_event, na.rm = T),
    n_events = n_distinct(mhw_event_no, na.rm = T),
    .groups = "drop") 

# Frequency
summary(lm(n_days ~ yr, data = yr_hw_days)) # Increasing
ggplot(yr_hw_days, aes(yr, n_days)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(title = "Increase in HW Days")
summary(lm(n_events ~ yr, data = yr_hw_days)) # Increasing
ggplot(yr_hw_days, aes(yr, n_events)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(title = "Increase in HW Events")


# Intensity/Duration
hw_event_deets <- region_hw %>% 
  filter(!is.na(mhw_event_no)) %>% 
  group_by(yr, mhw_event_no) %>% 
  summarise(
    avg_intens = mean(anom_f, na.rm = T),
    peak_intens = max(anom_f, na.rm = T),
    lenth = n_distinct(time),
    .groups = "drop") %>% 
  full_join(distinct(region_hw, yr)) %>% 
  mutate(lenth = ifelse(is.na(lenth), 0, lenth))



# Intensity
summary(lm(avg_intens ~ yr, data = hw_event_deets)) # Not increasing
summary(lm(peak_intens ~ yr, data = hw_event_deets)) # Increasing
ggplot(hw_event_deets, aes(yr, peak_intens)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(title = "Increase in Peak Intensity")
summary(lm(lenth ~ yr, data = hw_event_deets)) # Increasing
ggplot(hw_event_deets, aes(yr, lenth)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(title = "Increase in HW Length")

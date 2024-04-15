# Stockholm Resilience Center Images

#Adam, 1-2 based on the Warming Report?




####  Packages  ####
{
  library(lubridate)
  library(raster)
  library(here)
  library(rnaturalearth)
  library(scales)
  library(gmRi)
  library(heatwaveR)
  library(patchwork)
  library(tidyverse)
  library(geomtextpath)
  library(sysfonts)
  library(geomtextpath)
  library(ggpubr)
  library(ggtext)
  library(sf)
}


#---------- Support Functions
suppressPackageStartupMessages( source(here::here("R/oisst_support_funs.R"), verbose = FALSE) )
suppressPackageStartupMessages( source(here("R/temp_report_support.R"), verbose = FALSE) )


# Path to the directory containing the font file (replace with your actual path)
font_dir <- paste0(system.file("stylesheets", package = "gmRi"), "/GMRI_fonts/Avenir/")



# Register the font
font_add(
  family = "Avenir",
  file.path(font_dir, "LTe50342.ttf"),
  bold = file.path(font_dir, "LTe50340.ttf"),
  italic = file.path(font_dir, "LTe50343.ttf"),
  bolditalic = file.path(font_dir, "LTe50347.ttf"))

# # Load the font
showtext::showtext_auto()


#----------- Paths
oisst_path <- cs_path("res", "OISST/oisst_mainstays")

# File paths for various extents based on "apershing_gulf_of_maine"
region_paths <- get_timeseries_paths(
  region_group = "gmri_sst_focal_areas", 
  box_location = "cloudstorage")


# Timeseries/polygon Path
timeseries_path <- region_paths[["apershing_gulf_of_maine"]][["timeseries_path"]]
gom_poly <- read_sf(region_paths[["apershing_gulf_of_maine"]][["shape_path"]])







####  Load Data  ####



# Load the timeseries
gom_timeseries <- read_csv(timeseries_path, 
                           col_types = cols(), 
                           guess_max = 1e6)


# Clean up the data - add labels
gom_timeseries <- gom_timeseries %>% 
  mutate(time = as.Date(time)) %>% 
  distinct(time, .keep_all = T) %>% 
  supplement_season_info() %>% 
  filter(year %in% c(1982:2023))



# Get heatwave statuses for each day:
# Uses area weighted sst by default
gom_hw <- pull_heatwave_events(
  temperature_timeseries = gom_timeseries, 
  threshold = 90, 
  clim_ref_period = c("1991-01-01", "2020-12-31")) %>% 
  supplement_hw_data() %>% 
  filter(doy != 366)  %>% 
  filter(between(yr, 1982, 2023))



# # Global Temperature Anomalies
global_anoms <- read_csv(
  paste0(oisst_path, "global_timeseries/global_anoms_1982to2011.csv"), 
  guess_max = 1e6,
  col_types = cols()) %>% 
  mutate(year = year(time)) %>% 
  filter(between(year, 1982, 2023)) %>% 
  supplement_season_info()




#### Prep Annual Summaries and Rates  ####


# Summarize by year to return mean annual anomalies and variance
annual_summary <- gom_hw %>% 
  group_by(year = yr) %>% 
  temperature_summaries_hw() %>% 
  mutate(yr_as_dtime = as.Date(paste0(year, "-07-02"))) %>% 
  mutate(regime = if_else(year >= 2010, gmri_cols("orange"), gmri_cols("blue")))



# Summarize global sst by year again
global_summary <- global_anoms %>% 
  group_by(year) %>% 
  temperature_summaries() %>% 
  mutate(
    yr_as_dtime = as.Date(paste0(year, "-07-02")))


#---- Celsius

# Get Rates:
gom_rates_c <- get_decadal_rates(
  temp_df = annual_summary, 
  temp_col = "area_wtd_sst", 
  year_col = "year", 
  year_lim = c(1982, 2023), 
  area_name = "Gulf of Maine", 
  degree_c = T)
world_rates_c <- get_decadal_rates(
  temp_df = global_summary, 
  temp_col = "area_wtd_sst", 
  year_col = "year", 
  year_lim = c(1982, 2023), 
  area_name = "Global SST", 
  degree_c = T)

#---- Fahrenheit

# Get Rates:
gom_rates_f <- get_decadal_rates(
  temp_df = annual_summary, 
  temp_col = "area_wtd_f", 
  year_col = "year", 
  year_lim = c(1982, 2023), 
  area_name = "Gulf of Maine", 
  degree_c = F)
world_rates_f <- get_decadal_rates(
  temp_df = global_summary, 
  temp_col = "area_wtd_f", 
  year_col = "year", 
  year_lim = c(1982, 2023), 
  area_name = "Global SST", 
  degree_c = F)





####  Inset Map  ####


# Get states
new_england <- ne_states("united states of america", returnclass = "sf")
canada      <- ne_states("canada", returnclass = "sf")




# Build Inset
gom_inset_plot <- ggplot() +
  geom_sf(data = canada, fill = "gray50", linewidth = .1, color = "white") +
  geom_sf(data = new_england, fill = "gray50", linewidth = .1, color = "white") +
  geom_sf(data = gom_poly, color = "orange", fill = "orange", alpha = 0.2, 
          linewidth = 0.8) +
  map_theme() +
  theme(
    legend.title = element_text(face = "bold", family = "Avenir"),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.justification = c(0.5,0),
    plot.background = element_blank(),
    plot.margin = margin(c(0,0,0,0)),
    panel.border = element_rect(color = "black", fill = "transparent")) +
  coord_sf(
    expand = F, 
    crs = 4326,
    xlim = c(-80, -60),
    ylim = c(35, 50))












####  Plot 1: Annual Rate Comparison  ####


# Plot
simple_c <- global_rate_comparison(
  annual_summary_dat = annual_summary, 
  global_summary_dat = global_summary, 
  eq_all = gom_rates_c$eq_label, 
  eq_global = world_rates_c$eq_label, 
  temp_units = "C", 
  region_label = "Gulf of Maine") +
  labs(caption = NULL, subtitle = "Annual Sea Surface Temperature Anomalies (1991-2020 Climatology)") +
  inset_element(
    p = gom_inset_plot, 
    left = 0.75, 
    top = 0.36, 
    bottom = 0.02,
    right =  0.95)






# Review 
simple_c





####  Plot 2: Lollipop Anomalies  ####

# This figure looks at the bump in SST that 
# occurred around 2010 using text annotations


# Lollipops Figure
shift_anom_c <- annual_summary  %>% 
  ggplot() +
  # Lollipops
  geom_segment(
    aes(x = year, xend = year, color = I(regime),
        y = 0, 
        yend = area_wtd_anom),
    #color = "gray70", 
    alpha = 0.2) +
  geom_point(aes(year, area_wtd_anom, color = I(regime)), size = 2.5) +
  # Climatological Average
  geom_texthline(
    yintercept = 0, 
    label = "1991-2020 Average",
    linewidth = 1.5, color = "black", family = "Avenir",
    size = 6, hjust = 0.15, vjust = -0.5) +
  scale_x_continuous(expand = expansion(add = c(3,3))) +
  scale_y_continuous(
    labels = label_number(suffix = deg_c),
    expand = expansion(add = c(0.5,0.65))) +
  labs(
    title = "Gulf of Maine SST's Have Rapidly Warmed in Recent Years",
    y = "Sea Surface Temperature Anomaly",
    x = "Year")





# Add annotations:

# # Warming rate:
# rate_lm <- lm(area_wtd_anom ~ year, data = annual_summary)
# rate_df <- data.frame("year" = 1983:1997)
# rate_df$y <- predict(rate_lm, rate_df) + 3.5
# 
# # rate annotation
# #rate_text <- "SST in the Gulf of Maine has been\nwarming at a rate of 0.48\u00b0C / decade\nsince 1982, 3x the global average."
# rate_text <- "SST in the Gulf of Maine has been warming at a rate of **0.48°C / decade** since 1982, a rate **~3x the global average.**"
# rate_text_df <- data.frame(x = 1991, y = 1.7, label = rate_text)


# Step Increase annotation
# where did 1.38 come from: annual_summary %>% group_by(regime) %>% summarise(sst = mean(area_wtd_sst) )
fancy_text <- "<span style = 'color:#EA4F12;'>**Since 2010, Average Annual SST's have been +1.38°C warmer.**</span>"
fancy_df <- data.frame(x = 2017.5, y = -1.9, label = fancy_text)






# Maybe a little less info on the plot
(annotated_anoms <- shift_anom_c +
  geom_textbox(
    data = mutate(fancy_df, y = 2.7),
    aes(x, y, label = label), 
    family = "Avenir", 
    fill = "transparent", 
    box.color = "transparent",
    size = 5, 
    width = unit(13, "cm")) +
  theme(axis.title = element_text(family = "Avenir", size = 16))  +
  scale_y_continuous(
    labels = label_number(suffix = deg_c),
    expand = expansion(add = c(0.5,0.7))) + 
    inset_element(
      p = gom_inset_plot, 
      left = 0.01, 
      top = 0.95, 
      bottom = 0.68,
      right =  0.22)
)








####  Plot 3:  Lollipops Temperatures  ####



# Lollipops Figure
avg_climatol <- annual_summary %>% filter(between(year, 1991,2020)) %>% pull(clim) %>% mean()
shift_temps <- annual_summary  %>% 
  ggplot() +
  geom_segment(
    aes(x = year, xend = year, color = I(regime),
        y = avg_climatol, 
        yend = area_wtd_sst),
    alpha = 0.2) +
  geom_point(aes(year, area_wtd_sst, color = I(regime)), size = 2.5) +
  # Climatological Average
  geom_texthline(
    yintercept = avg_climatol, 
    label = "1991-2020 Average",
    linewidth = 1.5, color = "black", family = "Avenir",
    size = 6, hjust = 0.15, vjust = -0.5) +
  scale_x_continuous(expand = expansion(add = c(3,3))) +
  scale_y_continuous(
    labels = label_number(suffix = deg_c),
    expand = expansion(add = c(0.5,0.65))) +
  labs(
    title = "Gulf of Maine SST's Have Rapidly Warmed in Recent Years",
    y = "Sea Surface Temperature",
    x = "Year")



# Add Annotations to temps
(annotated_temps <- shift_temps +
    geom_textbox(
      data = mutate(fancy_df, y = 13.3),
      aes(x, y, label = label), 
      family = "Avenir", 
      fill = "transparent", 
      box.color = "transparent",
      size = 5, 
      width = unit(13, "cm")) +
    theme(axis.title = element_text(family = "Avenir", size = 16))  +
    scale_y_continuous(
      labels = label_number(suffix = deg_c),
      expand = expansion(add = c(0.5,0.7))) + 
    inset_element(
      p = gom_inset_plot, 
      left = 0.01, 
      top = 0.95, 
      bottom = 0.68,
      right =  0.22)
)






####  Plot 4 - Marine Heatwaves  ####


base_date <- as.Date("2000-01-01")
grid_dat <- gom_hw %>% 
  # grid_dat <- sst_new %>% 
  mutate(
    year = year(time),
    yday = yday(time),
    flat_date = as.Date(yday-1, origin = base_date),
    yr_rev = factor(year),
    yr_rev = fct_rev(yr_rev)) 


# Set years:
hw_dat <- grid_dat %>% 
  filter(between(year, 1980, 2023)) 

# Set Legend Controls
temp_limits <- c(-3,3)
temp_suff <- deg_c

# Color limit for palettes
temp_breaks <- c(temp_limits[1], temp_limits[1]/2,  0, temp_limits[2]/2, temp_limits[2])
temp_labels <- str_c(c(str_c("< ", temp_limits[1]), temp_limits[1]/2, 0, temp_limits[2]/2, str_c("> ", temp_limits[2])), temp_suff)


(heatmap_p <- hw_dat %>% 
    ggplot(aes(x = flat_date, y = year)) +
    geom_tile(aes(fill = sst_anom), color = "transparent") +
    geom_line(
      data = filter(hw_dat, mhw_event == TRUE) %>% mutate(event_num = str_c(year, mhw_event_no)),
      aes(x = flat_date, y = year, color = "", group = event_num), linewidth = .75)  +
    scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = expansion(add = c(0,0))) +
    #annotate(x = as.Date("2000-03-20"), y = 2006, label = "text", geom = "text", family = "Avenir") +
    #geom_hline(yintercept = 2009.5, color = "black", linewidth = 1) +
    scale_y_continuous(limits = c(1981.5, 2023.5), expand = expansion(add = c(0,0))) +
    scale_fill_distiller(
      palette = "RdBu", 
      na.value = "transparent", 
      limit = temp_limits, 
      oob = scales::squish,
      breaks = temp_breaks, 
      labels = temp_labels) +
    scale_color_manual(values = "black") +
    guides(
      "fill" = guide_colorbar(
        title = "Sea Surface Temperature Anomaly", 
        title.position = "right", 
        title.hjust = 0.5,
        barheight = unit(3.5, "inches"), 
        frame.colour = "black", 
        ticks.colour = "black"),
      "color" = guide_legend(
        title = "Marine Heatwave Event", 
        override.aes = list(size = 4, color = "black"), 
        title.hjust = 0.5,
        theme = theme(
          legend.title.position = "top"), 
      )) +  
    theme(legend.title = element_text(angle = 90)) +
    labs(
    title = "Marine Heatwaves in the Gulf of Maine More Frequent and Longer Lasting",
    y = "Year",
    x = "Month",
    caption = NULL) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(angle = 0, size = 11, face = "bold"),
    plot.margin = margin(b = 0, t = 20)) +
  guides(fill = guide_colorbar(
    title = "Sea Surface Temperature Anomaly",
    title.position = "top", 
    title.hjust = 0.5, barwidth = unit(3.5, "in"), 
    barheight = unit(0.5, "cm"), 
    frame.colour = "black", 
    ticks.colour = "black")) 
  )






####  Save Them:
out_folder <- here::here("local_data/stockholm_pres_images/")

# Prevents tiny fonts when saving
showtext::showtext_opts(dpi=300) 

ggsave(
  plot = simple_c, 
  filename = str_c(out_folder, "annual_rates_plot.png"), 
  height = unit(2.5, "in"),
  width = unit(4, "in"),
  dpi = "retina", 
  bg = "transparent", 
  scale = 2.25)

ggsave(
  plot = annotated_anoms, 
  filename = str_c(out_folder, "annual_anoms_lollipops_plot.png"), 
  height = unit(2.5, "in"),
  width = unit(4, "in"),
  dpi = "retina", 
  bg = "transparent", 
  scale = 2.5)

ggsave(
  plot = annotated_temps, 
  filename = str_c(out_folder, "annual_temps_lollipops_plot.png"), 
  height = unit(2.5, "in"),
  width = unit(4, "in"),
  dpi = "retina", 
  bg = "transparent", 
  scale = 2.25)

ggsave(
  plot = heatmap_p, 
  filename = str_c(out_folder, "mhw_heatmap.png"), 
  height = unit(2.5, "in"),
  width = unit(4, "in"),
  dpi = "retina", 
  bg = "transparent", 
  scale = 2.25)


####  Gulf of Maine Annual SST
# Simple Timeseries Figures
# Date 3/1/2024



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
#showtext::showtext_auto()


#----------- Paths
oisst_path <- cs_path("res", "OISST/oisst_mainstays")

# File paths for various extents based on "apershing_gulf_of_maine"
region_paths <- get_timeseries_paths(
  region_group = "gmri_sst_focal_areas", 
  box_location = "cloudstorage")


# Timeseries Path
timeseries_path <- region_paths[["apershing_gulf_of_maine"]][["timeseries_path"]]






####  Data  ####



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
  filter(doy != 366)



# # Global Temperature Anomalies
global_anoms <- read_csv(
  paste0(oisst_path, "global_timeseries/global_anoms_1982to2011.csv"), 
  guess_max = 1e6,
  col_types = cols()) %>% 
  mutate(year = year(time)) %>% 
  filter(between(year, 1982, 2023)) %>% 
  supplement_season_info()




#### Prep  ####


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




####  Plot 1: Annual Rate Comparison  ####


#---- Celsius

# Get Rates:
gom_rates <- get_decadal_rates(
  temp_df = annual_summary, 
  temp_col = "area_wtd_sst", 
  year_col = "year", 
  year_lim = c(1982, 2023), 
  area_name = "Gulf of Maine", 
  degree_c = T)
world_rates <- get_decadal_rates(
  temp_df = global_summary, 
  temp_col = "area_wtd_sst", 
  year_col = "year", 
  year_lim = c(1982, 2023), 
  area_name = "Global SST", 
  degree_c = T)


# Plot
simple_c <- global_rate_comparison(
  annual_summary_dat = annual_summary, 
  global_summary_dat = global_summary, 
  eq_all = gom_rates$eq_label, 
  eq_global = world_rates$eq_label, 
  temp_units = "C", 
  region_label = "Gulf of Maine")  +
  coord_cartesian(clip = "off") +
  geom_logo(x_npc = 0.115, y_npc = -0.15, logo_height = 1, height_units = "cm")


#---- Fahrenheit

# Get Rates:
gom_rates <- get_decadal_rates(
  temp_df = annual_summary, 
  temp_col = "area_wtd_f", 
  year_col = "year", 
  year_lim = c(1982, 2023), 
  area_name = "Gulf of Maine", 
  degree_c = F)
world_rates <- get_decadal_rates(
  temp_df = global_summary, 
  temp_col = "area_wtd_f", 
  year_col = "year", 
  year_lim = c(1982, 2023), 
  area_name = "Global SST", 
  degree_c = F)


# Plot
simple_f <- global_rate_comparison(
  annual_summary_dat = annual_summary, 
  global_summary_dat = global_summary, 
  eq_all = gom_rates$eq_label, 
  eq_global = world_rates$eq_label, 
  temp_units = "F", 
  region_label = "Gulf of Maine")  +
  coord_cartesian(clip = "off") +
  geom_logo(x_npc = 0.115, y_npc = -0.15, logo_height = 1, height_units = "cm")








####  Plot 2: Decadal Regimes Annotated  ####


# This figure looks at the bump in SST that 
# occurred around 2010 using text annotations


# Lollipops
library(geomtextpath)
library(ggpubr)
library(ggtext)

shift_c <- annual_summary  %>% 
  ggplot() +
  # Lollipops
  geom_segment(
    aes(x = year, xend = year, color = I(regime),
        y = 0, yend = area_wtd_anom),
    #color = "gray70", 
    alpha = 0.2) +
  geom_point(aes(year, area_wtd_anom, color = I(regime)), size = 2.5) +
  # Climatological Average
  geom_texthline(
    yintercept = 0, label = "1991-2020 Average",
    linewidth = 1.5, color = "black", family = "Avenir",
    size = 6, hjust = 0.2, vjust = -0.5) +
  scale_x_continuous(expand = expansion(add = c(3,3))) +
  scale_y_continuous(
    labels = label_number(suffix = deg_c),
    expand = expansion(add = c(0.8,0.5))) +
  labs(
    title = "Gulf of Maine Rapid Warming Bolstered by Abrupt Oceanographic Changes",
    y = "Sea Surface Temperature Anomaly",
    x = "Year")
  
shift_c



# Add annotations:

# Warming rate:
rate_lm <- lm(area_wtd_anom ~ year, data = annual_summary)
rate_df <- data.frame("year" = 1984:1996)
rate_df$y <- predict(rate_lm, rate_df) + 2.8

# rate annotation
#rate_text <- "SST in the Gulf of Maine has been\nwarming at a rate of 0.48\u00b0C / decade\nsince 1982, 3x the global average."
rate_text <- "SST in the Gulf of Maine has been warming at a rate of **0.48°C / decade** since 1982, **3x the global average**."
rate_text_df <- data.frame(x = 1991, y = 1.4, label = rate_text)


# Step Increase annotation
fancy_text <- "<span style = 'color:#EA4F12;'>**Since 2010, the Gulf of Maine has been +1.38°C**</span><span> warmer than <span style = 'color:#00608A;'>**SST's during 1982-2009.**</span><span> Rapid changes linked to shifts in regional currents."
fancy_df <- data.frame(x = 2016.5, y = -1.6, label = fancy_text)

# annotate the rate change and the jump in sst
shift_annotated <- shift_c +
  geom_textline(
    data = rate_df, aes(year, y), 
    linewidth = 0.5, 
    arrow = arrow(length = unit(0.3, "cm")),
    label = "Long-Term Warming", family = "Avenir", 
    size = 5.5) +
  geom_textbox(
    data = rate_text_df, aes(x, y, label = label),
    family = "Avenir", fill = "transparent", box.color = "transparent",
    size = 5, 
    width = unit(9, "cm")) +
  geom_textbox(
    data = fancy_df, aes(x, y, label = label), 
    family = "Avenir", fill = "transparent", box.color = "transparent",
    size = 5, 
    width = unit(9.5, "cm")) +
  theme(axis.title = element_text(family = "Avenir", size = 16))  +
  coord_cartesian(clip = "off") +
  geom_logo(x_npc = 0.115, y_npc = -0.08, logo_height = 1, height_units = "cm")



shift_annotated





####  Plot 3: SST Trend + Means  ####


# Period Averages
avg_early <- filter(annual_summary, year<2010) %>% pull(area_wtd_sst) %>% mean()
avg_late <- filter(annual_summary, year>=2010) %>% pull(area_wtd_sst) %>% mean()

# as tables
early_df <- data.frame(
  x = 1981.5, 
  xend = 2009.5, 
  y = avg_early, 
  yend = avg_early)
late_df <- data.frame(
  x = 2009.5, 
  xend = 2023.5, 
  y = avg_late, 
  yend = avg_late)



  
rates_c <- annual_summary  %>% 
  ggplot() +
  # Lollipops
  geom_line(
    aes(x = year, y = area_wtd_sst),
    color = "gray30", linetype = 1, linewidth = 1.5) +
  geom_segment(data = early_df, aes(x, xend = xend, y , yend = yend), color = gmri_cols("blue"), linewidth = 1.2) +
  geom_segment(data = late_df, aes(x, xend = xend, y , yend = yend), color = gmri_cols("orange"), linewidth = 1.2) +
  geom_point(
    data = filter(annual_summary, year < 2010),
    aes(year, area_wtd_sst), color = "gray30", size = 2) +
  geom_smooth(
    aes(year, area_wtd_sst),
    method = "lm",
    formula = y ~ x,
    color = gmri_cols("gmri green"),
    se = F,
    linewidth = 2) +
  scale_x_continuous(expand = expansion(add = c(3,3))) +
  scale_y_continuous(
    labels = label_number(suffix = deg_c),
    expand = expansion(add = c(0.8,0.5))) +
  theme(axis.title = element_text(family = "Avenir", size = 16))  +
  geom_bracket(
    xmin = 1982, xmax = 2009,
    y.position = 11.1, 
    label = str_c("1982-2009 Average: ", round(avg_early, 2), "°C"),
    tip.length = 0.05, coord.flip = F,
    color = gmri_cols("blue"),
    family = "Avenir",
    label.size = 5,
    size = 0.75) +
  geom_bracket(
    xmin = 2009, xmax = 2023,
    y.position = 11.1 + (avg_late-avg_early), 
    label = str_c("2009-2023 Average: ", round(avg_late, 2), "°C"),
    tip.length = 0.05, coord.flip = F,
    color = gmri_cols("orange"),
    family = "Avenir",
    label.size = 5,
    size = 0.75) +
  labs(
    title = "Gulf of Maine SST Changes",
    y = "Sea Surface Temperature",
    x = "Year") 

rates_c


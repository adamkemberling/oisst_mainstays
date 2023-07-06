# Creating figure for National Climate Assessment, NE Chapter
# Visualize Heatwave periodicity with annual temperatures:

# NCA 5
# Figure 21.4
# Visualization Code
# Adam A. Kemberling - Gulf of Maine Research Institute

# About
# This r script details the steps needed to recreate Figure 21.4 from the 5th NCA
# This script follows the jupyter notebook for analysis of OISST data from
# Grid form to a regional timeseries. 

# The following code identifies marine heatwaves and generates the figure used:
# Paths to the timeseries will be different for each user based on local machines



####  Packages  ####
library(sf)
library(heatwaveR)
library(gmRi) # For file paths on cloud
library(tidyverse)
library(lubridate)
library(conflicted)

# Function namespace conflicts
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")




####  Identify Heatwaves  ####


# Functions for 
# Provides a wrapper for heatwaveR function that detects MHW and MCS
# Also provides functions for supplementing date information
suppressPackageStartupMessages(source(here::here("R/oisst_support_funs.R")))
suppressPackageStartupMessages(source(here::here("R/temp_report_support.R")))


# Load the timeseries for the Gulf of Maine ecological production unit
# This lazy-load function just generates paths to my timeseries by name
# For dimensions: ecodata::epu_sf %>% filter(EPU == "GOM") %>% st_bbox() # sf and ecodata packages
epu_gom <- oisst_access_timeseries(
    region_family = "epu", 
    poly_name = "gom", 
    box_location = "cloudstorage") %>% 
  mutate(tod = format(time, format = "%H:%M:%S")) 




####  Format the timeseries dataframe  ####

# Add different temporal columns using date column
# Breaks out y/m/d information into different chunks that are helpful when plotting
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




# Format timeseries for group estimates
region_timeseries <-  epu_gom %>% 
  mutate(
    time = as.Date(time)#,
    #area_wtd_f = as_fahrenheit(area_wtd_sst),
    #anom_f     = as_fahrenheit(area_wtd_anom, "anomalies")
    ) %>% 
  distinct(time, .keep_all = T) %>% 
  supplement_season_info() %>% 
  filter(year %in% c(1982:2021))



####  Get heatwave statuses for each day:  #####

# The following function wraps 2 calls of the marine heatwave detection algorithm, 
# once for heatwaves, and again for cold spells 


#' @title Pull Marine Heatwave and cold Spell Events from Timeseries
#' 
#' @description Pull both heatwave and cold spell events using same threshold and return
#' as single table. Wrapper function to do heatwaves and coldwaves simultaneously at 90% 
#' or custom threshold
#' 
#' Option to de-trend anomalies at annual scale in accordance with Jacox et al. methodology. Default
#' is not de-trended and uses a statid climate reference period following the methods of hobday et al.
#'
#' @param temperature_timeseries timeseries dataframe with date and sst values
#' @param clim_ref_period start and end dates to use when calculating the climate reference 
#' period c("yyyy-mm-dd", "yyyy-mm-dd")
#' @date_col String indicating the column to use for dates
#' @temp_col String indicating the column to de-trend
#' @param threshold percentile cutoff for indicating a heatwave/coldspell event
#' @param detrend TRUE/FALSE Whether to de-trend anomalies prior to event detection, default is FALSE.
#'
#' @return
#' @export
#'
#' @examples
pull_heatwave_events <- function(temperature_timeseries, 
                                 clim_ref_period = c("1982-01-01", "2011-12-31"),
                                 date_col = "time",
                                 temp_col = "sst",
                                 threshold = 90,
                                 detrend = FALSE) {
  
  # temperature_timeseries <- gom_sst
  
  
  # Pull the two column dataframe for mhw estimation
  test_ts <- data.frame(t = as.Date(temperature_timeseries[[date_col]]), 
                        temp = temperature_timeseries[[temp_col]])
  
  
  # Calculate seasonally varying climatology with threshold w/ smoothing window
  ts  <- heatwaveR::ts2clm(data = test_ts, 
                           climatologyPeriod = clim_ref_period, 
                           pctile = threshold) %>% 
    mutate(sst_anom = temp - seas,
           yr = lubridate::year(t))
  
  
  
  # Perform linear detrending on anomalies
  if(detrend){
    
    # Detrend day of year temperature trends:
    ts <- ts %>% 
      split(.$doy) %>% 
      map_dfr(detrend_sst, vals = "sst_anom", yr_col = "yr") %>% 
      mutate(detrend_temp = seas + detrend_vals) %>% 
      arrange(t)
    
  }
  
  
  # Perform Heatwave Detection
  mhw <- ifelse(detrend,
                heatwaveR::detect_event(ts, x = t, y = detrend_temp),
                heatwaveR::detect_event(ts, x = t, y = temp))
  
  
  
  # Select and rename critical heatwave data
  mhw_out <- mhw[[1]] %>% 
    #mutate(sst_anom = temp - seas) %>% 
    rename(time = t,
           sst = temp,
           mhw_thresh = thresh,
           mhw_threshCriterion = threshCriterion,
           mhw_durationCriterion = durationCriterion,
           mhw_event = event,
           mhw_event_no = event_no)
  
  
  # Repeat for cold spells
  # 2. Detect cold spells
  # coldSpells = TRUE flips boolean to < thresh
  ts <- ts2clm(data = test_ts, 
               climatologyPeriod = clim_ref_period, 
               pctile = (100 - threshold)) %>% 
    mutate(sst_anom = temp - seas,
           yr = lubridate::year(t))
  
  
  # Perform linear detrending on anomalies
  if(detrend){
    
    # Detrend day of year temperature trends:
    ts <- ts %>%
      split(.$doy) %>%
      map_dfr(detrend_sst, vals = "sst_anom", yr_col = "yr") %>%
      mutate(detrend_temp = seas + detrend_vals) %>% 
      arrange(t)
    
  }
  
  
  
  # Perform Cold Spell Detection
  mcs <- ifelse(detrend,
                heatwaveR::detect_event(ts, x = t, y = detrend_temp, coldSpells = T),
                heatwaveR::detect_event(ts, x = t, y = temp, coldSpells = T))
  
  
  
  # Prepare cold spell data to join
  # Remove columns that are shared with heatwaves
  mcs_out <- mcs[[1]] %>%
    dplyr::select(time = t,
                  mcs_thresh = thresh,
                  mcs_threshCriterion = threshCriterion,
                  mcs_durationCriterion = durationCriterion,
                  mcs_event = event,
                  mcs_event_no = event_no)
  
  
  # join heatwave detection results to coldspell results
  hot_and_cold <- left_join(mhw_out, mcs_out, by = "time")
  
  
  # 3. Data formatting for plotting, 
  # adds columns to plot hw and cs seperately
  events_out <- hot_and_cold %>% 
    mutate(
      # Set up status to combine labelling for heatwaves and cold spells:
      status   = ifelse(mhw_event == TRUE, "Marine Heatwave Event", "Sea Surface Temperature"),
      status   = ifelse(mcs_event == TRUE, "Marine Cold Spell Event", status),
      event_type = ifelse(detrend, "Jacox Method", "Hobday Method"),
      # Corrective measures for where event flagging is off:
      # status   = ifelse(sst > mhw_thresh, "Marine Heatwave Event", status),
      # status   = ifelse(sst < mcs_thresh, "Marine Cold Spell Event", status),
      # Heatwave event temperature values:
      hwe      = ifelse(mhw_event == TRUE, sst, NA),
      cse      = ifelse(mcs_event == TRUE, sst, NA),
      nonevent = ifelse(mhw_event == FALSE & mcs_event == FALSE, sst, NA)) 
  
  # Close the gaps between a mhw event and sst (might not need if full line for temp exists)
  events_out <- events_out %>% 
    mutate(hwe = ifelse( (is.na(hwe) & is.na(lag(hwe, n = 1))) == FALSE, sst, hwe),
           cse = ifelse( (is.na(cse) & is.na(lag(cse, n = 1))) == FALSE, sst, cse)) %>% 
    distinct(time, .keep_all = T)
  
  
  return(events_out)
}







# Note: 
# pull_heatwave_eventsUses area weighted sst by default
region_hw <- region_timeseries %>% 
  pull_heatwave_events(
  temperature_timeseries = ., 
  threshold = 90, 
  clim_ref_period = c("1982-01-01", "2011-12-31")) %>% 
  supplement_hw_data() %>% 
  filter(doy != 366) %>% 
  mutate(year = year(time),
         yday = yday(time),
         yr_rev = factor(year),
         yr_rev = fct_rev(yr_rev),
         flat_date = as.Date("2000-01-01") + yday - 1)



# Process annual summaries into points with specific dates
# This lets them be placed on a datetime axis
hw_annual <- region_hw %>% 
  group_by(yr) %>% 
  summarise(
    sst_anom = mean(sst_anom),
    anom_f = mean(anom_f),
    .groups = "drop") %>% 
  mutate(yr_as_dt = as.Date(str_c(yr, "-06-15")))




# Plot the hw days below the lineplot as hash marks:
fig <-  ggplot() +
    geom_line(data = region_hw, aes(time, anom_f), color = "gray60", alpha = 0.5) +
    geom_line(data = hw_annual, aes(yr_as_dt, anom_f), linewidth = 1) +
    geom_segment(data = filter(region_hw, mhw_event),
                aes(x = time, xend = time,
                    y = -4.6, yend = -4.3), 
                linewidth = .2, color = "darkred") +
    scale_y_continuous(labels = scales::number_format(suffix = " \u00b0F")) +
    theme(plot.caption = element_text(hjust = 0)) + # set the left align for caption
    labs(x = "Date", 
         y = "Temperature Anomaly",
         title = "Gulf of Maine SST Anomalies",
         caption = "")
  


# Display the figure
fig


# Save the figure(s)
ggsave(
  plot = fig, 
  filename = here::here("local_data/", str_c(label_y,"_sst_anomalies.svg"))
  )
  
  





####  Figure Formatting Tests:  ####

# # Just change the line color
# ggplot() +
#   geom_line(data = region_hw, aes(time, sst_anom, color = mhw_event, group = 1), alpha = 0.5) +
#   geom_line(data = hw_annual, aes(yr_as_dt, sst_anom), size = 1) +
#   scale_color_manual(values = c("TRUE" = "darkred", "FALSE" = "gray60"))  +
#   labs(x = "Date", y = "SST Anom")

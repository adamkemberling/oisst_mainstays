####  Support Functions for the Regional Temperature Report



####  Convert Raster to Discrete Scale  ####
 
#' @title Reclassify Raster to Discrete Bins
#' 
#' @description Convenience function to convert warming rank and rate raster stacks to a discrete
#' scale. Also sets values below a cutoff to NA.
#' source code from here:
#' https://www.earthdatascience.org/courses/earth-analytics/lidar-raster-data-r/classify-raster/ 
#'
#' @param ranks_stack Raster stack of rank values to reclassify
#' @param rates_stack Raster stack of warming rates to reclassify
#' @param percentile_cutoff Threshold for relabeling values to NA
#'
#' @return
#' @export
#'
#' @examples
reclassify_to_discrete <- function(ranks_stack, 
                                   rates_stack, 
                                   percentile_cutoff = 80){
  
  # create classification matrix
  reclass_df <- c(0.00, 0.05,  0, #####_reclassification bins####
                  0.05, 0.10,  5,
                  0.10, 0.15, 10,
                  0.15, 0.20, 15,
                  0.20, 0.25, 20,
                  0.25, 0.30, 25,
                  0.30, 0.35, 30,
                  0.35, 0.40, 35,
                  0.40, 0.45, 40,
                  0.45, 0.50, 45,
                  0.50, 0.55, 50,
                  0.55, 0.60, 55,
                  0.60, 0.65, 60,
                  0.65, 0.70, 65,
                  0.70, 0.75, 70,
                  0.75, 0.80, 75,
                  0.80, 0.85, 80,
                  0.85, 0.90, 85,
                  0.90, 0.95, 90,
                  0.95,    1, 95)
  #####_ end bins ####
  
  
  # reshape the object into a matrix with columns and rows
  reclass_m <- matrix(reclass_df,
                      ncol = 3,
                      byrow = TRUE)
  
  # reclassify the raster using the reclass object - reclass_m
  ranks_classified <- reclassify(ranks_stack,
                                 reclass_m)
  
  # Save the un-masked layers as stars objects
  rates_raw_st <- st_as_stars(rotate(rates_stack))
  ranks_raw_st <- st_as_stars(rotate(ranks_stack))
  
  # Masking Below percentile cutoff - for ranking raster and rates raster
  ranks_stack[ranks_classified < percentile_cutoff] <- NA
  rates_stack[ranks_classified < percentile_cutoff] <- NA
  
  # Converting to stars
  rates_st   <- st_as_stars(rotate(rates_stack))
  ranks_st   <- st_as_stars(rotate(ranks_stack))
  ranks_c_st <- st_as_stars(rotate(ranks_classified))
  
  # #get scale ranges so they still pop
  # rate_range <- c("min" = cellStats(rates_stack, 'min')[1], 
  #                 "max" = cellStats(rates_stack,'max')[1])
  
  # Return the three stars objects
  return(list("rates_raw" = rates_raw_st,
              "ranks_raw" = ranks_raw_st,
              "rates" = rates_st, 
              "ranks" = ranks_st, 
              "ranks_discrete" = ranks_c_st))
  
}


####_____________________________####




#' @title Detrend Annual Values
#' 
#' @ Removes linear trend from a timeseries of annual values. Model is 
#' a linear regression using lm(), returns original dataframe with new 
#' column for detrended values `detrend_vals`.
#'
#' @param x dataframe containing data for the model
#' @param vals string indicating the column for the response variable to detrend
#' @param yr_col string indicating the column for the independent variable, typically a year integer
#'
#' @return
#' @export
#'
#' @examples
detrend_sst <- function(x, vals, yr_col){
  
  # Get the trend
  linear_trend <- lm(formula(str_c(vals,"~", yr_col)), data = x)
  
  # remove trend from the data
  trend_pred <- predict(linear_trend, x)
  x$detrend_vals <- x[[vals]] - trend_pred
  return(x)
  
  
}


####  Identify Marine Heatwaves  ####
# 
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
  ts  <- ts2clm(data = test_ts, 
                climatologyPeriod = clim_ref_period, 
                pctile = threshold) %>% 
    mutate(anom = temp - seas,
           yr = lubridate::year(t))
  
  
  
  # Perform linear detrending on anomalies
  if(detrend){
    
    # Detrend day of year temperature trends:
    ts <- ts %>% 
      split(.$doy) %>% 
      map_dfr(detrend_sst, vals = "anom", yr_col = "yr") %>% 
      mutate(detrend_temp = seas + detrend_vals)
    
  }
  
  
  # Perform Heatwave Detection
  mhw <- ifelse(detrend,
                detect_event(ts, x = t, y = detrend_temp),
                detect_event(ts, x = t, y = temp))
  
  
  
  # Select and rename critical heatwave data
  mhw_out <- mhw[[1]] %>% 
    mutate(sst_anom = temp - seas) %>% 
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
    mutate(anom = temp - seas,
           yr = lubridate::year(t))
  
  
  # Perform linear detrending on anomalies
  if(detrend){
    
    # Detrend day of year temperature trends:
    ts <- ts %>%
      split(.$doy) %>%
      map_dfr(detrend_sst, vals = "anom", yr_col = "yr") %>%
      mutate(detrend_temp = seas + detrend_vals)
    
  }
  
  
  
  # Perform Cold Spell Detection
  mcs <- ifelse(detrend,
                detect_event(ts, x = t, y = detrend_temp, coldSpells = T),
                detect_event(ts, x = t, y = temp, coldSpells = T))
  
  
  
  # Prepare cold spell data to join
  # Remove columns that are shared with heatwaves
  mcs_out <- mcs[[1]] %>%
    select(time = t,
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



####  Plotly Heatwave Plot  ####

# Helper function for plotting
plotly_mhw_plots <- function(data){
  
  # How to get rgb() from colors
  plot_cols <- list(
    "gray20"    = col2rgb(col = "gray20"),
    "gray40"    = col2rgb(col = "gray40"),
    "royalblue" = col2rgb(col = "royalblue"),
    "darkred"   = col2rgb(col = "darkred"),
    "lightblue" = col2rgb(col = "lightblue"))
  
  
  
  
  # Building the plot
  fig <- plot_ly(data, x = ~time) 
  
  # Sea Surface Temperature
  fig <- fig %>% add_trace(y = ~sst, 
                           name = 'Sea Surface Temperature',
                           mode = 'lines', 
                           type = "scatter",
                           line = list(color = "rgb(65, 105, 225)", 
                                       width = 2)) 
  # Heatwave Threshold
  fig <- fig %>% add_trace(y = ~mhw_thresh, 
                           name = 'MHW Threshold', 
                           mode = 'lines', 
                           type = "scatter",
                           line = list(color = "rgb(205, 91, 69)", #coral3
                                       #line = list(color = "rgb(255, 99, 71)", #tomato 
                                       #line = list(color = "rgb(51, 51, 51)", 
                                       width = 1, 
                                       dash = 'dot')) 
  # Seasonal Climatology
  fig <- fig %>% add_trace(y = ~seas, 
                           name = 'Daily Climatology', 
                           mode = 'lines', 
                           type = "scatter",
                           line = list(color = "rgb(102, 102, 102)", 
                                       width = 3, 
                                       dash = 'dash')) 
  # Marine Cold Spell Threshold
  fig <- fig %>% add_trace(y = ~mcs_thresh, 
                           name = 'MCS Threshold', 
                           mode = 'lines', 
                           type = "scatter",
                           line = list(color = "rgb(135, 206, 235)", #skyblue
                                       #line = list(color = "rgb(51, 51, 51)", 
                                       width = 1, 
                                       dash = 'dot')) 
  # Heatwave Event
  fig <- fig %>% add_trace(y = ~hwe, 
                           name = 'Marine Heatwave Event', 
                           mode = 'lines', 
                           type = "scatter",
                           line = list(color = "rgb(139, 0, 0)", 
                                       width = 2)) 
  
  # Cold Spell Event
  fig <- fig %>% add_trace(y = ~cse, 
                           name = 'Marine Cold Spell Event ', 
                           mode = 'lines', 
                           type = "scatter",
                           line = list(color = "rgb(65, 105, 225)", 
                                       width = 2)) 
  
  # Axis Formatting
  fig <- fig %>% plotly::layout(xaxis = list(title = ""),
                                yaxis = list (title = "Temperature (degrees C)"))
  
  
  # Legend formatting
  fig <- fig %>% plotly::layout(legend = list(orientation = 'h'))
  
  
  return(fig)
}




####  Make Fahrenheit

# Plotting Function
plot_mhw <- function(timeseries_data){
  
  
  # Set colors by name
  color_vals <- c(
    "Sea Surface Temperature" = "royalblue",
    "Heatwave Event"          = "darkred",
    "Cold Spell Event"        = "lightblue",
    "MHW Threshold"           = "gray30",
    "MCS Threshold"           = "gray30",
    "Daily Climatology"       = "gray30")
  
  
  # Set the label with degree symbol
  ylab <- expression("Sea Surface Temperature"~degree~C)
  
  
  
  # Plot the last 365 days
  p1 <- ggplot(timeseries_data, aes(x = time)) +
    geom_segment(aes(x = time, xend = time, y = seas, yend = sst), 
                 color = "royalblue", alpha = 0.25) +
    geom_segment(aes(x = time, xend = time, y = mhw_thresh, yend = hwe), 
                 color = "darkred", alpha = 0.25) +
    geom_line(aes(y = sst, color = "Sea Surface Temperature")) +
    geom_line(aes(y = hwe, color = "Heatwave Event")) +
    geom_line(aes(y = cse, color = "Cold Spell Event")) +
    geom_line(aes(y = mhw_thresh, color = "MHW Threshold"), lty = 3, size = .5) +
    geom_line(aes(y = mcs_thresh, color = "MCS Threshold"), lty = 3, size = .5) +
    geom_line(aes(y = seas, color = "Daily Climatology"), lty = 2, size = 1) +
    scale_color_manual(values = color_vals) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme(legend.title = element_blank(),
          legend.position = "top") +
    labs(x = "", 
         y = ylab, 
         caption = paste0("Climate reference period :  1982-2011"))
  
  
  return(p1)
}

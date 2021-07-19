####  Support Funcitons for the Regional Temperature Report



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
  #####_####
  
  
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



####  Identify Marine Heatwaves  ####
# Wrapper function to do heatwaves and coldwaves simultaneously at 90%
#' @title Pull Marine Heatwave and cold Spell Events from Timeseries
#' 
#' @description Pull both heatwave and cold spell events using same threshold and return
#' as single table
#'
#' @param temperature_timeseries timeseries dataframe with date and sst values
#' @param threshold percentile cutoff for indicating a heatwave/coldspell event
#'
#' @return
#' @export
#'
#' @examples
pull_heatwave_events <- function(temperature_timeseries, 
                                 threshold = 90, 
                                 clim_ref_period = c("1982-01-01", "2011-12-31")) {
  
  # Pull the two column dataframe for mhw estimation
  test_ts <- data.frame(t    = temperature_timeseries$time, 
                        temp = temperature_timeseries$sst)
  
  # Detect the events in a time series
  ts  <- ts2clm(data = test_ts, 
                climatologyPeriod = clim_ref_period, 
                pctile = threshold)
  
  #heatwaves
  mhw <- detect_event(ts)                         
  
  # prep heatwave data
  mhw_out <- mhw$climatology %>% 
    mutate(sst_anom = temp - seas) %>% 
    select(time = t,
           sst = temp,
           seas,
           sst_anom,
           mhw_thresh = thresh,
           mhw_event = event,
           mhw_event_no = event_no)
  
  
  # 2. Detect cold spells
  # coldSpells = TRUE flips boolean to < thresh
  ts <- ts2clm(data = test_ts, 
               climatologyPeriod = clim_ref_period, 
               pctile = (100 - threshold))
  mcs <- detect_event(ts, coldSpells = TRUE) 
  
  # prep cold spell data
  mcs_out <- mcs$climatology %>%
    select(time = t,
           mcs_thresh = thresh,
           mcs_event = event,
           mcs_event_no = event_no)
  
  
  # join heatwaves to coldwaves
  hot_and_cold <- left_join(mhw_out, mcs_out, by = "time")
  
  
  
  # 3. Data formatting for plotting, 
  # adds columns to plot hw and cs seperately
  events_out <- hot_and_cold %>% 
    mutate(status   = ifelse(mhw_event == TRUE, "Marine Heatwave Event", "Sea Surface Temperature"),
           status   = ifelse(mcs_event == TRUE, "Marine Cold Spell Event", status),
           hwe      = ifelse(mhw_event == TRUE, sst, NA),
           cse      = ifelse(mcs_event == TRUE, sst, NA),
           nonevent = ifelse(mhw_event == FALSE & mcs_event == FALSE, sst, NA)) 
  
  # Close the gaps between a mhw event and sst (might not need if full line for temp exists)
  events_out <- events_out %>% 
    mutate(hwe = ifelse(is.na(hwe) & is.na(lag(hwe)) == FALSE, sst, hwe),
           cse = ifelse(is.na(cse) & is.na(lag(cse)) == FALSE, sst, cse))%>% 
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
  fig <- fig %>% layout(xaxis = list(title = ""),
                        yaxis = list (title = "Temperature (degrees C)"))
  
  
  # Legend formatting
  fig <- fig %>% layout(legend = list(orientation = 'h'))
  
  
  return(fig)
}




####  Make Fahrenheit
# Get Fahrenheit from Celsius
as_farenheit <- function(x){x * (9/5) + 32}

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

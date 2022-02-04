# Belkin O'reilly fonts, removed fom regional temperature report:
# followed heatwave progression code

#### Front Progression

#Same idea as above but looking at the Belkin O'Reilly fronts rather than absolute values. Under development.

# Use belkin fronts function to get the sst fronts
this_yr_fronts <- map(unstack(this_yr), get_belkin_fronts) %>% 
  stack() %>% 
  setNames(names(this_yr))

# Set palette limits to center it on 0 with scale_fill_distiller
limit <- c(max(values(this_yr_fronts), na.rm = T) * -1, 
           max(values(this_yr_fronts), na.rm = T) )


# Build Plots for Animation

# Plot Heatwave 1 day at a time as a GIF
front_plots <- imap(day_count, function(date_index, date_label) {
  
  # grab dates
  sst_fronts_st  <- st_as_stars(this_yr_fronts[[date_index]])
  
  #### 1. Map the Anomalies in Space
  day_plot <- ggplot() +
    geom_stars(data = sst_fronts_st) +
    geom_sf(data = new_england, fill = "gray90", size = .25) +
    geom_sf(data = canada, fill = "gray90", size = .25) +
    geom_sf(data = greenland, fill = "gray90", size = .25) +
    geom_sf(data = region_extent, 
            color = gmri_cols("gmri blue"), 
            linetype = 2, size = 1,
            fill = "transparent") +
    scale_fill_distiller(palette = "RdYlBu", 
                         na.value = "transparent",
                         limit = limit) +
    map_theme +
    coord_sf(xlim = crop_x, 
             ylim = crop_y, 
             expand = T) +
    guides("fill" = guide_colorbar(
      title = "Front Strength?",
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(3, "in"),
      frame.colour = "black",
      ticks.colour = "black")) 
  
  
  
  
  #### 2.  Plot the day and the overall anomaly to track dates
  date_timeline <- ggplot(data = hw_timeline, aes(x = time)) +
    geom_line(aes(y = sst, color = "Sea Surface Temperature")) +
    geom_line(aes(y = hwe, color = "Heatwave Event")) +
    geom_line(aes(y = cse, color = "Cold Spell Event")) +
    geom_line(aes(y = mhw_thresh, color = "MHW Threshold"), lty = 3, size = .5) +
    geom_line(aes(y = mcs_thresh, color = "MCS Threshold"), lty = 3, size = .5) +
    geom_line(aes(y = seas, color = "Daily Climatology"), lty = 2, size = .5) +
    scale_color_manual(values = color_vals) +
    
    # Animated Point /  line
    geom_point(
      data = filter(hw_timeline, time == as.Date(date_label)),
      aes(time, sst, shape = factor(mhw_event)), 
      color = gmri_cols("gmri blue"), 
      size = 3, show.legend = FALSE) + 
    geom_vline(data = filter(hw_timeline, time == as.Date(date_label)),
               aes(xintercept = time), 
               color = "gray50", 
               size = 0.5,
               linetype = 3,
               alpha = 0.8) + 
    labs(x = "", 
         y = "",
         color = "",
         subtitle = "Regional Temperature \u00b0C",
         shape = "Heatwave Event") +
    theme(legend.position = "bottom")
  
  
  ####  3. Assemble plot(s)
  p_layout <- c(
    area(t = 1, l = 1, b = 2, r = 8),
    area(t = 3, l = 1, b = 8, r = 8))
  
  # plot_agg <- (date_timeline / day_plot) + plot_layout(heights = c(1, 3))
  plot_agg <- date_timeline + day_plot + plot_layout(design = p_layout)
  
  
  return(plot_agg)
  
  
})


walk(front_plots, print)


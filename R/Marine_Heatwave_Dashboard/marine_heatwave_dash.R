#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Author : Adam Kemberling
# Date: 4/14/2021
# 
# NOTE: BROKEN
# Process All the shapes ahead of time
# Then Prepare the look up lists

####  Packages  ####
library(lubridate)
library(rnaturalearth)
library(sf)
library(gmRi)
library(here)
library(knitr)
library(heatwaveR)
library(plotly)
library(shinymaterial)
library(tidyverse)
library(shiny)

# Support Functions
source(here("R/oisst_support_funs.R"))
source(here("R/temp_report_support.R"))
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")

#box paths
box_paths <- research_access_paths(mac_os = "mojave")

# File Paths
mills_path <- box_paths$mills
res_path   <- box_paths$res
okn_path   <- box_paths$okn  
oisst_path <- box_paths$oisst_mainstays

# Set ggplot theme for figures
theme_set(theme_bw())


# Polygons for mapping
world <-  ne_countries() %>% st_as_sf(crs = 4326)

# Build a Base map
world_map <- ggplot() +
    geom_sf(data = world, fill = "gray80", color = "white", size = 0.1) +
    coord_sf(expand = FALSE) +
    map_theme(axis.text.x = element_blank(),
              axis.text.y = element_blank())

# Function to create lme bounding box to draw eyes over to region
sf_to_rect <- function(sf_obj) {
    bbox_obj <- st_bbox(sf_obj)
    xmin <- as.numeric(bbox_obj[1]) - 5
    ymin <- as.numeric(bbox_obj[2]) - 5
    xmax <- as.numeric(bbox_obj[3]) + 5
    ymax <- as.numeric(bbox_obj[4]) + 5
    
    bbox_df <- tribble(
        ~"lon", ~"lat",
        xmin,   ymin,
        xmin,   ymax,
        xmax,   ymax,
        xmax,   ymin,
        xmin,   ymin
    )
    
    #Turn them into polygons
    area_polygon <- bbox_df %>%
        select(lon, lat) %>% 
        as.matrix() %>% 
        list() %>% 
        st_polygon()
    
    #And then make the sf object  from the polygons
    sfdf <- st_sf(st_sfc(area_polygon), crs = 4326)
    return(sfdf)
}



####________________________####
####   Pre-Load Data  ####



#### 1. Group Selection Choices  ####
# Build List of Regional Timeline Resources
region_groups <- list(#"A. Allyn: NELME Regions" = "nelme_regions",
                   "GMRI: SST Focal Areas"   = "gmri_sst_focal_areas", 
                   "NMFS Trawl Regions"      = "nmfs_trawl_regions",
                   "Large Marine Ecosystems" = "lme"
                   )



#### 2. Sub-Group Choices  ####

# Get the sub Groups - set names to values of group selections
region_list <- map(region_groups, function(region_group){
    region_choices <- gmRi::get_region_names(region_group)
    region_choice_names <- str_replace_all(region_choices, "_", " ")
    region_choice_names <- str_to_title(region_choice_names)
    region_choice_selection <- setNames(region_choices, region_choice_names)
    return(region_choice_selection)
}) %>% setNames(region_groups)


#### 3. Timeseries Data  ####


# Get the timeseries paths for each group
region_paths_l <- map(region_groups, function(shape_fam){
  paths <- get_timeseries_paths(shape_fam, mac_os = "mojave")
  return(paths)
})  %>% setNames(region_groups)


# get the timeseries data
region_timeseries_l <- map(region_paths_l, function(file_paths){
  
  # Go through each path and load the timeseries
  ts_data <- map(file_paths, function(x){
    
    # Read each timeseries in the group
    single_ts <- read_csv(x[["timeseries_path"]], 
    col_types = list(
      col_datetime(), 
      col_double(),
      col_double(),
      col_double(),
      col_double(),
      col_double(),
      col_double(),
      col_double(),
      col_double()))
    return(single_ts)
  })
  
  # Return the list of timeseries for the group
  return(ts_data)
})



# Make the interactive plots
timeseries_list <- map(region_timeseries_l, function(x){

  # Go through each group and make plots
  plot_list <- map(x, function(x2){
    
    # Find heatwaves
    region_hw <- pull_heatwave_events(
      temperature_timeseries = drop_na(x2), 
      threshold = 90, 
      clim_ref_period = as.Date(c("1982-01-01", "2011-12-31"))) %>% 
      supplement_hw_data() %>% 
      filter(doy != 366)
    
    # Make interactive plot
    plotly_out <- plotly_mhw_plots(region_hw)
    return(plotly_out)
    
    
  })

  # Return the list of plots for that group
  return(plot_list)

})






#### 4. Masking Polygon Maps  ####

# get the polygons
region_shapes_l <- map(region_paths_l, function(shape_paths){
  shapes <- map(shape_paths, ~read_sf(.x[["shape_path"]]))
  return(shapes)
})


# make a bunch of maps
region_maps <- map(region_shapes_l, function(group_fam){

  # repeat for each polygon in the group
  imap(group_fam, function(shape_poly, region_name){

    # title formatting
    area_title <- str_replace_all(region_name, "_", " ")
    area_title <- str_to_title(area_title)

    # Build Map of Extent
    lme_bb <- sf_to_rect(shape_poly)

    # Build Plot
    extent_map <- world_map +
      geom_sf(data = shape_poly,
              fill = gmri_cols("gmri blue"),
              color = gmri_cols("gmri_blue"),
              alpha = 0.7) +
      geom_sf(data = lme_bb,
              color = gmri_cols("orange"),
              fill = "transparent",
              size = 1) +
      labs(title = area_title) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, color = "gray10"))


    p <- extent_map
    return(p)


  })

}) %>% setNames(as.character(region_groups))


####________________________####

####____User Interface____####
ui <- material_page(
    nav_bar_fixed = TRUE,
    primary_theme_color = "#00695c", 
    secondary_theme_color = "#00796b",
    
    # Application title
    title = "Sea Surface Temperature Trends of Earth's Large Marine Ecosystems (and more!)",
    
    
    ####  Side Bar Navigation  ####
    material_side_nav(
        fixed = FALSE,
        tags$h5(tags$strong("Choosing a Region"), align = "center"),
        tags$p("Every region in this application corresponds to a large marine ecosystem (LME)."),
        tags$p("Large marine ecosystems (LMEs) are areas of coastal oceans delineated on the basis of
                ecological characteristics—bathymetry, hydrography, productivity, and trophically linked
                populations (Sherman and Alexander, 1986)."),
        tags$p("Select any LME from the list below to see how sea surface temperature anomalies have
               changed over time and how that LME compares up to others around the globe."),
        tags$br(),
        
        
        ##### 1. Region Group  ####
        material_dropdown(input = "Region_Family", 
                          label = "Select a Collection of Regions", 
                          choices = region_groups, 
                          selected = "GMRI: SST Focal Areas"),
        
        
        ##### 2. Region Choices  ####
        uiOutput("region_choice_reactive"),
        
        tags$br(),
        tags$p("The selected Area's polygon information is accessed directly 
               using the {gmRi} package")
        
    ),
    
    
    ####  Content Tabs  ####
    
    material_tabs(
        tabs = c(
            "Selected Large Marine Ecosystem"   = "first_tab",
            "Marine Heatwave timelines"         = "second_tab"#,
            #"Compare Climate Metrics"           = "third_tab"
        )
    ),
    
    
    ####__ Tab 1 - Map of Region  ####
    material_tab_content(
        tab_id = "first_tab",
        material_card(
            title = "Currently Selected Large Marine Ecosystem:",
            plotOutput("world_map") ) ),
    
    
    
    ####__ Tab 2 - SST Anomalies  ####
    material_tab_content(
        tab_id = "second_tab",
        material_card(
            title = "Tracking Deviations from the Climate-Mean",
            plotlyOutput("plotly_anomaly_timeline") ) )
    
    
)



####________________________####


####____  Input Testing  ____####
# input <- list("Region_Family" = "gmri_sst_focal_areas",
#               "Region_Choice" = "apershing_gulf_of_maine")
# 
# get_timeseries_paths(region_group = input$Region_Family)








####________________________####
####_____Server____####
server <- function(input, output, session) {
    
    
    # Could be faster to just load them as we go since there are now like a million:
    # Use the Shiny Inputs to Get the paths for the Regional Timeseries and shape
    
    
    ####  Reactive UI elements  ####
    
    # reactive value storage the region choices
    reactive_sub_regions <- reactiveValues(subregion_choices = NULL)
  
  
    # observe the region family, assign the subregion choices
    observeEvent(input$Region_Family, {
      region_choices      <- region_list[[input$Region_Family]]
      region_names_pretty <- str_replace_all(region_choices, "_", " ")
      region_names_pretty <- str_to_title(region_names_pretty)
      region_choices      <- setNames(region_choices, region_names_pretty)
      reactive_sub_regions$subregion_choices <- region_choices
    })
    
    
    # Generate the Reactive UI Here
    output$region_choice_reactive <- renderUI({
        render_material_from_server(
            material_dropdown(input_id = "Region_Choice",
                              choices = reactive_sub_regions$subregion_choices,
                              label = "Target Area:")
        )
        

    })
    
    
    
    # Reactive Data for Timeline and Plot
    #If everything goes as planned this should get us the timeseries and shapefile info
    
    
    
    
    ####  WORKING HERE  ####

    
    ####  Region Map  ####
    output$world_map <- renderPlot({
        
        # ORIGINAL MAPPING CODE, moved out of server
        
        # NEW MAP CODE Grab Map that was pre-made on startup
        extent_map <- region_maps[[input$Region_Family]][[input$Region_Choice]]  
      
        p <- extent_map
        return(p)
        
    })
    
    
    
    
    
    ####  Anomaly Timeline  ####
    output$plotly_anomaly_timeline <- renderPlotly({
        
        # Plotly Timelines
        timeseries_list[[input$Region_Family]][[input$Region_Choice]]
    })
    
    
    
    
    
    
    
    
    
}




#---- Run the application ----
shinyApp(ui = ui, server = server)

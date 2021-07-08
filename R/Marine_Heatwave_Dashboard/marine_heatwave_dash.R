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
library(janitor)
library(knitr)
library(patchwork)
library(tidyverse)
library(heatwaveR)
library(plotly)
library(ggpmisc)
library(shinymaterial)
library(shiny)

# Support Functions
source(here("R/oisst_support_funs.R"))

#box paths
box_paths <- research_access_paths()

# File Paths
mills_path <- box_paths$mills
res_path   <- str_replace(box_paths$res, "RES Data", "RES_Data")
okn_path   <- box_paths$okn  
oisst_path <- paste0(res_path, "OISST/oisst_mainstays/")

# Set ggplot theme for figures
theme_set(theme_bw())

# Set theme up for maps
map_theme <- list(
    theme(
        panel.border       = element_rect(color = "black", fill = NA),
        plot.background    = element_rect(color = "transparent", fill = "transparent"),
        line               = element_blank(),
        axis.title.x       = element_blank(), # turn off titles
        axis.title.y       = element_blank(),
        legend.position    = "bottom", 
        legend.title.align = 0.5))


# Polygons for mapping
world <-  ne_countries() %>% st_as_sf(crs = 4326)

world_map <- ggplot() +
    geom_sf(data = world, fill = "gray80", color = "white", size = 0.1) +
    map_theme

# create lme bounding box tool
sf_to_rect <- function(sf_obj) {
    bbox_obj <- st_bbox(sf_obj)
    xmin = as.numeric(bbox_obj[1]) - 5
    ymin = as.numeric(bbox_obj[2]) - 5
    xmax = as.numeric(bbox_obj[3]) + 5
    ymax = as.numeric(bbox_obj[4]) + 5
    
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
    sfdf <- st_sf(area = sf_obj$name[1], st_sfc(area_polygon), crs = 4326)
    return(sfdf)
}

####________________________####
####____ Loading Content____####




# Build List of Regional Timeline Resources
region_groups <- c("A. Allyn: NELME Regions" = "nelme_regions",
                   "GMRI: SST Focal Areas"   = "gmri_sst_focal_areas", 
                   "NMFS Trawl Regions"      = "nmfs_trawl_regions",
                   "Large Marine Ecosystems" = "lme")



# Get the sub Groups - set names to values of group selections
region_list <- map(region_groups, function(region_group){
    region_choices <- gmRi::get_region_names(region_group)
    region_choice_names <- str_replace_all(region_choices, "_", " ")
    region_choice_names <- str_to_title(region_choice_names)
    return(region_choices)
}) %>% setNames(region_groups)






####________________________####

####____User Interface____####
ui <- material_page(
    nav_bar_fixed = TRUE,
    primary_theme_color = "#00695c", 
    secondary_theme_color = "#00796b",
    
    # Application title
    title = "Sea Surface Temperature Trends of Earth's Large Marine Ecosystems (and more!)",
    
    
    
    ####  User Selections  ####
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
        
        
        material_dropdown(input = "Region_Family", 
                          label = "Select a Collection of Regions", 
                          choices = region_groups, selected = "GMRI: SST Focal Areas"),
        
        
        #Reactive ui
        uiOutput("region_choice_reactive"),
        
        tags$br(),
        tags$p("The selected Area's polygon information is accessed directly 
               using the {gmRi} package")
        
    ),
    
    
    ####  Define tabs  ####
    material_tabs(
        tabs = c(
            "Selected Large Marine Ecosystem"   = "first_tab",
            "Marine Heatwave timelines"         = "second_tab",
            "Compare Climate Metrics"           = "third_tab"
        )
    ),
    
    
    ####__ Tab 1 Content - Map of LME  ####
    material_tab_content(
        tab_id = "first_tab",
        material_card(
            title = "Currently Selected Large Marine Ecosystem:",
            plotOutput("world_map") ) ),
    
    
    
    ####__ Tab 2 Content - SST Anomalies  ####
    material_tab_content(
        tab_id = "second_tab",
        material_card(
            title = "Tracking Deviations from the Climate-Mean",
            plotlyOutput("plotly_anomaly_timeline") ) )
    
    
)



####________________________####
####_____Server____####
server <- function(input, output, session) {
    
    
    # Could be faster to just load them as we go since there are now like a million:
    # Use the Shiny Inputs to Get the paths for the Regional Timeseries and shape
    
    
    ####  Reactive UI elements  ####
    
    
    #Region Choices Reactive - Generate Choices Here
    region_names_reactive <- reactive({
        
        region_choices      <- region_list[[input$Region_Family]]
        region_names_pretty <- str_replace_all(region_choices, "_", " ")
        region_names_pretty <- str_to_title(region_names_pretty)
        region_choices      <- setNames(region_choices, region_names_pretty)
        
        
    })
    
    
    # Generate the Reactive UI Here
    output$region_choice_reactive <- renderUI({
        render_material_from_server(
            material_dropdown(input_id = "Region_Choice",
                              choices = region_names_reactive(),
                              label = "Target Area:")
        )
        

    })
    
    
    
    # Reactive Data for Timeline and Plot
    #If everything goes as planned this should get us the timeseries and shapefile info
    
    # Shapefile Used as Mask
    mask_path <- reactive({
        
        # Paths to the timeseries and the shapes
        region_paths <- get_timeseries_paths(region_group = input$Region_Family)
        
        # Path to Shapefile
        shape_path <- region_paths[[input$Region_Choice]]["shape_path"]
        
        

        
    })
    
    
    
    
    
    
    
    ####  Region Map  ####
    output$world_map <- renderPlot({
        
        lme_polygon <- read_sf(mask_path())
        
        # title formatting
        area_title <- str_replace_all(input$Region_Choice, "_", " ")
        area_title <- str_to_title(area_title)
        
        # Build Map of Extent
        lme_bb <- sf_to_rect(lme_polygon)
        
        
        # Build Plot
        extent_map <- world_map + 
            geom_sf(data = lme_polygon, 
                    fill = gmri_cols("gmri blue"), 
                    color = gmri_cols("gmri_blue")) +
            geom_sf(data = lme_bb, 
                    color = gmri_cols("orange"), 
                    fill = "transparent", 
                    size = 1) +
            labs(title = area_title) +
            theme(plot.title = element_text(hjust = 0.5, size = 16, color = "gray10"))
        
      
        p <- extent_map()
        p
        
    })
    
    
    ####  Anomaly Timeline  ####
    output$plotly_anomaly_timeline <- renderPlotly({
        
        # Plotly Timelines
        plotly_mhw_plots[[input$lme_choice]]
    })
    
    
    
    
    
    
    
    
    
}




#---- Run the application ----
shinyApp(ui = ui, server = server)

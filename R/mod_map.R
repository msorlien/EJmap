################################### HEADER ###################################
#  TITLE: ej_map.R
#  DESCRIPTION: Module to display EJ map
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-23
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(shinyjs)
library(sf)
library(leaflet)
library(leaflet.extras2)
library(shinycssloaders)

########################################################################.
###                       User Interface                            ####
########################################################################.

map_ui <- function(id, input_shp, percentiles = c('N_', 'P_')) {
  
  ns <- NS(id)

  # UI ----
  tagList(
    # Metric/Category ----
    pickerInput(
      inputId = ns('select_layer'),
      label = 'Select Layer',
      choices = list_col_codes(input_shp),
      selected = 'SCORE',
      options = list(
        `live-search` = TRUE),
      multiple = FALSE
    ),
    # Percentile type ----
    awesomeRadio(
      inputId = ns('percentile_type'),
      label = NULL, 
      choices = list_percentile_codes(percentiles),
      selected = list_percentile_codes(percentiles)[1],
      inline = TRUE, 
      checkbox = TRUE
    ),
    # Map ----
    shinycssloaders::withSpinner(
      leafletOutput(ns('map'), width='100%', height = '75vh'),
      type = 5
    )
  )
  
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

map_server <- function(id, ejvar, min_overall_score = 0) {
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    input_shp <- reactive({
      ejvar$output_shp()
    })
    
    # Update list of parameters ----
    observeEvent(ejvar$btn_metrics(), {

      col_codes <- list_col_codes(input_shp())

      # Update picker input (choices, selected)
      updatePickerInput(session = session,
                        inputId = 'select_layer',
                        choices = col_codes,
                        selected = 'SCORE')
    })
    
    # Selected column ----
    display_layer <- reactive({
      req(input$select_layer)
      req(input$percentile_type)

      layer_name <- paste0(input$percentile_type, input$select_layer)

      display_layer <- input_shp()[[layer_name]]

      return(display_layer)

    })
    
    # Color ramp ----
    pal <- reactive({ 
      req(input$select_layer)
      
      pal_colors(input$select_layer, ejvar$percentile_min(), min_overall_score) 
      
      })

    # Leaflet basemap ----
    output$map <- renderLeaflet({
      leaflet() %>%
      # * Set map dimensions ----
      fitBounds(-71.9937973, # Lon min
                41.29999924, # Lat min
                -70.5164032, # Lon max
                42.43180084 # Lat max
                ) %>%
      # * Add basemap tiles ----
      addProviderTiles(providers$CartoDB.Positron) %>%
      # * Add spinner ----
      leaflet.extras2::addSpinner() %>%
      # * Add scale bar ----
      addScaleBar(position='bottomleft')
    })

    # Update polygons ----
    observe({
      # * Clear polygons, legend ----
      leafletProxy('map') %>%
        clearShapes() %>%
        removeControl('legend')
      
      # * Add legend ----
      leafletProxy('map') %>%
        addLegend(
          layerId = 'legend',
          pal = pal(), 
          values = c(0,100),
          position = 'bottomright')
      
      # * Add EJ map ----
      if (nrow(input_shp()) > 0) {
        leafletProxy('map') %>%
          # Add spinner
          leaflet.extras2::startSpinner(
            list('length' = 0, 'lines' = 8, 'width' = 20, 'radius' = 40,
                 'color' = '#0275D8')
          ) %>%
          # Add polygons
          addPolygons(
            data = input_shp(),
            layerId = input_shp(),
            # Label
            label = ~paste(Town, State,
                           display_layer()),
            labelOptions = labelOptions(textsize = '15px'),
            # Popup
            popup = ~paste0('<b>Block Group: </b>', BlockGroup,
                            '<br/><b>Town: </b>', Town, ', ', State,
                            '<br/><b>Watersheds: </b>', HUC10_Name,
                            '<br/><br/><b>', input$select_layer, ': </b>',
                            display_layer()),
            # Stroke
            color = '#000000',
            weight = 0.5,
            smoothFactor = 0.5,
            opacity = 0.2,
            # Fill
            fillOpacity = 0.8,
            fillColor = ~pal()(display_layer()),
            # Highlight
            highlightOptions = highlightOptions(fillColor = '#ffffff',
                                                weight = 2,
                                                bringToFront = TRUE)
          ) %>%
          # Stop spinner
          leaflet.extras2::stopSpinner()
      }
        
    })
    
  })
}

# end Server Function

#  TITLE: ej_map.R
#  DESCRIPTION: Module to display EJ map
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-07-26
#  GIT REPO: NBEP/EJmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
# -----------------------------------------------------------------------------.

library(shinyjs)
library(sf)
library(leaflet)
library(leaflet.extras2)
library(shinycssloaders)

# UI --------------------------------------------------------------------------

map_ui <- function(id, input_shp, percentiles = c('N_', 'P_'), 
                   default_layer = 'SCORE') {
  
  ns <- NS(id)

  # UI ----
  tagList(
    # Metric/Category ----
    shinyWidgets::pickerInput(
      inputId = ns('select_layer'),
      label = 'Select Layer',
      choices = list_column_codes(input_shp),
      selected = default_layer,
      options = list(
        `live-search` = TRUE,
        container = 'body'),  # Allows dropdown overflow
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
    textOutput(ns('test')),
    
    # Map ----
    shinycssloaders::withSpinner(
      leafletOutput(ns('map'), width='100%', height = '70vh'),
      type = 5
    )
  )
}

# Server ----------------------------------------------------------------------.

map_server <- function(id, ejvar, default_layer = 'SCORE') {
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    # Define variables ----
    input_shp <- reactive({ ejvar$output_shp() })
    
    # Update list of parameters ----
    observeEvent(ejvar$btn_metrics(), {

      # Define variables
      col_codes <- list_column_codes(input_shp())

      # Update layer list
      updatePickerInput(session = session,
                        inputId = 'select_layer',
                        choices = col_codes,
                        selected = default_layer)
    })
    
    # Selected layer ----
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
      
      pal_colors(input$select_layer, ejvar$percentile_min()) 
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
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.Positron, 
          group = 'Light Map') %>%
          leaflet::addProviderTiles(
            leaflet::providers$CartoDB.DarkMatter, 
            group = 'Dark Map') %>%
          leaflet::addProviderTiles(
            leaflet::providers$Esri.WorldImagery, 
            group = 'Satellite') %>%
        # Add legend? maybe???
        { if (default_layer == 'EJAREA') 
          addLegend(
            map = .,
            layerId = 'legend',
            title = 'EJ Communities',
            colors = c('#F03B20', '#FFEDA0', '#B1B1B1'),
            labels = c('EJ Community', 'Not an EJ Communitiy', 'No Data'),
            position = 'bottomright') 
          else
            addLegend(
              map = .,
              layerId = 'legend',
              title = 'Score',
              pal = pal_colors(default_layer),
              values = c(0,100),
              position = 'bottomright')
          } %>% 
        # * Add layer toggle ----
        leaflet::addLayersControl(
          baseGroups = c('Light Map', 'Dark Map', 'Satellite'),
          position='topleft'
        ) %>%
        # * Add spinner ----
        leaflet.extras2::addSpinner() %>%
        # * Add scale bar ----
        addScaleBar(position='bottomleft')
    })

    # Update legends ----
    # * Legend type ----
    legend_type <- reactive({
      if (input$select_layer == 'EJAREA') {
        legend_type <- 'EJ'
      } else if (input$select_layer %in% c('SOCVUL', 'HEALTH', 'ENVBUR',
                                           'CLIMATE', 'SCORE')) {
        legend_type <- 'SCORE'
      } else {
        legend_type <- 'PERCENTILE'
      }
      
      return(legend_type)
    })
    
    # * Remove/add legend ----
    observeEvent( c(legend_type(), ejvar$btn_metrics()), {

      leafletProxy('map') %>%
        removeControl('legend')

      if (legend_type() == 'EJ') {
        leafletProxy('map') %>%
          addLegend(
            layerId = 'legend',
            title = 'EJ Communities',
            colors = c('#F03B20', '#FFEDA0', '#B1B1B1'),
            labels = c('EJ Community', 'Not an EJ Communitiy', 'No Data'),
            position = 'bottomright')
      } else {
        leafletProxy('map') %>%
          addLegend(
            layerId = 'legend',
            title = stringr::str_to_title(legend_type()),
            pal = pal(),
            values = c(0,100),
            position = 'bottomright')
      }
    })

    # Update polygons ----
    observe({
      # * Clear polygons ----
      leafletProxy('map') %>%
        clearShapes()
      
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

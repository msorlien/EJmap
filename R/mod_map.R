################################### HEADER ###################################
#  TITLE: ej_map.R
#  DESCRIPTION: Module to display EJ map
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-18
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(shinyjs)
library(sf)
library(leaflet)
library(shinycssloaders)

########################################################################.
###                       User Interface                            ####
########################################################################.

map_ui <- function(id, input_shp, percentiles = c('N_', 'P_')) {
  
  # Set var ----
  percentile_codes <- c('N_', 'P_')
  percentile_names <- c('Compare to Narragansett Bay Watershed',
                        'Compare to State')
  df_percentiles <- data.frame(percentile_codes, percentile_names) %>%
    filter(percentile_codes %in% percentiles)
  
  percentile_list <- df_percentiles$percentile_codes
  names(percentile_list) <- df_percentiles$percentile_names
  
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
      choices = percentile_list,
      selected = percentile_list[1],
      inline = TRUE, 
      checkbox = TRUE
    ),
    # Map ----
    shinycssloaders::withSpinner(
      leafletOutput(ns('map'), width='100%', height = '75vh')
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
      # * Add legend ----
      addLegend(pal = pal(), values = c(0,100),
                position = 'bottomright') %>%
      # * Add scale bar ----
      addScaleBar(position='bottomleft') 
    })

    # Add polygons ----
    observe({
      req(input$select_layer)
      req(input$percentile_type)

      leafletProxy('map') %>%
        clearShapes() %>%
        # * EJ map ----
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
      )
    })
    
  })
}

# end Server Function

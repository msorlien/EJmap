################################### HEADER ###################################
#  TITLE: ej_map.R
#  DESCRIPTION: Module to display EJ map
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-09
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(shinyjs)
library(sf)
library(leaflet)

########################################################################.
###                       User Interface                            ####
########################################################################.

map_ui <- function(id) {
  
  ns <- NS(id)

  tagList(
    # Metric/Category ----
    pickerInput(
      inputId = ns('select_layer'),
      label = 'Select Layer',
      choices = NULL,
      selected = NULL,
      options = list(
        `live-search` = TRUE),
      multiple = FALSE
    ),
    # Percentile type ----
    awesomeRadio(
      inputId = ns('percentile_type'),
      label = NULL, 
      choices = c('Compare to NBEP'='N_', 'Compare to State'='P_'),
      selected = 'N_',
      inline = TRUE, 
      checkbox = TRUE
    ),
    # Map ----
    leafletOutput(ns('map'), width='100%', height = '75vh')
  )
  
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

map_server <- function(id, ejvar, edit_metrics=TRUE) {
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    input_shp <- reactive({
      ejvar$output_shp()
    })
    
    # Update list of parameters ----
    observeEvent(ejvar$btn_metrics(), {

      # List col names in input_shp, minus first two letters
      # (drops N_, P_)
      input_col <- str_sub(colnames(input_shp()), 3, -1)

      # Filter column table for col in input_shp
      df_columns <- column_table %>%
        filter(COL_CODE %in% input_col)

      # Make named list
      col_codes <- df_columns$COL_CODE
      names(col_codes) <- df_columns$COL_NAME

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
    pal <- colorNumeric (
        palette=colorRamp(
          # Iridescent color palette by Paul Tol
          # https://personal.sron.nl/~pault/data/colourschemes.pdf
          c('#FEFBE9', '#FCF7D5', '#F5F3C1', '#EAF0B5', '#DDECBF', '#D0E7CA',
            '#C2E3D2', '#B4DDD8', '#A8D8DC', '#9BD2E1', '#8DCBE4', '#81C4E7',
            '#7BBCE7', '#7EB2E4', '#88A5DD', '#9398D2', '#9B8AC4', '#9D7DB2',
            '#9A709E', '#906388', '#805770', '#684957', '#46353A'), 
          interpolate = 'spline'),
        na.color = '#999999',
        domain = c(0,100)
        )

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
      addLegend(pal = pal, values = c(0,100),
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
          fillColor = ~pal(display_layer()),
          # Highlight
          highlightOptions = highlightOptions(fillColor = '#ffffff',
                                              weight = 2,
                                              bringToFront = TRUE)
      )
    })
    
  })
}

# end Server Function

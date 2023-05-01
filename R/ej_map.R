################################### HEADER ###################################
#  TITLE: ej_map.R
#  DESCRIPTION: Module to display EJ map
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-01
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
    # List metrics ----
    tagList(
      uiOutput(ns("ui_select_layer")),
      leafletOutput(ns("map"), width='100%', height = "70vh")
    )
  )
  
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

map_server <- function(id, shp_input, edit_metrics=TRUE) {
  moduleServer(id, function(input, output, session) {
    
    # Select parameter/score ----
    # * List columns, column names ---
    col_codes <- reactive({
      df_columns <- column_table %>%
        filter(COL_CODE %in% colnames(shp_input()))
      col_codes <- df_columns$COL_CODE
      names(col_codes) <- df_columns$COL_NAME
      
      return(col_codes)
    })
    
    # * UI
    output$ui_select_layer <- renderUI({
      pickerInput(
        inputId = "select_layer",
        label = "Select Layer",
        choices = col_codes(),
        selected = "S_SCORE",
        options = list(
          `live-search` = TRUE),
        multiple = FALSE
        )
    })
    
    # Leaflet basemap ----
    output$map <- renderLeaflet({
      leaflet() %>%
      # * Set map dimensions ----
      fitBounds(-71.993781, # Lon min
                41.300040, # Lat min
                -70.516363, # Lon max
                42.431835 # Lat max
                ) %>%
      # * Add basemap tiles ----
      addProviderTiles(providers$CartoDB.Positron) %>%
        # * Add scale bar ----
      addScaleBar(position='bottomleft')
    })
    
    # Leaflet polygons ----
    pal <- reactive({
      colorBin("YlOrRd", 
               domain = shp_input()$S_SCORE)
    })
    
    observe({
      leafletProxy("map") %>%
        clearShapes() %>%
        # * EJ map ----
        addPolygons(
          data = shp_input(),
          layerId = shp_input(),
          # Label
          label = "Label",
          labelOptions = labelOptions(textsize = "15px"),
          # Popup
          popup = "Popup",
          # Stroke
          color = '#08306b',
          weight = 0.5,
          smoothFactor = 0.5,
          opacity = 0.2,
          # Fill
          fillOpacity = 0.8,
          fillColor = ~pal()(S_SCORE),
          # Highlight
          highlightOptions = highlightOptions(fillColor = '#f7fbff',
                                              weight = 2,
                                              bringToFront = TRUE)
      )
      
    })
    
    # # Output reactive values ----
    # return(
    #   list(
    #   )
    # )
    
  })
}

# end Server Function

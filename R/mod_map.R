#  TITLE: mod_map.R
#  DESCRIPTION: Module to display EJ map
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2024-02-05
#  GIT REPO: NBEP/EJmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
# -----------------------------------------------------------------------------.

# UI --------------------------------------------------------------------------

map_ui <- function(id, input_shp, default_layer = 'SCORE') {
  
  ns <- NS(id)

  # UI ----
  tagList(
    # Metric/Category ----
    shinyWidgets::pickerInput(
      inputId = ns('select_layer'),
      label = 'Select Layer',
      choices = list_indicators_grouped(input_shp),
      selected = default_layer,
      options = list(
        `live-search` = TRUE,
        container = 'body'),  # Allows dropdown overflow
      multiple = FALSE
    ),
    
    # Map ----
    shinycssloaders::withSpinner(
      leaflet::leafletOutput(ns('map')),
      type = 5
    )
  )
}

# Server ----------------------------------------------------------------------.

map_server <- function(id, ejvar, active_tab, map_tab, 
                       default_layer = 'SCORE') {
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    # Define variables ----
    input_shp <- reactive({ 
      input_shp <- ejvar$output_shp()
      colnames(input_shp) <- gsub("N_", "_", colnames(input_shp))
      colnames(input_shp) <- gsub("P_", "_", colnames(input_shp))
      
      input_shp <- add_popup_text(input_shp, "_")
      
      return(input_shp)
    })
    
    # Update list of parameters ----
    observeEvent(ejvar$btn_metrics(), {

      # Define variables
      col_codes <- list_indicators_grouped(input_shp())

      # Update layer list
      shinyWidgets::updatePickerInput(session = session,
                        inputId = 'select_layer',
                        choices = col_codes,
                        selected = default_layer)
    })
    
    # Selected layer ----
    display_layer <- reactive({
      req(input$select_layer)

      layer_name <- paste0("_", input$select_layer)

      display_layer <- input_shp()[[layer_name]]

      return(display_layer)
    })
    
    popup_text <- reactive({
      req(input$select_layer)
      
      cat_code <- dplyr::filter(column_table, COL_CODE == input$select_layer)
      popup_column <- paste0("T_", cat_code$CAT_CODE)
      
      popup_text <- input_shp()[[popup_column]]
      
      return(popup_text)
    })
    
    # Color ramp ----
    pal <- reactive({ 
      req(input$select_layer)
      
      pal_colors(input$select_layer, ejvar$percentile_min()) 
      })

    # Leaflet basemap ----
    output$map <- renderLeaflet({
      leaflet::leaflet() %>%
        # * Set map dimensions ----
      leaflet::fitBounds(
        -71.9937973, # Lon min
        41.29999924, # Lat min
        -70.5164032, # Lon max
        42.43180084 # Lat max
        ) %>%
      # * Add basemap tiles ----
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldTopoMap) %>%
      # * Add spinner ----
      leaflet.extras2::addSpinner() %>%
      # * Add scale bar ----
      leaflet::addScaleBar(position='bottomleft')
    })

    # Update legend ----
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
    observe({
      req(active_tab() == map_tab)

      leaflet::leafletProxy('map') %>%
        leaflet::removeControl('legend')

      if (legend_type() == 'EJ') {
        leaflet::leafletProxy('map') %>%
          leaflet::addLegend(
            layerId = 'legend',
            title = 'EJ Areas',
            colors = c('#F03B20', '#FFEDA0', '#B1B1B1'),
            labels = c('EJ Area', 'Not an EJ Area', 'No Data'),
            position = 'bottomright')
      } else {
        leaflet::leafletProxy('map') %>%
          leaflet::addLegend(
            layerId = 'legend',
            title = stringr::str_to_title(legend_type()),
            pal = pal(),
            values = c(0,100),
            position = 'bottomright')
      }
    })

    # Update polygons ----
    observe({
      req(input_shp())
      req(active_tab() == map_tab)
      
      # * Clear polygons ----
      leaflet::leafletProxy('map') %>%
        leaflet::clearShapes()

      # * Add EJ map ----
      leafletProxy('map') %>%
        # Start spinner
        leaflet.extras2::startSpinner(
          list('length' = 0, 'lines' = 8, 'width' = 20, 'radius' = 40,
               'color' = '#0275D8')
        ) %>%
        # Add polygons
        leaflet::addPolygons(
          data = input_shp(),
          layerId = input_shp(),
          # Label
          label = ~paste(Town, State,
                         display_layer()),
          labelOptions = labelOptions(textsize = '15px'),
          # Popup
          popup = ~popup_text(),
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
    })
    
  })
}

# end Server Function

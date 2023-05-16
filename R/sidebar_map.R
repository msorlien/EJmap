################################### HEADER ###################################
#  TITLE: sidebar_map.R
#  DESCRIPTION: Module to select location & parameters
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-15
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(shinyjs)

# UI --------------------------------------------------------------------------

map_sidebar_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    # Enable javascript ----
    useShinyjs(),
    
    # Select location ----
    h2('Select Location'),
    # select_location_ui(ns('select_location')),
    
    # Select data ----
    selectPar_ui(ns('select_metrics')),
    
    # Download ----
    h2('Download Data')
  )
  
}

# Server ----------------------------------------------------------------------

map_sidebar_server <- function(id, input_shp, input_shp_simple, 
                               select_metrics = TRUE) {
  moduleServer(id, function(input, output, session) {
    
    # Select parameters ----
    # * Add module ----
    df_par <- selectPar_server('select_metrics', input_shp_simple)

    # * Filter data ----
    shp_metrics <- reactive({ df_par$output_shp() })
    
    # Filter location ----
    # * Add module
    # df_loc <- select_location_server('select_location', df_par$output_shp())

    # Output reactive values ----
    return(
      list(

        output_shp = reactive({ df_par$output_shp() }),
        btn_metrics = reactive({ df_par$button() }),
        # location_type = reactive({ df_loc$location_type() }),
        percentile_min = reactive({ df_par$percentile_min() })

      )
    )
    
  })
}

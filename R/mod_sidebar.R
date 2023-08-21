################################### HEADER ###################################
#  TITLE: mod_sidebar.R
#  DESCRIPTION: Module to select location & parameters
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-08-21
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
    
    # Accordion ----
    bslib::accordion(
      multiple = FALSE,
      # Select location ----
      bslib::accordion_panel(
        title = h2('Select Location'),
        value = 'location',
        select_location_ui(ns('location'))
      ),
      # Select metrics ----
      conditionalPanel(
        condition = paste0('output["', ns('show_metrics'), '"] == "TRUE"'),
        bslib::accordion_panel(
          title = h2('Select Indicators'),
          value = 'metrics',
          selectPar_ui(ns('metrics'))
          )
      ),
      # Download ----
      bslib::accordion_panel(
        title = h2('Download Data'),
        value = 'download',
        download_ui(ns('download'))
      )
    )
  )
  
}

# Server ----------------------------------------------------------------------

map_sidebar_server <- function(
    id, input_shp, input_shp_simple, select_metrics = TRUE, 
    percentile_type = c('N_', 'P_')
    ) {
  moduleServer(id, function(input, output, session) { 
    
    # Pass info to ui ----
    output$show_metrics <- renderText({ paste0(select_metrics) })
    
    outputOptions(output, 'show_metrics', suspendWhenHidden = FALSE)
    
    # Default shp ----
    shp_reactive <- reactiveValues(shp = input_shp_simple)
    
    # Select parameters ----
    # * Add module ----
    df_par <- selectPar_server('metrics', input_shp_simple)
    
    # * Update shp ----
    observeEvent(df_par$button(), {
      shp_reactive$shp <- df_par$output_shp()
    })
    
    # Filter location ----
    # * Add module ----
    df_loc <- select_location_server('location', shp_reactive)
    
    # Download ----
    # * Add module ----
    download_server('download', input_shp)

    # Output reactive values ----
    return(
      list(

        output_shp = reactive({ df_loc$output_shp() }),
        btn_metrics = reactive({ df_par$button() }),
        location_type = reactive({ df_loc$location_type() }),
        percentile_min = reactive({ df_par$percentile_min() })

      )
    )
    
  })
}

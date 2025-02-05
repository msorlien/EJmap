################################### HEADER ###################################
#  TITLE: mod_sidebar.R
#  DESCRIPTION: Module to select location & parameters
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2025-02-05
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

# UI --------------------------------------------------------------------------

map_sidebar_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    # Enable javascript ----
    shinyjs::useShinyjs(),
    
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
    percentile_type = c("N_", "P_")) {
  moduleServer(id, function(input, output, session) { 
    
    # Pass info to ui ----
    output$show_metrics <- renderText({ paste0(select_metrics) })
    outputOptions(output, 'show_metrics', suspendWhenHidden = FALSE)
    
    # Default shp ----
    # Drop columns for "wrong" percentile type
    drop_list <- c("N_", "P_")
    drop_list <- drop_list[drop_list != percentile_type[1]]
    
    default_shp <- input_shp_simple %>% 
      select(-starts_with(drop_list)) 
    
    # If using regional percentiles, drop extra rows
    if(percentile_type[1] == "N_") {
      default_shp <- filter(default_shp, !Study_Area == "Outside Study Area")
    }
 
    shp_reactive <- reactiveValues(shp = default_shp)
    
    # Select parameters ----
    # * Add module ----
    df_par <- selectPar_server('metrics', 
      input_shp = input_shp_simple, 
      percentile_type = percentile_type)
    
    # * Update shp ----
    observeEvent(df_par$button(), {
      shp_reactive$shp <- df_par$output_shp()
    })
    
    # Filter location ----
    # * Add module ----
    df_loc <- select_location_server('location', shp_reactive)
    
    # Download ----
    # * Add module ----
    download_server('download', 
                    input_shp = input_shp, 
                    select_metrics = select_metrics, 
                    metrics = df_par, 
                    location = df_loc)

    # Output reactive values ----
    return(
      list(

        output_shp = reactive({ df_loc$output_shp() }),
        btn_metrics = reactive({ df_par$button() }),
        percentile_min = reactive({ df_par$percentile_min() })

      )
    )
    
  })
}

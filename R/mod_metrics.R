################################### HEADER ###################################
#  TITLE: select_metrics.R
#  DESCRIPTION: Module to select parameters
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-07-26
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(shinyWidgets)

# UI --------------------------------------------------------------------------

selectPar_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    # Enable javascript ----
    useShinyjs(),
    
    # Secret tabs ----
    bslib::navset_hidden(
      id = ns('tabset_metrics'),
      # Tab 1 ----
      bslib::nav_panel_hidden(
        'tab_metrics',
        
        # * Select metrics ----
        h3('Metrics'),
        selectParInput_ui(ns('socvul_metrics'), 'SOCVUL'),
        selectParInput_ui(ns('health_metrics'), 'HEALTH'),
        selectParInput_ui(ns('envbur_metrics'), 'ENVBUR'),
        selectParInput_ui(ns('climate_metrics'), 'CLIMATE'),
        
        # Minimum score ----
        h3('Minimum Score'),
        numericInput(
          ns('min_ej'),
          'Set minimum EJ score for environmental justice areas.',
          value = 0,
          min = 0,
          max = 100
        ),
        
        # * Advanced Options Button ----
        actionButton(ns('btn_advanced'),
                     label = 'Show Advanced Options'),
        
        # * Calculate Button ----
        actionButton(ns('btn_calculate'),
                     label = 'Calculate Score')
      ),
      # Tab 2 ----
      bslib::nav_panel_hidden(
        'tab_advanced',
        # * Advanced Options ----
        advancedSelect_ui(ns('advanced_options'))
      )
    )
  )
}

# Server -----------------------------------------------------------------------

selectPar_server <- function(id, input_shp) {
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)  # Set namespace
    
    # Add metric servers ----
    socvul <- selectParInput_server('socvul_metrics')
    health <- selectParInput_server('health_metrics')
    envbur <- selectParInput_server('envbur_metrics')
    climate <- selectParInput_server('climate_metrics')
    
    # Compile list of selected metrics ----
    metrics_list <- reactive({ c(socvul(), health(), envbur(), climate()) })
    
    # Pass btn_advanced as reactive value ----
    btn_reset <- reactive({ input$btn_advanced })
    
    # Add server for advanced options ----
    adv_opt <- advancedSelect_server('advanced_options', metrics_list, 
                                     btn_reset)
    
    # Button: View Advanced Options
    observeEvent(input$btn_advanced, {
      bslib::nav_select('tabset_metrics', 'tab_advanced')
    })
    
    # Button: Hide advanced options
    observeEvent(c(adv_opt$btn_save(), adv_opt$btn_cancel()), {
      bslib::nav_select('tabset_metrics', 'tab_metrics')
    })
    
    # Toggle buttons on/off ----
    shinyjs::disable('btn_calculate')
    shinyjs::disable('btn_advanced')

    observe({
      if (length(metrics_list()) > 0) {
        shinyjs::enable('btn_calculate')
        shinyjs::enable('btn_advanced')
      } else {
        shinyjs::disable('btn_calculate')
        shinyjs::disable('btn_advanced')
      }
    })
    
    # Calculate score ----
    shp_output <- eventReactive(input$btn_calculate, {
      
      # Drop pre-existing score columns
      shp_output <- input_shp %>%
        select(-contains(c('_SOCVUL', '_HEALTH', '_ENVBUR', '_CLIMATE', 
                           '_SCORE', '_ISEJ')))
      
      # Calculate score
      shp_output <- calculate_score(
        input_shp = shp_output, 
        percentile_min = adv_opt$percentile_min(), 
        min_pass = adv_opt$min_pass(),
        min_ej = as.numeric(input$min_ej),
        df_metrics = adv_opt$df_metric(), 
        df_categories = adv_opt$df_cat()
        ) 
      
      # Set -999999 to NA
      shp_output <- shp_output %>%
        mutate(across(where(is.numeric), ~na_if(., -999999)))
      
      return(shp_output)
    })
    
    
    # Output reactive values ----
    return(
      list(
        
        output_shp = reactive({ shp_output() }),
        percentile_min = reactive({ adv_opt$percentile_min() }),
        button = reactive({ input$btn_calculate })
        
      )
    )
    
  })
}

################################### HEADER ###################################
#  TITLE: select_metrics.R
#  DESCRIPTION: Module to select parameters
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-15
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
    
    # Select metrics ----
    h2('Select Metrics'),
    selectParInput_ui(ns('socvul_metrics'), 'SOCVUL'),
    selectParInput_ui(ns('health_metrics'), 'HEALTH'),
    selectParInput_ui(ns('envbur_metrics'), 'ENVBUR'),
    selectParInput_ui(ns('climate_metrics'), 'CLIMATE'),
    
    # Advanced options ----
    advancedSelect_ui(ns('advanced_options')),
    
    # Button (calculate score) ----
    tags$br(),
    tags$br(),
    actionButton(ns('btn_calculate'),
                 label = 'Calculate Score')
  )
  
}

# Server -----------------------------------------------------------------------

selectPar_server <- function(id, input_shp) {
  moduleServer(id, function(input, output, session) {
    
    # Add metric servers ----
    socvul <- selectParInput_server('socvul_metrics')
    health <- selectParInput_server('health_metrics')
    envbur <- selectParInput_server('envbur_metrics')
    climate <- selectParInput_server('climate_metrics')
    
    # Filter metric table for selected variables ----
    df_metrics <- reactive({
      
      selected_metrics <- c(socvul(), health(), envbur(), climate())
      
      df_metrics <- metric_table %>%
        filter(METRIC_CODE %in% selected_metrics)
      
      return(df_metrics)
    })
    
    # Add server for advanced options ----
    adv_opt <- advancedSelect_server('advanced_options', df_metrics)
    
    # Button: Calculate score ----
    # * Toggle button on/off ----
    shinyjs::disable('btn_calculate')

    observe({
      if (nrow(df_metrics()) > 0) {
        shinyjs::enable('btn_calculate')
      } else {
        shinyjs::disable('btn_calculate')
      }
    })
    
    # Calculate score ----
    shp_output <- eventReactive(input$btn_calculate, {
      
      shp_output <- calculate_score(
        input_shp = input_shp, 
        percentile_min = adv_opt$percentile_min(), 
        exceed_all_min_scores = adv_opt$exceed_all(), 
        df_metrics = adv_opt$df_metric(), 
        df_categories = adv_opt$df_cat()
      ) 
      
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

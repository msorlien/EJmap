################################### HEADER ###################################
#  TITLE: select_metrics.R
#  DESCRIPTION: Module to select parameters
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-08
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(shinyWidgets)

########################################################################.
###                       User Interface                            ####
########################################################################.

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
    
    # Button ----
    'BUTTON PLACEHOLDER',
    
    # Advanced options ----
    advancedSelect_ui(ns('advanced_options')),
    
    # Button ----
    tags$br(),
    actionButton(ns('btn_calculate'),
                 label = 'Calculate Score')
  )
  
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

selectPar_server <- function(id) {
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
      if (nrow(df_metrics()>0)) {
        shinyjs::enable('btn_calculate')
      } else {
        shinyjs::disable('btn_calculate')
      }
    })
    
    # Output reactive values ----
    return(
      list(
        
        percentile_min = reactive({ adv_opt$percentile_min() }),
        exceed_all = reactive({ adv_opt$exceed_all() }),
        df_cat = reactive({ adv_opt$df_cat() }),
        df_metric = reactive({ adv_opt$df_metric() }),
        button = reactive({ input$btn_calculate })
        
      )
    )
    
  })
}

# end Server Function

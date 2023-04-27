################################### HEADER ###################################
#  TITLE: mod_select.R
#  DESCRIPTION: Module to select parameters
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-04-27
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
    # Metrics ----
    selectParInput_ui(ns('socvul_metrics'), 'SOCVUL'),
    selectParInput_ui(ns('health_metrics'), 'HEALTH'),
    selectParInput_ui(ns('envbur_metrics'), 'ENVBUR'),
    selectParInput_ui(ns('climate_metrics'), 'CLIMATE')
  )
  
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

selectPar_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Add module servers ----
    socvul <- selectParInput_server('socvul_metrics')
    health <- selectParInput_server('health_metrics')
    envbur <- selectParInput_server('envbur_metrics')
    climate <- selectParInput_server('climate_metrics')
    
    # List selected variables ----
    selected_parameters <- reactive({
      c(socvul(), health(), envbur(), climate())
    })
    
    # Filter metric table for selected variables ----
    df_par <- reactive({
      metric_table %>%
        filter(METRIC_CODE %in% selected_parameters())
    })
    
    # Output dataframe ----
    return( 
      reactive({ df_par() }) 
      )
    
  })
}

# end Server Function

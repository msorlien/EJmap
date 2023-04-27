################################### HEADER ###################################
#  TITLE: mod_select.R
#  DESCRIPTION: Module to select location & parameters
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-04-27
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(shinyWidgets)
# library(bslib)

########################################################################.
###                       User Interface                            ####
########################################################################.

SELECT_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    # Select data ----
    tabsetPanel(type = "tabs",
                # Location ----
                tabPanel("Select Location", 
                         h2('Select Town')
                         ),
                # Metrics ----
                tabPanel("Select Metrics",
                         SELECT_METRIC_UI(ns('socvul_metrics'), 
                                          'Social Vulnerability', 'SOCVUL'),
                         SELECT_METRIC_UI(ns('health_metrics'), 
                                          'Health', 'HEALTH'),
                         SELECT_METRIC_UI(ns('envbur_metrics'), 
                                          'Environmental Burden', 'ENVBUR'),
                         SELECT_METRIC_UI(ns('climate_metrics'), 
                                          'Climate', 'CLIMATE')
                         ),
                # Advanced Options ----
                tabPanel("Advanced Options",
                         h2('Percentiles'),
                         'Minimum value, state vs NBEP',
                         
                         h2('Categories'),
                         weightVar_ui(ns("weight_cat"), "category", "all"),
                         
                         h2('Metrics'),
                         weightVar_ui(ns("socvul_metric_weight"), "metrics", 
                           "Social Vulnerability"),
                         METRIC_WEIGHT_UI(ns('health_metric_weight'), 'Health'),
                         METRIC_WEIGHT_UI(ns('envbur_metric_weight'),
                                          'Environmental Burden'),
                         METRIC_WEIGHT_UI(ns('climate_metric_weight'),
                                          'Climate')
                         )
    ),
    '(calculate) (cancel? reset?)'
  )
  
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

SELECT_SERVER <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Add module servers ----
    select_socvul <- SELECT_METRIC_SERVER('socvul_metrics')
    select_health <- SELECT_METRIC_SERVER('health_metrics')
    select_envbur <- SELECT_METRIC_SERVER('envbur_metrics')
    select_climate <- SELECT_METRIC_SERVER('climate_metrics')
    
    select_all <- reactive({
      c(select_socvul(), select_health(), select_envbur(), select_climate())
    })
    
    weight_cat <- weightVar_server("weight_cat", "category", NULL, select_all)
    
    weight_socvul <- weightVar_server(
      'socvul_metric_weight', "metrics", "SOCVUL", select_socvul)
    weight_health <- METRIC_WEIGHT_SERVER('health_metric_weight', "HEALTH", 
                                          select_health)
    weight_envbur <- METRIC_WEIGHT_SERVER('envbur_metric_weight', "ENVBUR", 
                                          select_envbur)
    weight_climate <- METRIC_WEIGHT_SERVER('climate_metric_weight', "CLIMATE", 
                                           select_climate)
    weight_all <- reactive({
      rbind(weight_socvul(), weight_health(), weight_envbur(), weight_climate())
    }) 

    
    # # Output reactive values ----
    # return(
    #   list(
    #   )
    # )
    
  })
}

# end Server Function

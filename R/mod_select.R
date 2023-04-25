################################### HEADER ###################################
#  TITLE: mod_select.R
#  DESCRIPTION: Module to select location & parameters
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-04-25
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
                         CAT_WEIGHT_UI('socvul_weight', 'Social Vulnerability'),
                         CAT_WEIGHT_UI('health_weight', 'Health'),
                         CAT_WEIGHT_UI('envbur_weight', 'Environmental Burden'),
                         CAT_WEIGHT_UI('climate_weight', 'Climate'),
                         h2('Metrics'),
                         METRIC_WEIGHT_UI(ns('socvul_metric_weight'),
                                          'Social Vulnerability'),
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
    cat_socvul <- SELECT_METRIC_SERVER('socvul_metrics')
    cat_health <- SELECT_METRIC_SERVER('health_metrics')
    cat_envbur <- SELECT_METRIC_SERVER('envbur_metrics')
    cat_climate <- SELECT_METRIC_SERVER('climate_metrics')
    
    CAT_WEIGHT_SERVER('socvul_weight', 'SOCVUL')
    CAT_WEIGHT_SERVER('health_weight', "HEALTH")
    CAT_WEIGHT_SERVER('envbur_weight', "ENVBUR")
    CAT_WEIGHT_SERVER('climate_weight', "CLIMATE")
    
    METRIC_WEIGHT_SERVER('socvul_metric_weight', "SOCVUL", cat_socvul)
    METRIC_WEIGHT_SERVER('health_metric_weight', "HEALTH", cat_health)
    METRIC_WEIGHT_SERVER('envbur_metric_weight', "ENVBUR", cat_envbur)
    METRIC_WEIGHT_SERVER('climate_metric_weight', "CLIMATE", cat_climate)
    
    # Output reactive values ----
    return(
      list(
      )
    )
    
  })
}

# end Server Function

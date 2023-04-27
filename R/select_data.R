################################### HEADER ###################################
#  TITLE: select_data.R
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
                # # Location ----
                # tabPanel("Select Location", 
                #          h2('Select Town')
                #          ),
                # Metrics ----
                tabPanel("Select Metrics",
                         selectPar_ui(ns("select_metrics"))
                         )
                ,
                # Advanced Options ----
                tabPanel("Advanced Options",
                         advancedSelect_ui(ns("advanced_options"))
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
    df_par <- selectPar_server("select_metrics")
    
    df_advanced <- advancedSelect_server("advanced_options", df_par)
    
    # # Output reactive values ----
    # return(
    #   list(
    #   )
    # )
    
  })
}

# end Server Function

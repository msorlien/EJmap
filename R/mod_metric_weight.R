################################### HEADER ###################################
#  TITLE: mod_metric_weight.R
#  DESCRIPTION: Module to assign weight, minimum value to each metric category
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-04-25
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(shinyWidgets)
library(reactable)

########################################################################.
###                       User Interface                            ####
########################################################################.

METRIC_WEIGHT_UI <- function(id, cat_name) {
  
  ns <- NS(id)
  
  tagList(
    # Title ----
    h3(cat_name),
    # Table
    reactableOutput(ns("cat_table"))
  )
  
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

METRIC_WEIGHT_SERVER <- function(id, cat_code, metrics) {
  moduleServer(id, function(input, output, session) {
    
    # Create dataframe ----
    df_metric <- reactive ({
      metric_list %>%
        filter(METRIC_CODE %in% metrics()) %>%
        select("METRIC", "WEIGHT") %>%
        rename(Metric=METRIC, Weight=WEIGHT)
    })
    
    # Reactable table ----
    output$cat_table <- renderReactable({
      reactable(df_metric())
    })
    
    # # Output reactive values ----
    # return(
    #   list(
    #     metWeight = reactive({ df_metric() })
    #   )
    # )
    
  })
}

# end Server Function

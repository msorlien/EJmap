################################### HEADER ###################################
#  TITLE: mod_select_metric.R
#  DESCRIPTION: Module to select metrics within given category
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-04-25
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(shinyWidgets)

########################################################################.
###                       User Interface                            ####
########################################################################.

SELECT_METRIC_UI <- function(id, cat_name, cat_code) {
  
  ns <- NS(id)
  
  # Filter for metrics in category
  df_cat <- metric_list %>% 
    filter(CAT_CODE == cat_code)
  
  # Generate METRIC/METRIC_CODE list
  cat_choices <- df_cat$METRIC_CODE
  names(cat_choices) <- df_cat$METRIC
  
  # Select metrics
  tagList(
    pickerInput(
      ns('metricSelect'),
      label = h2(cat_name),
      choices = cat_choices,
      selected = df_cat$METRIC_CODE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE),
      multiple = TRUE
    )
  )
  
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

SELECT_METRIC_SERVER <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Output reactive values ----
    return(
      reactive({ input$metricSelect })
    )
    
  })
}

# end Server Function

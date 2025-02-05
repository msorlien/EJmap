################################### HEADER ###################################
#  TITLE: select_parameter_input.R
#  DESCRIPTION: Module to select metrics within given category
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2025-02-05
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

########################################################################.
###                       User Interface                            ####
########################################################################.

selectParInput_ui <- function(id, cat_code) {
  
  ns <- NS(id)
  
  # Filter for metrics in category
  df_metric <- metric_table %>% 
    filter(CAT_CODE == cat_code)
  
  # Find category name
  cat_name <- df_metric$CATEGORY[1]
  
  # Generate METRIC/METRIC_CODE list
  metric_choices <- df_metric$METRIC_CODE
  names(metric_choices) <- df_metric$METRIC
  
  # Select metrics
  tagList(
    shinyWidgets::pickerInput(
      ns('metricSelect'),
      label = h4(cat_name),
      choices = metric_choices,
      selected = df_metric$METRIC_CODE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = 'count > 2',
        container = 'body'),  # Allows dropdown overflow
      multiple = TRUE
    )
  )
  
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

selectParInput_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Output reactive values ----
    return(
      reactive({ input$metricSelect })
    )
    
  })
}

# end Server Function

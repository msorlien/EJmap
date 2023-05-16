################################### HEADER ###################################
#  TITLE: advanced_options.R
#  DESCRIPTION: Module to select location & parameters
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-15
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(shinyWidgets)

########################################################################.
###                       User Interface                            ####
########################################################################.

advancedSelect_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    # Button to show/hide advanced options ----
    actionButton(ns('btn_advanced'),
                 label = 'Show Advanced Options'),
    
    # Conditional Panel 
    conditionalPanel(
      condition = paste0('input["', ns('btn_advanced'), '"] % 2 != 0'), 
      
      # Title ----
      h3('Advanced Options'),
      
      # Categories ----
      h4('Categories'),
      weightCat_ui(ns('cat')),
      awesomeRadio(
        inputId = ns('exceed_all'),
        label = 'Must match all minimum scores?', 
        choices = c('Yes'='AND', 'No'='OR'),
        selected = 'AND',
        inline = TRUE, 
        checkbox = TRUE),
      
      # Percentiles ----
      h4('Percentiles'),
      
      # * Minimum value ----
      pickerInput(
        ns('percentile_min'),
        label = 'Minimum Value',
        choices = c(50, 60, 70, 80, 90, 95),
        selected = 80),
      
      h4('Metrics'),
      weightMetric_ui(ns('socvul'), 'Social Vulnerability'),
      weightMetric_ui(ns('health'), 'Health'),
      weightMetric_ui(ns('envbur'), 'Environmental Burden'),
      weightMetric_ui(ns('climate'), 'Climate')
      
      )
    )
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

advancedSelect_server <- function(id, metric_list) {
  moduleServer(id, function(input, output, session) {

    # Toggle button text ----
    observeEvent(input$btn_advanced, {
      if (input$btn_advanced %% 2 != 0) {
        updateActionButton(session, 'btn_advanced', 
                           label = 'Hide Advanced Options')
      } else {
        updateActionButton(session, 'btn_advanced', 
                           label = 'Show Advanced Options')
      }
    })
    
    # Split metrics by category ----
    socvul_metrics <- reactive({
      metric_list() %>% filter(CAT_CODE == 'SOCVUL')
    }) 
    health_metrics <- reactive({
      metric_list() %>% filter(CAT_CODE == 'HEALTH')
    }) 
    envbur_metrics <- reactive({
      metric_list() %>% filter(CAT_CODE == 'ENVBUR')
    }) 
    climate_metrics <- reactive({
      metric_list() %>% filter(CAT_CODE == 'CLIMATE')
    }) 
    
    # Add module servers ----
    cat <- weightCat_server('cat', metric_list)
    socvul <- weightMetric_server('socvul', 'SOCVUL', socvul_metrics)
    health <- weightMetric_server('health', 'HEALTH', health_metrics)
    envbur <- weightMetric_server('envbur', 'ENVBUR', envbur_metrics)
    climate <- weightMetric_server('climate', 'CLIMATE', climate_metrics)
    
    # Category weight dataframe ----
    
    df_cat <- reactive({
      
      df_metric <- metric_list() %>%
        select('CATEGORY', 'CAT_CODE') %>%
        distinct()  # Drop duplicate rows
      
      df_weight <- cat() %>%
        rename(CATEGORY=Category, WEIGHT=Weight, MIN_SCORE='Minimum Score')
      
      df_join <- merge(df_metric, df_weight, by='CATEGORY')
      
      return(df_join)
      
    })
    
    # Metric weight dataframe ----
    df_metrics <- reactive({
      
      df_metric <- metric_list() 
      
      df_weight <- rbind(socvul(), health(), envbur(), climate()) %>%
        rename(METRIC=Metric, WEIGHT=Weight)
      
      df_join <- merge(df_metric, df_weight, by='METRIC')
      
      return(df_join)
    })
    
    
    # Output reactive values ----
    return(
      list(
        
        percentile_min = reactive({ input$percentile_min }),
        exceed_all = reactive({ input$exceed_all }),
        df_cat = reactive({ df_cat() }),
        df_metric = reactive({ df_metrics() })
        
      )
    )
    
  })
}

# end Server Function

################################### HEADER ###################################
#  TITLE: advanced_options.R
#  DESCRIPTION: Module to select location & parameters
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-24
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
    
    h4('Metrics'),
    weightMetric_ui(ns('socvul'), 'Social Vulnerability'),
    weightMetric_ui(ns('health'), 'Health'),
    weightMetric_ui(ns('envbur'), 'Environmental Burden'),
    weightMetric_ui(ns('climate'), 'Climate')
    
    )
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

advancedSelect_server <- function(id, metric_list) {
  moduleServer(id, function(input, output, session) {
    
    # Set namespace ----
    ns <- session$ns
    
    # Define variables ----
    adv_opt <- reactiveValues(
      percentile_min=80
      )
    
    observeEvent(input$btn_advanced, {
      # display a modal dialog with a header, textinput and action buttons
      showModal(modalDialog(
        tags$h2('Advanced Options'),  # Title defaults to H4 - unacceptable
        h3('Category Weight'),
        h3('Minimum Category Scores'),
        h3('Minimum Percentile'),
        'Percentiles are... yada yada',
        pickerInput(
          ns('percentile_min'),
          label = 'Minimum Percentile',
          choices = c(50, 60, 70, 80, 90, 95),
          selected = adv_opt$percentile_min),
        h3('Metric Weight'),
        footer=tagList(
          actionButton(ns('submit'), 'Submit'),
          modalButton('Cancel')
        )
      ))
    })
    
    # Save edits on submit
    observeEvent(input$submit, {
      adv_opt$percentile_min <- input$percentile_min
      removeModal()
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
        
        percentile_min = reactive({ as.numeric(adv_opt$percentile_min) }),
        exceed_all = reactive({ input$exceed_all }),
        df_cat = reactive({ df_cat() }),
        df_metric = reactive({ df_metrics() })
        
      )
    )
    
  })
}

# end Server Function

################################### HEADER ###################################
#  TITLE: mod_advanced_options.R
#  DESCRIPTION: Module to select advanced options for selected metrics
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-07-19
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
    
    # Title ----
    h3('Advanced Options'),
        
    # Categories ----
    h4('Category Weight'),
    'Double click on a cell to edit.',
    weightVar_ui(ns('cat_weight')),
    
    h4('Minimum Category Score'),
    weightCat_ui(ns('cat')),
    awesomeRadio(
      inputId = ns('exceed_all'),
      label = 'Must match all minimum scores?', 
      choices = c('Yes'='AND', 'No'='OR'),
      selected = 'AND',
      inline = TRUE, 
      checkbox = TRUE),
      
    h4('Minimum Percentile'),
    'Percentiles indicate what percent of block groups have a lower 
      score than the selected block group.',
    pickerInput(
      ns('percentile_min'),
      label = 'Minimum Percentile',
      choices = c(50, 60, 70, 80, 90, 95),
      selected = 80),
    
    h4('Metrics'),
    weightMetric_ui(ns('socvul'), 'Social Vulnerability'),
    weightMetric_ui(ns('health'), 'Health'),
    weightMetric_ui(ns('envbur'), 'Environmental Burden'),
    weightMetric_ui(ns('climate'), 'Climate'),
    
    # * Save Button ----
    actionButton(ns('btn_save'),
                 label = 'Save'),
    
    # * Cancel Button ----
    actionButton(ns('btn_cancel'),
                 label = 'Cancel')
  )
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

advancedSelect_server <- function(id, metric_list) {
  moduleServer(id, function(input, output, session) {
    
    # Set namespace ----
    ns <- session$ns
    
    # List selected categories ----
    cat_list <- reactive({ unique(metric_list()$CAT_CODE) })
    
    # Add row names to default tables ----
    row.names(cat_table) = cat_table$CATEGORY
    row.names(metric_table) = metric_table$METRIC
    
    # Create reactiveValues object ----
    values <- reactiveValues(cats = cat_table, metrics = metric_table, 
                             min_percentile = 80, min_pass = 0)
    
    # Table: category weight ----
    df_cat_weight <- reactive({
      values$cats %>%
        filter(CAT_CODE %in% cat_list()) %>%
        select(WEIGHT)
    })
    
    weightVar_server('cat_weight', df_cat_weight(), c('Weight' = 'WEIGHT'), 
                     input$btn_cancel)
    
    # output$cat_weight <- DT::renderDataTable({
    #   DT::datatable(
    #     df_cat_weight(),
    #     colnames = c('Weight' = 'WEIGHT'),
    #     options = list(
    #       dom = 't',
    #       # scrollY = '250px',
    #       ordering = F,
    #       pageLength = nrow(df_cat_weight())
    #       ),
    #     editable = list(
    #       target = 'column',
    #       disable = list(columns = 0)
    #     )
    #     )
    # })
    
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
        
        btn_save = reactive({ input$btn_save }),
        btn_cancel = reactive({ input$btn_cancel }),
        
        percentile_min = reactive({ as.numeric(input$percentile_min) }),
        exceed_all = reactive({ input$exceed_all }),
        df_cat = reactive({ df_cat() }),
        df_metric = reactive({ df_metrics() })
        
      )
    )
    
  })
}

# end Server Function

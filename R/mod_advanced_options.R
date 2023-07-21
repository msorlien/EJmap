################################### HEADER ###################################
#  TITLE: mod_advanced_options.R
#  DESCRIPTION: Module to select advanced options for selected metrics
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-07-21
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
    weightVar_ui(ns('cat_weight')),
    
    h4('Minimum Category Score'),
    weightVar_ui(ns('cat_min')),
    h4('OLD STUFF'),
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

advancedSelect_server <- function(id, metric_list, btn_reset) {
  moduleServer(id, function(input, output, session) {
    
    # Set namespace ----
    ns <- session$ns
    
    # List selected categories ----
    cat_list <- reactive({ unique(metric_list()$CAT_CODE) })
    
    # Add row names to default tables ----
    row.names(cat_table) = wrap_text(cat_table$CATEGORY, 15, '<br>')
    row.names(metric_table) = wrap_text(metric_table$METRIC, 15, '<br>')
    
    # Create reactiveValues object ----
    values <- reactiveValues(cats = cat_table, metrics = metric_table, 
                             min_percentile = 80, min_pass = 0)
    
    # Table: category weight ----
    # * Modify table ----
    df_cat_weight <- reactive({
      values$cats %>%
        filter(CAT_CODE %in% cat_list()) %>%
        select(WEIGHT) %>%
        rename(Weight = WEIGHT)
    })
    
    # * Add server ----
    s_cat_weight <- weightVar_server('cat_weight', df_cat_weight, btn_reset)
    
    # # * Save data ----
    # observeEvent(input$btn_save, {
    #   base_table <- values$cats
    # 
    #   new_data <- s_cat_weight() %>%
    #     rename(WEIGHT = Weight) %>%
    #     tibble::rownames_to_column('CATEGORY') %>%
    #     mutate(CATEGORY = gsub('<br>', ' ', CATEGORY))
    # 
    #   new_table <- left_join(base_table, new_data, by='CATEGORY') %>%
    #     mutate(WEIGHT = ifelse(is.na(WEIGHT.y), WEIGHT.x, WEIGHT.y)) %>%
    #     select(!c(WEIGHT.x, WEIGHT.y))
    # 
    #   values$cat <- new_table
    # })
    
    # Table: min category score ----
    df_cat_min <- reactive({
      values$cats %>%
        filter(CAT_CODE %in% cat_list()) %>%
        select(MIN_SCORE) %>%
        rename('Minimum<br>Score' = MIN_SCORE)
    })
    
    weightVar_server('cat_min', df_cat_min, btn_reset)
    
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

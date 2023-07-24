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
    'Minimum scores of 0 are ignored.',
    weightVar_ui(ns('cat_min')),
    pickerInput(
      ns('min_pass'),
      label = 'EJ areas must meet or exceed...',
      choices = c('All  minimum category scores' = 4, 
                  'At least one minimum category score' = 1,
                  'At least two minimum category scores' = 2,
                  'At least three minimum category scores' = 3),
      selected = '4',
      options = list(
        container = 'body')  # Allows dropdown overflow
      ),
      
    h4('Minimum Percentile'),
    'Percentiles indicate what percent of block groups have a lower 
      score than the selected block group.',
    pickerInput(
      ns('percentile_min'),
      label = 'Minimum Percentile',
      choices = c(50, 60, 70, 80, 90, 95),
      selected = 80),
    
    h4('Metrics'),
    weightVar_ui(ns('socvul'), 'Social Vulnerability'),
    weightVar_ui(ns('health'), 'Health'),
    weightVar_ui(ns('envbur'), 'Environmental Burden'),
    weightVar_ui(ns('climate'), 'Climate'),
    
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
    cat_list <- reactive({ 
      df_cat <- metric_table %>% 
        dplyr::filter(METRIC_CODE %in% metric_list())
      
      cat_list <- unique(df_cat$CAT_CODE)
      
      return(cat_list)
      })
    
    # Add row names ----
    row.names(cat_table) = wrap_text(cat_table$CATEGORY, 15, '<br>')
    row.names(metric_table) = wrap_text(metric_table$METRIC, 15, '<br>')
    
    # Create reactiveValues object ----
    values <- reactiveValues(cats = cat_table, metrics = metric_table, 
                             min_percentile = 80, min_pass = 4)
    
    # Cat Tables ----
    df_cat <- reactive({
      values$cats %>%
        filter(CAT_CODE %in% cat_list())
    })
    
    # * Category weight ----
    df_cat_weight <- reactive({
      df_cat() %>%
        select(WEIGHT) %>%
        rename(Weight = WEIGHT)
    })
    
    # * Min category score ----
    df_cat_min <- reactive({
      df_cat() %>%
        select(MIN_SCORE) %>% 
        mutate(MIN_SCORE = ifelse(MIN_SCORE == 0, NA_real_, MIN_SCORE)) %>%
        rename('Minimum<br>Score' = MIN_SCORE)
    })
    
    # Metric tables ----
    df_metric <- reactive({
      values$metrics %>%
        filter(METRIC_CODE %in% metric_list())
    })
    
    # * Social vulnerability ----
    df_socvul <- reactive({
      df_metric() %>%
        filter(CAT_CODE == 'SOCVUL') %>%
        select(WEIGHT) %>%
        rename(Weight = WEIGHT)
    })
    
    # * Health ----
    df_health <- reactive({
      df_metric() %>%
        filter(CAT_CODE == 'HEALTH') %>%
        select(WEIGHT) %>%
        rename(Weight = WEIGHT)
    })
    
    # * Environmental Burden ----
    df_envbur <- reactive({
      df_metric() %>%
        filter(CAT_CODE == 'ENVBUR') %>%
        select(WEIGHT) %>%
        rename(Weight = WEIGHT)
    })
    
    # * Climate ----
    df_climate <- reactive({
      df_metric() %>%
        filter(CAT_CODE == 'CLIMATE') %>%
        select(WEIGHT) %>%
        rename(Weight = WEIGHT)
    })
    
    # Add module servers ----
    cat_weight <- weightVar_server('cat_weight', df_cat_weight, btn_reset)
    cat_min <- weightVar_server('cat_min', df_cat_min, btn_reset)
    socvul <- weightVar_server('socvul', df_socvul, btn_reset)
    health <- weightVar_server('health', df_health, btn_reset)
    envbur <- weightVar_server('envbur', df_envbur, btn_reset)
    climate <- weightVar_server('climate', df_climate, btn_reset)
    
    # Save data ----
    observeEvent(input$btn_save, {
      # Update category data ----
      df_cat_weight <- update_table_values(values$cats, cat_weight(), 
                                           'WEIGHT', 'CATEGORY')
      df_cat_min <- update_table_values(df_cat_weight, cat_min(), 'MIN_SCORE',
                                        'CATEGORY')
      values$cats <- df_cat_min
    })

    # Output reactive values ----
    return(
      list(
        
        btn_save = reactive({ input$btn_save }),
        btn_cancel = reactive({ input$btn_cancel }),
        
        percentile_min = reactive({ as.numeric(input$percentile_min) }),
        # exceed_all = reactive({ 'AND' }),
        df_cat = reactive({ df_cat() }),
        df_metric = reactive({ df_metric() })
        
      )
    )
    
  })
}

# end Server Function

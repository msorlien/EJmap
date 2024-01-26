#  TITLE: mod_advanced_options.R
#  DESCRIPTION: Module to select advanced options for selected metrics
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2024-01-25
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
# -----------------------------------------------------------------------------.

library(shinyWidgets)

# Module ui -------------------------------------------------------------------

advancedSelect_ui <- function(id, percentiles = c("N_", "P_")) {
  
  ns <- NS(id)
  
  tagList(
    
    # Title ----
    h3('Advanced Options'),
        
    # Categories ----
    
    h4('Category Weight'),
    weightVar_ui(ns('cat_weight')),

    h4('Minimum Category Score'),
    'Set minimum category scores for environmental justice areas. If you do not 
    wish to set a minimum score, set to 0 or leave blank.',
    weightVar_ui(ns('cat_min')),
    # Min number of cat scores
    conditionalPanel(
      condition = paste0('output["', ns('cat_count'), '"] > 1'),
      shinyWidgets::pickerInput(
        ns('min_pass'),
        label = 'EJ areas must meet or exceed...',
        choices = c('All  minimum category scores' = 4, 
                    'At least one minimum category score' = 1,
                    'At least two minimum category scores' = 2,
                    'At least three minimum category scores' = 3),
        selected = '4',
        options = list(
          container = 'body')  # Allows dropdown overflow
      )
    ),
    
    # Percentiles -----
    h4('Minimum Percentile'),
    'Percentiles are a way to rank and compare data. A block group that is 
    listed as the 80th percentile for a metric has a higher value 
    than 80% of the block groups.',
    shinyWidgets::pickerInput(
      ns('percentile_min'),
      label = 'Minimum Percentile',
      choices = c(50, 60, 70, 80, 90, 95),
      selected = 80),
    
    conditionalPanel(
      condition = paste0(length(percentiles), " > 1"),
      h4("Percentile Type"),
      "Regional percentiles rank block groups across the entire region. State 
      percentiles rank block groups within a state.",
      shinyWidgets::pickerInput(
        ns("percentile_type"),
        label = "Percentile Type", 
        choices = list_percentile_codes(percentiles),
        selected = list_percentile_codes(percentiles)[1]
      )
    ),
    
    # Metrics ----
    h4('Metric Weight'),
    conditionalPanel(
      condition = paste0('output["', ns('cat_socvul'), '"] == 1'),
      weightVar_ui(ns('socvul'), 'Social Vulnerability')
      ),
    conditionalPanel(
      condition = paste0('output["', ns('cat_health'), '"] == 1'),
      weightVar_ui(ns('health'), 'Health')
      ),
    conditionalPanel(
      condition = paste0('output["', ns('cat_envbur'), '"] == 1'),
      weightVar_ui(ns('envbur'), 'Environmental Burden')
    ),
    conditionalPanel(
      condition = paste0('output["', ns('cat_climate'), '"] == 1'),
      weightVar_ui(ns('climate'), 'Climate')
      ),
    
    # Save Button ----
    actionButton(ns('btn_save'),
                 label = 'Save'),
    
    # Cancel Button ----
    actionButton(ns('btn_cancel'),
                 label = 'Cancel')
  )
}

# Module server ---------------------------------------------------------------

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
    
    # Pass info to ui ----
    output$cat_count <- renderText({ length(cat_list()) })
    output$cat_socvul <- renderText({ if('SOCVUL' %in% cat_list()) 1 else 0 })
    output$cat_health <- renderText({ if('HEALTH' %in% cat_list()) 1 else 0 })
    output$cat_envbur <- renderText({ if('ENVBUR' %in% cat_list()) 1 else 0 })
    output$cat_climate <- renderText({ if('CLIMATE' %in% cat_list()) 1 else 0 })
    
    outputOptions(output, 'cat_count', suspendWhenHidden = FALSE)
    outputOptions(output, 'cat_socvul', suspendWhenHidden = FALSE)
    outputOptions(output, 'cat_health', suspendWhenHidden = FALSE)
    outputOptions(output, 'cat_envbur', suspendWhenHidden = FALSE)
    outputOptions(output, 'cat_climate', suspendWhenHidden = FALSE)
    
    # Add row names to tables ----
    row.names(cat_table) = wrap_text(cat_table$CATEGORY, 15, '<br>')
    row.names(metric_table) = wrap_text(metric_table$METRIC, 15, '<br>')
    
    # Create reactiveValues object ----
    values <- reactiveValues(
      cats = cat_table, 
      metrics = metric_table, 
      min_percentile = 80, 
      percentile_type = "N_",
      min_pass = 4)
    
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
    
    # Update/reset pickerInput -----
    observeEvent(btn_reset(), {
      
      # * Set variables ----
      cat_length <- length(cat_list())
      cat_choices <- c('All  minimum category scores' = 4, 
                        'At least one minimum category score' = 1,
                        'At least two minimum category scores' = 2,
                        'At least three minimum category scores' = 3)
      cat_selected <- values$min_pass
      
      if (cat_length <= 2) {
        cat_choices <- c('All  minimum category scores' = 4, 
                          'At least one minimum category score' = 1)
      } else if (cat_length == 3) {
        cat_choices <- c('All  minimum category scores' = 4, 
                          'At least one minimum category score' = 1,
                          'At least two minimum category scores' = 2)
      }
      if (cat_length - 1 < values$min_pass) {
        cat_selected <- 4
      }
      
      # * Update min percentile ----
      shinyWidgets::updatePickerInput(session = session,
                        inputId = 'percentile_min',
                        selected = values$min_percentile)
      
      # * Update percentile type ----
      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "percentile_type",
                                      selected = values$percentile_type)
      
      # * Update min categories ----
      shinyWidgets::updatePickerInput(session = session,
                        inputId = 'min_pass',
                        choices = cat_choices,
                        selected = cat_selected)
    })
    
    # Save data ----
    observeEvent(input$btn_save, {
      # * Update category data ----
      df_cat_weight <- update_table(values$cats, cat_weight(), 'WEIGHT', 
                                    'CATEGORY')
      df_cat_min <- update_table(df_cat_weight, cat_min(), 'MIN_SCORE', 
                                 'CATEGORY')
      values$cats <- df_cat_min
      
      # * Update metric data ----
      df_met_join <- rbind(socvul(), health(), envbur(), climate())
      df_met_weight <- update_table(values$metrics, df_met_join, 'WEIGHT', 
                                    'METRIC')
      values$metrics <- df_met_weight
      
      # * Update dropdown values ----
      values$min_percentile <- input$percentile_min
      values$percentile_type <- input$percentile_type
      values$min_pass <- input$min_pass
    })

    # Output reactive values ----
    return(
      list(
        
        btn_save = reactive({ input$btn_save }),
        btn_cancel = reactive({ input$btn_cancel }),
        
        percentile_min = reactive({ as.numeric(values$min_percentile) }),
        percentile_type = reactive ({ values$percentile_type }),
        min_pass = reactive({ as.numeric(values$min_pass) }),
        df_cat = reactive({ df_cat() }),
        df_metric = reactive({ df_metric() })
        
      )
    )
    
  })
}

# end Server Function

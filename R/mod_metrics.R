#  TITLE: select_metrics.R
#  DESCRIPTION: Module to select parameters

# UI --------------------------------------------------------------------------

selectPar_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    # Enable javascript ----
    shinyjs::useShinyjs(),
    
    # Secret tabs ----
    bslib::navset_hidden(
      id = ns('tabset_metrics'),
      # Tab 1 ----
      bslib::nav_panel_hidden(
        'tab_metrics',
        
        # * Select metrics ----
        h3('Metrics'),
        selectParInput_ui(ns('socvul_metrics'), 'SOCVUL'),
        selectParInput_ui(ns('health_metrics'), 'HEALTH'),
        selectParInput_ui(ns('envbur_metrics'), 'ENVBUR'),
        selectParInput_ui(ns('climate_metrics'), 'CLIMATE'),
        
        # Minimum score ----
        h3('Minimum Score'),
        numericInput(
          ns('min_ej'),
          'Set minimum EJ score for environmental justice areas.',
          value = 0,
          min = 0,
          max = 100
        ),
        
        # * Advanced Options Button ----
        actionButton(
          ns('btn_advanced'),
          label = 'Show Advanced Options'
        ),
        
        # * Calculate Button ----
        actionButton(
          ns('btn_calculate'),
          label = 'Calculate Score'
        )
      ),
      # Tab 2 ----
      bslib::nav_panel_hidden(
        'tab_advanced',
        # * Advanced Options ----
        advancedSelect_ui(ns('advanced_options'))
      )
    )
  )
}

# Server -----------------------------------------------------------------------

selectPar_server <- function(id, input_shp, percentile_type) {
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)  # Set namespace
    
    # Add metric servers ----
    socvul <- selectParInput_server('socvul_metrics')
    health <- selectParInput_server('health_metrics')
    envbur <- selectParInput_server('envbur_metrics')
    climate <- selectParInput_server('climate_metrics')
    
    # Advanced Options ----
    # * Prep data for module ----
    metrics_list <- reactive({ c(socvul(), health(), envbur(), climate()) })
    btn_reset <- reactive({ input$btn_advanced })
    
    # * Add module ----
    adv_opt <- advancedSelect_server(
      'advanced_options', 
      metric_list = metrics_list,
      percentile_type = percentile_type,
      btn_reset = btn_reset
    )

    # * Show/hide button ----
    observeEvent(input$btn_advanced, {
      bslib::nav_select('tabset_metrics', 'tab_advanced')
    })
    observeEvent(c(adv_opt$btn_save(), adv_opt$btn_cancel()), {
      bslib::nav_select('tabset_metrics', 'tab_metrics')
    })
    
    # Disable buttons if no metrics selected ----
    shinyjs::disable('btn_calculate')
    shinyjs::disable('btn_advanced')

    observe({
      if (length(metrics_list()) > 0) {
        shinyjs::enable('btn_calculate')
        shinyjs::enable('btn_advanced')
      } else {
        shinyjs::disable('btn_calculate')
        shinyjs::disable('btn_advanced')
      }
    })
    
    # Calculate score ----
    shp_output <- eventReactive(input$btn_calculate, {
      
      # Drop pre-existing score columns
      shp_output <- input_shp %>%
        dplyr::select(
          !tidyselect::contains(
            c('_SOCVUL', '_HEALTH', '_ENVBUR', '_CLIMATE', '_SCORE', '_ISEJ')
          )
        )
      
      # Calculate score
      shp_output <- calculate_score(
        input_shp = shp_output, 
        percentile_min = adv_opt$percentile_min(),
        prefix_list = adv_opt$percentile_type(),
        min_pass = adv_opt$min_pass(),
        min_ej = as.numeric(input$min_ej),
        df_metrics = adv_opt$df_metric(), 
        df_categories = adv_opt$df_cat()
      ) 
      
      # Set -999999 to NA
      shp_output <- shp_output %>%
        dplyr::mutate(
          dplyr::across(
            tidyselect::where(is.numeric), 
            ~na_if(., -999999)
          )
        )
      
      return(shp_output)
    })
    
    # Update output values ----
    values <- reactiveValues(
      df_metric = metric_table, 
      df_cat = cat_table,
      percentile_min = 80,
      percentile_type = percentile_type[1],
      min_pass = 4,
      min_ej = 0)
    observe({ 
      values$df_metric <- adv_opt$df_metric()
      values$df_cat <- adv_opt$df_cat()
      values$percentile_min <- adv_opt$percentile_min()
      values$percentile_type <- adv_opt$percentile_type()
      values$min_pass <- adv_opt$min_pass()
      values$min_ej <- as.numeric(input$min_ej)
    }) %>%
      bindEvent(input$btn_calculate)
    
    # Output reactive values ----
    return(
      list(
        output_shp = reactive({ shp_output() }),
        df_metric = reactive({ values$df_metric }),
        df_cat = reactive({ values$df_cat }),
        percentile_min = reactive({ values$percentile_min }),
        percentile_type = reactive({ values$percentile_type }),
        min_pass = reactive({ values$min_pass }),
        min_ej = reactive({ values$min_ej }),
        button = reactive({ input$btn_calculate })
      )
    )
    
  })
}

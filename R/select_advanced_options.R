################################### HEADER ###################################
#  TITLE: select_advanced_options.R
#  DESCRIPTION: Module to select location & parameters
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-04-27
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
    
    # Percentiles ----
    h2('Percentiles'),
    
    # * NBEP vs state ----
    awesomeRadio(
      inputId = ns("percentile_type"),
      label = NULL, 
      choices = c("Compare to NBEP"="NBEP", "Compare to State"="State"),
      selected = "NBEP",
      inline = TRUE, 
      checkbox = TRUE
    ),
    
    # * Minimum value ----
    numericInput(
      inputId=ns("percentile_min"),
      label="Minimum Value",
      value=80,
      min = 50,
      max = 95
    ),
    
    # Categories ----
    h2('Categories'),
    weightCat_ui(ns("cat")),
    
    h2('Metrics'),
    weightMetric_ui(ns("socvul"), "Social Vulnerability"),
    weightMetric_ui(ns("health"), "Health"),
    weightMetric_ui(ns("envbur"), "Environmental Burden"),
    weightMetric_ui(ns("climate"), "Climate"),
    
    '(calculate) (cancel? reset?)'
  )
  
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

advancedSelect_server <- function(id, metric_list) {
  moduleServer(id, function(input, output, session) {
    
    # Divide metrics by category ----
    socvul_metrics <- reactive({
      metric_list() %>% filter(CAT_CODE == "SOCVUL")
    }) 
    health_metrics <- reactive({
      metric_list() %>% filter(CAT_CODE == "HEALTH")
    }) 
    envbur_metrics <- reactive({
      metric_list() %>% filter(CAT_CODE == "ENVBUR")
    }) 
    climate_metrics <- reactive({
      metric_list() %>% filter(CAT_CODE == "CLIMATE")
    }) 
    
    # Add module servers ----
    cat <- weightCat_server("cat", metric_list)
    socvul <- weightMetric_server("socvul", "SOCVUL", socvul_metrics)
    health <- weightMetric_server("health", "HEALTH", health_metrics)
    envbur <- weightMetric_server("envbur", "ENVBUR", envbur_metrics)
    climate <- weightMetric_server("climate", "CLIMATE", climate_metrics)
    
    # Category weight dataframe ----
    
    df_cat <- reactive({
      
      df_metric <- metric_list() %>%
        select('CATEGORY', 'CAT_CODE') %>%
        distinct()  # Drop duplicate rows
      
      df_weight <- cat() %>%
        rename(Category=CATEGORY, Weight=WEIGHT, "Minimum Score"=MIN_SCORE)
      
      df_join <- merge(df_metric, df_weight, by="CATEGORY")
      
      return(df_join)
      
    })
    
    # Metric weight dataframe ----
    df_metrics <- reactive({
      
      df_metric <- metric_list() 
      
      df_weight <- rbind(socvul(), health(), envbur(), climate()) %>%
        rename(Metric=METRIC, Weight=WEIGHT)
      
      df_join <- merge(df_metric, df_weight, by="METRIC")
      
      return(df_join)
    })
    
    
    # Output reactive values ----
    return(
      list(
        
        percentile_type = reactive({ input$percentile_type }),
        percentile_min = reactive({ input$percentile_min }),
        df_cat = reactive({ df_cat() }),
        df_metric = reactive({ df_metrics() })
        
      )
    )
    
  })
}

# end Server Function

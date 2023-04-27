################################### HEADER ###################################
#  TITLE: weight_metrics.R
#  DESCRIPTION: Module to assign weight, minimum value to metrics
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-04-27
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(shinyWidgets)
library(reactable)
library(htmltools)

########################################################################.
###                       User Interface                            ####
########################################################################.

weightMetric_ui <- function(id, cat_name) {
  
  ns <- NS(id)
  
  tagList(
    # Conditional panel ----
    conditionalPanel(
      condition = paste0('output["', ns('metric_count'), '"] > 0'),
      # Title ----
      h3(cat_name),
      # Edit button ---- 
      actionButton(inputId = ns("edit"),
                   label = "Edit Selected Row"),
      # Table ----
      reactableOutput(ns("table"))
    )
  )
  
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

weightMetric_server <- function(id, cat_code, selected_var) {
  moduleServer(id, function(input, output, session) {
    
    # Set namespace ----
    ns <- session$ns
    
    # Count selected variables ----
    
    # * Count rows ----
    output$metric_count <- renderText({ nrow(selected_var()) })
    
    outputOptions(output, "metric_count", suspendWhenHidden = FALSE)
    
    # Create dataframe ----
    df_var <- metric_table %>%
      filter(CAT_CODE == cat_code) %>%
      select(METRIC) %>%
      rename(Metric=METRIC) %>%
      add_column("Weight" = 1)
    
    # Reactable table ----
    # Code for reactable edits by DeepanshKhurana
    # https://gist.github.com/DeepanshKhurana/95027b7130b1be0d7fea6fb9d85df89a
    
    # * Selected row ----
    selected_row <- reactive({
      getReactableState("table")$selected
    })
    
    # * Dataframe as reactive values -----
    values <- reactiveValues(dataframe = df_var)
    
    # * Edit data ----
    observeEvent(input$edit, {
      if (!is.null(selected_row())) {
        showModal(
          modalDialog(
            title = "Edit Values",
            # Metric name
            p(values$dataframe[selected_row(), "Metric"]),
            # Edit weight
            numericInput(
              inputId=ns("weight"),
              label="Weight",
              value=values$dataframe[selected_row(), "Weight"],
              min = 0,
              max = 10
            ),
            easyClose = TRUE,
            footer = actionButton(ns("save"), "Save")
          )
        )
      }
    })
    
    # * Save edits ----
    observeEvent(input$save, {
      # Update weight if between 0 and 10
      if(between(input$weight, 0, 10)) {
        values$dataframe[selected_row(), "Weight"] <- input$weight
      }
      # Close module
      removeModal()
    })
    
    # * Build table ----
    reactable_table <- reactive({
      reactable(values$dataframe,
                selection = "single")
    })
    
    output$table <- renderReactable(
      reactable_table()
    )
    
    # Output reactive values ----
    return(
      reactive({ values$dataframe })
    )
    
  })
}

# end Server Function

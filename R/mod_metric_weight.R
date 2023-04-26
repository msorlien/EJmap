################################### HEADER ###################################
#  TITLE: mod_metric_weight.R
#  DESCRIPTION: Module to assign weight, minimum value to each metric category
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-04-25
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(shinyWidgets)
library(reactable)
library(htmltools)

########################################################################.
###                       User Interface                            ####
########################################################################.

METRIC_WEIGHT_UI <- function(id, cat_name) {
  
  ns <- NS(id)
  
  tagList(
    # Conditional panel ----
    conditionalPanel(
      condition = paste0('output["', ns('metric_count'), '"] > 0'),
      # Title ----
      h3(cat_name),
      # Edit button ---- 
      actionButton(inputId = ns("edit"),
                   label = "Edit"),
      # Table ----
      reactableOutput(ns("table"))
    )
  )
  
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

METRIC_WEIGHT_SERVER <- function(id, cat_code, metrics) {
  moduleServer(id, function(input, output, session) {
    
    # Set namespace ----
    ns <- session$ns
    
    # Count selected variables ----
    output$metric_count <- renderText({length(metrics())})
    
    outputOptions(output, "metric_count", suspendWhenHidden = FALSE)
    
    # Create dataframe ----
    df_metric <- reactive({
      metric_list %>%
        filter(METRIC_CODE %in% metrics()) %>%
        select("METRIC", "WEIGHT") %>%
        rename(Metric=METRIC, Weight=WEIGHT)
    })
    
    # Reactable table ----
    # Code for reactable edits by DeepanshKhurana
    # https://gist.github.com/DeepanshKhurana/95027b7130b1be0d7fea6fb9d85df89a
    
    # * Selected row ----
    selected_row <- reactive({
      getReactableState("table")$selected
    })
    
    
    # * Dataframe as reactive values
    values <- reactive({
      reactiveValues(dataframe = df_metric())
    })
    
    # * Edit data ----
    observeEvent(input$edit, {
      if (!is.null(selected_row())) {
        showModal(
          modalDialog(
            title = "Edit Values",
            # Metric name
            p(values()$dataframe[selected_row(), "Metric"]),
            # Edit weight
            numericInput(
              inputId=ns("weight"),
              label="Weight",
              value=values()$dataframe[selected_row(), "Weight"],
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
        values()$dataframe[selected_row(), "Weight"] <- input$weight
      }
      # Close module
      removeModal()
    })
    
    # * Build table ----
    reactable_table <- reactive({
      reactable(values()$dataframe,
                selection = "single")
    })

    output$table <- renderReactable(
      reactable_table()
    )
    
    # # Output reactive values ----
    # return(
    #   list(
    #     metWeight = reactive({ df_metric() })
    #   )
    # )
    
  })
}

# end Server Function

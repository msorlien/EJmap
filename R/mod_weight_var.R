#  TITLE: mod_weight_var.R
#  DESCRIPTION: Module to produce table ui to select var weight, min score for
#    advanced options
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2025-02-05
#  GIT REPO: nbep/ejmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
# -----------------------------------------------------------------------------.

# UI --------------------------------------------------------------------------

weightVar_ui <- function(id, var_name = NA) {
  
  ns <- NS(id)
  
  tagList(
    if (!is.na(var_name)){
      h5(var_name)
    },
    rhandsontable::rHandsontableOutput(ns('table'))
  )
}

# Server ----------------------------------------------------------------------

weightVar_server <- function(id, input_df, btn_reset) {
  moduleServer(id, function(input, output, session) {
    
    # Create reactiveValues ----
    x = reactiveValues(df = NULL)
    
    observe({ x$df <- input_df() })
    
    # Create table ----
    output$table <- rhandsontable::renderRHandsontable({
      btn_reset()
      rhandsontable::rhandsontable(
        x$df,
        rowHeaderWidth = 140) %>%
        rhandsontable::hot_cols(colWidths = 100) %>%
        rhandsontable::hot_validate_numeric(cols = 1, min = 0, max = 100)
    })
    
    # Update table ---
    observeEvent(input$table, {
      x$df <- rhandsontable::hot_to_r(input$table)
    })
    
    # Reset Table -----
    observeEvent(btn_reset(), {
      x$df <- input_df()
    })
    
    # Output reactive values ----
    return(
      reactive({ x$df })
      )
    
  })
}

# end Server Function

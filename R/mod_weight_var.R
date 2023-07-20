#  TITLE: mod_weight_var.R
#  DESCRIPTION: Module to produce table ui to select var weight, min score for
#    advanced options
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-07-20
#  GIT REPO: nbep/ejmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
# -----------------------------------------------------------------------------.

library(shinyWidgets)

# UI --------------------------------------------------------------------------

weightVar_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    DT::DTOutput(ns('table'), fill = FALSE)
  )
}

# Server ----------------------------------------------------------------------

weightVar_server <- function(id, input_df, col_names, btn_cancel) {
  moduleServer(id, function(input, output, session) {
    
    # Create reactiveValues ----
    x = reactiveValues(df = NULL)
    
    observe({
      x$df <- input_df
    })
    
    # Determine if scroll ----
    y_len <- reactive({
      
      if(nrow(input_df) >= 10) {
        y_len <- '250px'
      } else {
        y_len <- FALSE
      }
      
      return(y_len)
    })
    
    # Create table ----
    output$table <- DT::renderDT(
      x$df,
      colnames = col_names,
      selection = 'none',
      options = list(
        dom = 't',
        scrollY = y_len(),
        ordering = F,
        pageLength = nrow(input_df)
      ),
      editable = list(
        target = 'column',
        disable = list(columns = 0)
      )
    )
    
    outputOptions(output, 'table', suspendWhenHidden = FALSE)
    
    # Update table values -----
    proxy <- DT::dataTableProxy('table')
    
    observeEvent(input$table_cell_edit, {
      x$df <- editData(x$df, input$table_cell_edit, proxy)
    })

    observeEvent(input$table_cell_edit, {
      info = input$table_cell_edit

      i = info$row
      j = info$col
      k = info$value

      if(j == 'WEIGHT' & k > 0){
        x$df[i, j] <- isolate(DT::coerceValue(k, x$df[i, j]))
      } else if (k == 'MIN_SCORE' & k > 0 & k <= 100) {
        x$df[i, j] <- isolate(DT::coerceValue(k, x$df[i, j]))
      } else if (k == 'MIN_SCORE' & k == 0){
        x$df[i, j] <- isolate(DT::coerceValue(NA, x$df[i, j]))
      }

      replaceData(proxy, x$df, resetPaging = FALSE)
    })
    
    # Reset Table -----
    observeEvent(btn_cancel, {
      x$df <- input_df
    })
    
    # # Output reactive values ----
    # return( 
    #   reactive({ x$df })
    #   )
    
  })
}

# end Server Function

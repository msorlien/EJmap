################################### HEADER ###################################
#  TITLE: select_data.R
#  DESCRIPTION: Module to select location & parameters
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-01
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(shinyjs)

########################################################################.
###                       User Interface                            ####
########################################################################.

SELECT_UI <- function(id, edit_metrics=TRUE) {
  
  ns <- NS(id)
  
  tagList(
    
    # Enable javascript
    useShinyjs(),
    
    # Select data ----
    tabsetPanel(type = "tabs",
                # # Location ----
                # tabPanel("Select Location", 
                #          h2('Select Town')
                #          ),
                # Metrics ----
                tabPanel("Select Metrics",
                         selectPar_ui(ns("select_metrics"))
                         )
                ,
                # Advanced Options ----
                tabPanel("Advanced Options",
                         advancedSelect_ui(ns("advanced_options"))
                         )
    ),
    
    actionButton(ns("update_map"),
                 label = paste0('Calculate'))
  )
  
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

SELECT_SERVER <- function(id, shp_input, edit_metrics=TRUE) {
  moduleServer(id, function(input, output, session) {
    
    # Add module servers ----
    df_par <- selectPar_server("select_metrics")
    
    df_advanced <- advancedSelect_server("advanced_options", df_par)
    
    # Action Button ----
    # * Toggle button on/off ----
    shinyjs::disable("update_map")
    
    observe({
      if (nrow(df_par()>0)) {
        shinyjs::enable("update_map")
      } else {
        shinyjs::disable("update_map")
      }
    })
    
    # * Edit shapefile ----
    shp_output <- eventReactive(input$update_map, {

      if (edit_metrics == TRUE) {
        shp_output <- calculate_score(shp_input, df_advanced$percentile_type(),
                                   df_advanced$percentile_min(),
                                   df_advanced$df_metric(),
                                   df_advanced$df_cat())
      } else {
        shp_output <- shp_input
      }

      return(shp_output)
    })
    
    # Output reactive values ----
    return(
      list(
        
        percentile_min = reactive({ df_advanced$percentile_min() }),
        shp_ejzones = reactive({ shp_output() })
        
      )
    )
    
  })
}

# end Server Function

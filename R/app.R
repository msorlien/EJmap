################################### HEADER ###################################
#  TITLE: app.R
#  DESCRIPTION: R shiny app for mapping EJ areas
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-15
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(shiny)
library(tidyverse)

# For testing
library(shinya11y)

EJmap <- function(...){
  
  ########################################################################.
  #                         User Interface                               #
  ########################################################################.
  
  ui <- fluidPage(
    #theme = bslib::bs_theme(bootswatch = 'sandstone'),
    use_tota11y(),
    
    tags$header(
      class = 'col-sm-12 title-panel',
      tags$h1('NBEP Environmental Justice Mapper')
    ),
    
    sidebarLayout(
      # Select variables ----
      sidebarPanel(map_sidebar_ui('custom_sidebar')),
      mainPanel(map_ui('dynamic_map'))
    )
  )
  
  # Set language -----
  attr(ui, 'lang')='en'
  
  ########################################################################.
  #                         Server Function                              #
  ########################################################################.
  server <- function(input, output, session) {
    # Set variables
    fixed_metrics <- FALSE
    
    # Add module servers ----
    ej_score <- map_sidebar_server(
      'custom_sidebar',
      input_shp = shp_raw, 
      input_shp_simple = shp_raw_simple)
    
    map_server('dynamic_map', ej_score)
  }
  
  ########################################################################.
  #                             Run App                                  #
  ########################################################################.
  
  shinyApp(ui, server)
}

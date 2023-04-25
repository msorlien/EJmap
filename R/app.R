################################### HEADER ###################################
#  TITLE: app.R
#  DESCRIPTION: R shiny app for mapping EJ areas
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-04-21
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
      class = "col-sm-12 title-panel",
      tags$h1("NBEP Environmental Justice Mapper")
    ),
    sidebarLayout(
      
      # * Side panel ----
      sidebarPanel(
        SELECT_UI('select_blank')
        
      ),
      
      # * Main panel ----
      mainPanel(
        "hewwo world uwu",
      )
    )
  )
  
  # Set language -----
  attr(ui, "lang")="en"
  
  ########################################################################.
  #                         Server Function                              #
  ########################################################################.
  server <- function(input, output, session) {
    # Add module servers ----
    SELECT_SERVER('select_blank')
  }
  
  ########################################################################.
  #                             Run App                                  #
  ########################################################################.
  
  shinyApp(ui, server)
}

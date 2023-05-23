################################### HEADER ###################################
#  TITLE: app.R
#  DESCRIPTION: R shiny app for mapping EJ areas
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-18
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
    
    tabsetPanel(
      type = 'tabs',
      id = 'tabset',
      
      # Tab: About ----
      tabPanel(
        'About',
        value='about',
        'row scuttle parrel provost Sail ho shrouds spirits boom mizzenmast 
        yardarm. Pinnace holystone mizzenmast quarter crow\'s nest nipperkin 
        grog yardarm hempen halter furl. Swab barque interloper chantey 
        doubloon starboard grog black jack gangway rutters. Deadlights jack lad 
        schooner scallywag dance the hempen jig carouser broadside cable strike 
        colors. Bring a spring upon her cable holystone blow the man down 
        spanker Shiver me timbers to go on account lookout wherry doubloon 
        chase. Belay yo-ho-ho keelhaul squiffy black spot yardarm spyglass 
        sheet transom heave to.'
        ),
      
      # Tab: NBEP map ----
      tabPanel(
        'NBEP Map',
        value = 'nbep',
        sidebarLayout(
          sidebarPanel(
            map_sidebar_ui('nbep_sidebar')
            ),
          mainPanel(
            map_ui(
              'nbep_map', 
              input_shp = shp_nbep_simple, 
              percentiles = c('N_'))
            )
          )
        ),
      
      # Tab: Custom map -----
      tabPanel(
        'Custom Map',
        value = 'diy',
        sidebarLayout(
          sidebarPanel(
            map_sidebar_ui('custom_sidebar')
            ),
          mainPanel(
            map_ui(
              'custom_map', 
              input_shp = shp_default_simple)
            )
          )
        )
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
    # NBEP map
    nbep_score <- map_sidebar_server(
      'nbep_sidebar',
      input_shp = shp_nbep, 
      input_shp_simple = shp_nbep_simple,
      select_metrics = FALSE
      )
    map_server('nbep_map', nbep_score, 20)
    
    # Custom map
    custom_score <- map_sidebar_server(
      'custom_sidebar',
      input_shp = shp_raw, 
      input_shp_simple = shp_default_simple)
    map_server('custom_map', custom_score)
  }
  
  ########################################################################.
  #                             Run App                                  #
  ########################################################################.
  
  shinyApp(ui, server)
}

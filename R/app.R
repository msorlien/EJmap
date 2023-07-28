#  TITLE: app.R
#  DESCRIPTION: R shiny app for mapping EJ areas
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-07-26
#  GIT REPO: nbep/ejmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
# -----------------------------------------------------------------------------.

library(shiny)
library(dplyr)

EJmap <- function(...){
  
  # UI ------------------------------------------------------------------------
  
  ui <- bslib::page_navbar(
    shinya11y::use_tota11y(),
    
    theme = bslib::bs_theme(version = 5),
    
    title = h1('NBEP EJmap'),
    
    # Tab: About ----
    bslib::nav_panel(
      'About',
      value='about',
      'This app is under development and subject to frequent changes.'
    ),
    
    # Tab: NBEP map ----
    bslib::nav_panel(
      'NBEP Map',
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          # style = css(overflow = "visible"),
          map_sidebar_ui('nbep_sidebar')
          ),
        map_ui(
          'nbep_map',
          input_shp = shp_nbep_simple,
          percentiles = 'N_', 
          default_layer = 'EJAREA')
      )
    ),
    
    # Tab: Custom map ----
    bslib::nav_panel(
      'Custom Map',
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          map_sidebar_ui('custom_sidebar')
        ),
        map_ui(
          'custom_map',
          input_shp = shp_default_simple)
        )
      )
    )
  
  # Set language -----
  attr(ui, 'lang')='en'
  
  # Server --------------------------------------------------------------------
  
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
    map_server('nbep_map', nbep_score, default_layer = 'EJAREA')
    
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

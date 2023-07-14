################################### HEADER ###################################
#  TITLE: mod_nbep_tab.R
#  DESCRIPTION: Module for tab with NBEP static map/table + info
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-07-13
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

# UI --------------------------------------------------------------------------

nbep_tab_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::card_header('Title'),
      # Sidebar ----
      sidebar = bslib::sidebar(
        bslib::accordion(
          multiple = FALSE,
          # * Select location ----
          bslib::accordion_panel(
            title = h2('Select Location'),
            value = 'location',
            select_location_ui(ns('location'))
          ),
          # * Download ----
          bslib::accordion_panel(
            title = h2('Download Data'),
            value = 'download',
            'placeholder text'
          )
        )
      ),
      # Map ----
      bslib::nav_panel(
        'Map',
        'this is where the map goes',
        # map_ui(
        #   'nbep_map', 
        #   input_shp = shp_nbep_simple, 
        #   percentiles = 'N_'
        #   )
        ),
      # Table ----
      bslib::nav_panel(
        'Table',
        'this is where the table goes'
      )
    )
  )
  
}

# Server ----------------------------------------------------------------------

nbep_tab_server <- function(id, input_shp, input_shp_simple) {
  moduleServer(id, function(input, output, session) { 
    
    # Add modules ----
    df_loc <- select_location_server('location', input_shp_simple)
    # map_server('nbep_map', df_loc, 1)
    
  })
}

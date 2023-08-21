################################### HEADER ###################################
#  TITLE: mod_download.R
#  DESCRIPTION: Module to download data
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-06-20
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(shinyWidgets)

# UI --------------------------------------------------------------------------

download_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    shinyWidgets::pickerInput(
      ns('downloadSelect'),
      label = h3('Select Format'),
      choices = c('Shapefile', 'GeoJSON', 'Excel', 'CSV', 'TSV'),
      selected = 'Shapefile'
    ),
    'download button'
  )
  
}

# Server -----------------------------------------------------------------------

download_server <- function(id, input_shp) {
  moduleServer(id, function(input, output, session) {
    
    # Adjust shapefile ----
    df_shp <- input_shp %>%
      select(-Town_Code)
    
  })
}

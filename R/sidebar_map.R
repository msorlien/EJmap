################################### HEADER ###################################
#  TITLE: sidebar_map.R
#  DESCRIPTION: Module to select location & parameters
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-09
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(shinyjs)

########################################################################.
###                       User Interface                            ####
########################################################################.

map_sidebar_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    # Enable javascript ----
    useShinyjs(),
    
    # Select location ----
    h2('Select Location'),
    
    # Select data ----
    selectPar_ui(ns('select_metrics')),
    
    # Download ----
    h2('Download Data')
  )
  
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

map_sidebar_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Add module servers ----
    df_par <- selectPar_server("select_metrics")
    
    # * Edit shapefile ----
    shp_output <- eventReactive(df_par$button(), {
      
      shp_output <- calculate_score(
        input_shp = shp_raw_simple, 
        percentile_min = df_par$percentile_min(), 
        exceed_all_min_scores = df_par$exceed_all(), 
        df_metrics = df_par$df_metric(), 
        df_categories = df_par$df_cat()
        ) 
      
      shp_output <- shp_output %>%
        mutate(across(where(is.numeric), ~na_if(., -999999)))

      return(shp_output)
    })

    # Output reactive values ----
    return(
      list(

        shp_ejzones = reactive({ shp_output() }),
        btn_metrics = reactive({ df_par$button() })

      )
    )
    
  })
}

# end Server Function

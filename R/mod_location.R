################################### HEADER ###################################
#  TITLE: select_location.R
#  DESCRIPTION: Module to select location
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-16
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(shinyjs)

########################################################################.
###                       User Interface                            ####
########################################################################.

select_location_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    # Town or watershed ----
    radioGroupButtons(ns('location_type'),
                      label = NULL,
                      choices = c('Select Town' = 'town',
                                  'Select Watershed' = 'watershed'),
                      justified = FALSE),
    
    # Select towns ----
    # Only show if "towns" selected in town/watershed toggle
    conditionalPanel(
      condition = paste0('input["', ns('location_type'), '"] == "town"'),
      pickerInput(
        ns('select_town'),
        label = h3('Select Towns'),
        choices = town_list,
        selected = town_list,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE),
        multiple = TRUE)
    ),

    # Select watersheds ----
    # Only show if "watershed" selected in town/watershed toggle
    conditionalPanel(
      condition = paste0('input["', ns('location_type'), '"] == "watershed"'),

      pickerInput(
        ns('select_watershed'),
        label = h3('Select Watersheds'),
        choices = watershed_list,
        selected = watershed_list,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE),
        multiple = TRUE)
    )
  )
  

  
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

select_location_server <- function(id, input_shp) {
  moduleServer(id, function(input, output, session) {
    
    shp_town <- reactive({
      req(input$select_town)
      
      shp_town <- filter_location(
        input_shp = input_shp, 
        town_or_watershed = 'town', 
        selected_towns = input$select_town)
      
      return(shp_town)
    })
    
    shp_watershed <- reactive({
      req(input$select_watershed)

      filter_location(
        input_shp = input_shp,
        town_or_watershed = 'watershed',
        selected_watersheds = input$select_watershed)
    })

    shp_output <- reactive({
      if (input$location_type == 'town') {
        shp_town()
      } else {
        shp_watershed()
      }
    })
    
    # Output reactive values ----
    return(
      list(
        
        location_type = reactive({ input$location_type }),
        output_shp = reactive({ shp_output() })
        
      )
    )
    
  })
}

# end Server Function

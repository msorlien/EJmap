#  TITLE: mod_location.R
#  DESCRIPTION: Module to select location
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-07-14
#  GIT REPO: NBEP/EJmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64

library(shinyjs)

# UI --------------------------------------------------------------------------

select_location_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    # Select State ----
    shinyWidgets::pickerInput(
      ns('select_state'),
      label = h3('Select State'),
      choices = c('Connecticut' = 'CT', 
                  'Massachusetts' = 'MA', 
                  'Rhode Island' = 'RI'),
      selected = c('CT', 'MA', 'RI'),
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = 'count > 2',
        container = 'body'),  # Allows dropdown overflow
      multiple = TRUE
      ),
    
    # Select towns ----
    shinyWidgets::pickerInput(
      ns('select_town'),
      label = h3('Select Town'),
      choices = town_list,
      selected = town_list,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = 'count > 2',
        container = 'body'),  # Allows dropdown overflow
      multiple = TRUE
      ),

    # Select watersheds ----
    shinyWidgets::pickerInput(
      ns('select_watershed'),
      label = h3('Select Watershed'),
      choices = watershed_list,
      selected = watershed_list,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = 'count > 2',
        container = 'body'),  # Allows dropdown overflow
      multiple = TRUE
      )
  )
}

# Server ----------------------------------------------------------------------

select_location_server <- function(id, input_shp) {
  moduleServer(id, function(input, output, session) {
    
    # Filter towns by state ----
    town_list_filtered <- reactive({
      req(input$select_state)
      
      subset(town_list, sub('^.+,\\s+', '', town_list) %in% input$select_state)
    })
    
    observeEvent(input$select_state, {
      shinyWidgets::updatePickerInput(session = session,
                        inputId = 'select_town',
                        choices = town_list_filtered(),
                        selected = town_list_filtered()
                        )
    })
    
    
    # Update shapefile ----
    shp_output <- reactive({
      req(input$select_town)
      req(input$select_watershed)
      
      shp_output <- filter_location(
        input_shp = input_shp$shp,
        selected_towns = input$select_town,
        selected_watersheds = input$select_watershed)
    })
    
    # Output reactive values ----
    return(
      list(
        
        selected_towns = reactive({ input$select_town }),
        selected_watersheds = reactive ({ input$select_watershed }),
        output_shp = reactive({ shp_output() })
        
      )
    )
  })
}

# end Server Function

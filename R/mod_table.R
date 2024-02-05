#  TITLE: mod_table.R
#  DESCRIPTION: Module to display EJ data as a table
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2024-01-23
#  GIT REPO: NBEP/EJmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
# -----------------------------------------------------------------------------.

# UI --------------------------------------------------------------------------

table_ui <- function(id, input_shp) {
  
  ns <- NS(id)
  
  # UI ----
  tagList(
    # Metric/Category ----
    shinyWidgets::pickerInput(
      inputId = ns("select_col"),
      label = "Select Category",
      choices = c("Summary" = "EJ", list_category_codes(input_shp)),
      selected = "EJ",
      options = list(
        `live-search` = TRUE,
        container = "body"),  # Allows dropdown overflow
      multiple = FALSE
    ),
    
    # Table ----
    shinycssloaders::withSpinner(
      reactable::reactableOutput(ns("table")),
      type = 5
    )

  )
}

# Server ----------------------------------------------------------------------.

table_server <- function(id, ejvar) {
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    # Define variables ----
    input_shp <- reactive({ 
      # Drop extra columns, format data
      input_shp <- ejvar$output_shp() %>%
        sf::st_drop_geometry() %>%
        select(c(BlockGroup, Town_Code, ACSTOTPOP) | 
                 starts_with(c("N_", "P_"))) %>%
        rename(c("Block Group" = BlockGroup, Town = Town_Code,
                 "Population" = ACSTOTPOP))
      
      # Truncate indicator column names
      colnames(input_shp) <- gsub("N_", "", colnames(input_shp))
      colnames(input_shp) <- gsub("P_", "", colnames(input_shp))
      
      return(input_shp)
    })
    
    filter_shp <- reactive({
      req(input$select_col)
      
      col_list <- list_category_indicators(ejvar$output_shp(),
                                           input$select_col)
      
      filter_shp <- input_shp() %>%
        select(c("Block Group", Town, "Population", col_list))
      
      return(filter_shp)
    })
    
    cat_list <- reactive({
      metric_list <- colnames(input_shp())
      
      col_list <- metric_table %>%
        filter(METRIC_CODE %in% metric_list) %>%
        select(c(CATEGORY, CAT_CODE)) %>%
        unique()
      
      cat_list <- col_list$CAT_CODE  
      names(cat_list) <- col_list$CATEGORY 
      
      return(cat_list)
    })
    
    # Update list of parameters ----
    observeEvent(ejvar$btn_metrics(), {

      # Update layer list
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "select_col",
        choices = c("Summary" = "EJ", 
                    list_category_codes(ejvar$output_shp())),
        selected = "EJ")
    })
    
    # TO DO: use reactablefmtr to add title, subtitle
    # TO DO: Allow user to toggle population on/off?
    
    output$table <- reactable::renderReactable({
      reactable::reactable(
        filter_shp(),
        # height = 500,
        highlight = TRUE,
        defaultColDef = reactable::colDef(minWidth = 150),
        columns = list(
          "Block Group" = reactable::colDef(
            sticky = "left", rowHeader = TRUE, minWidth = 150),
          Town = reactable::colDef(
            sticky = "left", minWidth = 200, 
            style = list(borderRight = "1px solid #eee"),
            headerStyle = list(borderRight = "1px solid #eee")),
          "Population" = reactable::colDef( 
            style = list(borderRight = "1px solid #eee"),
            headerStyle = list(borderRight = "1px solid #eee"))
          )
      )
    })
    
  })
}

# end Server Function

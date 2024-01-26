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
  
  # Define vars
  metric_list <- stringr::str_sub(colnames(input_shp), 3, -1)
  
  cat_list <- metric_table %>%
    filter(METRIC_CODE %in% metric_list) %>%
    select(c(CATEGORY, CAT_CODE)) %>%
    unique()
  
  col_list <- cat_list$CAT_CODE  
  names(col_list) <- cat_list$CATEGORY 
  
  # UI ----
  tagList(
    # Metric/Category ----
    shinyWidgets::pickerInput(
      inputId = ns("select_col"),
      label = "Select Category",
      choices = c("Summary" = "EJ", col_list),
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

table_server <- function(id, ejvar, selected_tab, this_tab) {
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    # Define variables ----
    input_shp <- reactive({ 
      input_shp <- ejvar$output_shp() %>%
        sf::st_drop_geometry() %>%
        select(c(BlockGroup, Town_Code, ACSTOTPOP) | 
                 starts_with(c("N_", "P_"))) %>%
        rename(c("Block Group" = BlockGroup, Town = Town_Code,
                 "Population" = ACSTOTPOP))
      })
    
    filter_shp <- reactive({
      # List selected columns
      col_list <- list_cat_columns(input_shp(), input$select_col)
      
      # Truncate indicator column names
      filter_shp <- input_shp()
      colnames(filter_shp) <- gsub("N_", "", colnames(filter_shp))
      colnames(filter_shp) <- gsub("P_", "", colnames(filter_shp))
      
      # Filter for selected columns
      filter_shp <- filter_shp %>%
        select(c("Block Group", Town, "Population", col_list))
      
      return(filter_shp)
    })
    
    cat_list <- reactive({
      metric_list <- stringr::str_sub(colnames(input_shp()), 3, -1)
      
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
      
      # Define variables
      
      
      # Update layer list
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "select_col",
        choices = c("Summary" = "EJ", cat_list()),
        selected = "EJ")
    })
    
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
            headerStyle = list(borderRight = "1px solid #eee"))
          )
      )
    })
    
  })
}

# end Server Function

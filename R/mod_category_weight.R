################################### HEADER ###################################
#  TITLE: mod_category_weight.R
#  DESCRIPTION: Module to assign weight, minimum value to each metric category
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-04-25
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(shinyWidgets)

########################################################################.
###                       User Interface                            ####
########################################################################.

CAT_WEIGHT_UI <- function(id, cat_name) {
  
  ns <- NS(id)
  
  tagList(
    # Title ----
    h3(cat_name),
    # Weight ----
    numericInput(
      inputId=ns("cat_weight"),
      label="Weight",
      value=1,
      min = 0
    ),
    # Min Value ----
    numericInput(
      inputId=ns("cat_min"),
      label="Minimum Score",
      value=0,
      min = 0,
      max = 1,
      step = .1
    )
  )
  
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

CAT_WEIGHT_SERVER <- function(id, cat_code) {
  moduleServer(id, function(input, output, session) {
    
    # Create dataframe ----
    df_cat <- reactive({
      df_cat <- data.frame(CAT_CODE = cat_code,
                           WEIGHT = input$cat_weight,
                           MIN_VALUE = input$cat_min)
      return(df_cat)
    })
    
    # Output reactive values ----
    return(
      list(
        catWeight = reactive({ df_cat() })
      )
    )
    
  })
}

# end Server Function

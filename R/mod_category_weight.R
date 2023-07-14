################################### HEADER ###################################
#  TITLE: mod_category_weight.R
#  DESCRIPTION: Module to assign weight, minimum value to categories
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-06-14
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(shinyWidgets)
library(htmltools)
library(purrr)

########################################################################.
###                       User Interface                            ####
########################################################################.

test_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    verbatimTextOutput(ns('test')),
    
    # map(ns('cat_list'), ~ weight_var_ui(.x)),
    
    uiOutput(ns('weight'))
    
    # numericInput(
    #   inputId=ns('socvul'),
    #   label = 'Social Vulnerability',
    #   value=1,
    #   min = 0,
    #   step = 0.5
    # ),
    # numericInput(
    #   inputId=ns('health'),
    #   label = 'Health',
    #   value=1,
    #   min = 0,
    #   step = 0.5
    # ),
    # numericInput(
    #   inputId=ns('envbur'),
    #   label = 'Environmental Burden',
    #   value=1,
    #   min = 0,
    #   step = 0.5
    # ),
    # numericInput(
    #   inputId=ns('climate'),
    #   label = 'Climate',
    #   value=1,
    #   min = 0,
    #   step = 0.5
    # )
  )
  
}

########################################################################.
###                         MODULE SERVER                           ####
########################################################################.

test_server <- function(id, df_cat) {
  moduleServer(id, function(input, output, session) {
    
    # Set namespace ----
    ns <- session$ns
    
    # Category list ----
    cat_list <- reactive({ df_cat()$CATEGORY })
    
    # UI ----
    output$weight <- renderUI({
      map(cat_list(), ~ weight_var_ui(.x, isolate(input[[.x]])))
    })

    # test shit
    output$test <- renderPrint({
      map_chr(cat_list(), ~ input[[.x]] %||% '')
    })
    
  })
}

# end Server Function

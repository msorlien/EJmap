#  TITLE: app.R
#  DESCRIPTION: R shiny app for mapping EJ areas
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-08-29
#  GIT REPO: NBEP/EJmap
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
    id = 'main_tabs',
    
    # Tab: About ----
    bslib::nav_panel(
      'About',
      value='about',
      h2('About EJMap'),
      HTML('Environmental justice is the just and equitable distribution of 
           environmental benefits and burdens for all communities, regardless of 
           race, culture, ethnicity, or income. <p>Mapping tools such as EPA’s 
           <a href="https://www.epa.gov/ejscreen">EJScreen</a> 
           serve to identify communities that face disproportionate negative 
           environmental impacts, or are particularly vulnerable to those 
           impacts. EJMap is designed to supplement EJScreen with data 
           from additional sources and to allow local communities to build and 
           design custom maps that meet their needs and concerns.</p>'),
      h3("NBEP's Environmental Justice Map"),
      HTML(
        paste0(
          '<p>The NBEP Map tab shows the Narragansett Bay Estuary Program\'s
          designated environmental justice areas. The <a 
          href="www/METADATA_EJAREAS_', ejmap_year, '_NBEP', ejmap_year, 
          '.html">associated metadata</a> summarizes which indicators were 
          selected and how they were weighted.</p>'
          )
        ),
      h3('Build a Custom Map'),
      HTML('<p>The Custom Map tab lets you build your own environmental
        justice map. Simply select a location, up to', nrow(metric_table), 
        'indicators, and at the push of button EJMap will calculate an 
        environmental justice score for each block group. The map can be 
        further customized under Advanced Options - set how each variable is 
        weighted, raise or lower the minimum percentile, and more. 
        </p><p>Maps can be downloaded as a spreadsheet or shapefile. All 
        downloads include metadata.</p>'
        ),
      h2('About NBEP'),
      HTML('<p>Founded in 1985, the <a href="https://www.nbep.org/">Narragansett 
           Bay Estuary Program</a> (NBEP) is a stakeholder-led organization 
           pursuing place-based conservation across the Narragansett Bay 
           region.</p> <p>NBEP is part of the <a href="https://www.epa.gov/nep">
           National Estuary Program</a>, a network of 28 "estuaries of national 
           significance" across the United States. NBEP is hosted by <a 
           href="https://www.rwu.edu/">Roger Williams University</a>.'),
      h2('Links and Technical Documentation'),
      tags$a(
        href='www/EJMap_TechnicalDocumentation.pdf',
        'Technical Documentation'),
      tags$a(
        href='https://narragansett-bay-estuary-program-nbep.hub.arcgis.com/search?q=ejmap',
        'Archived Data'),
      tags$a(
        href='https://github.com/NBEP/EJmap',
        'EJMap Code'),
      h2('FAQ'),
      'Coming soon',
      h2('Upcoming Features'),
      HTML('This is the beta version of EJMap. Planned updates include:
           <ul><li>Improved accessibility</li>
           <li>Improved pop-up text</li>
           <li>View data as a table</li>
           <li>Better symbology for null data</li>
           </ul>')
    ),
    
    # Tab: NBEP map ----
    bslib::nav_panel(
      'NBEP Map',
      value = 'map_nbep',
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
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
      value = 'map_custom',
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
    
    # Allow links to items in www folder
    shiny::addResourcePath('www', here::here("www"))
    
    # Set variables
    fixed_metrics <- FALSE
    selected_tab <- reactive({ input$main_tabs })
    
    # Add module servers ----
    # NBEP map
    nbep_score <- map_sidebar_server(
      'nbep_sidebar',
      input_shp = shp_nbep, 
      input_shp_simple = shp_nbep_simple,
      select_metrics = FALSE,
      percentile_type = 'N_'
      )
    map_server('nbep_map', nbep_score, selected_tab, 'map_nbep',
               default_layer = 'EJAREA')
    
    # Custom map
    custom_score <- map_sidebar_server(
      'custom_sidebar',
      input_shp = shp_raw, 
      input_shp_simple = shp_default_simple)
    map_server('custom_map', custom_score, selected_tab, 'map_custom')
    
  }
  
  # Run app -------------------------------------------------------------------.
  shinyApp(ui, server)
}

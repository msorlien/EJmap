#  TITLE: app.R
#  DESCRIPTION: R shiny app for mapping EJ areas
# -----------------------------------------------------------------------------.

library(dplyr)

EJmap <- function(...){
  
  # UI ------------------------------------------------------------------------
  
  ui <- bslib::page_navbar(
    
    theme = bslib::bs_theme(version = 5),
    
    title = h1("NBEP EJmap"),
    id = "main_tabs",
    
    # Tab: About ----
    bslib::nav_panel(
      "About",
      value="about",
      class = "bslib-page-dashboard",
      bslib::card(
        min_height = 250,
        h2("About EJMap"),
        HTML("Environmental justice is the just and equitable distribution of 
             environmental benefits and burdens for all communities, regardless of 
             race, culture, ethnicity, or income. <p>Mapping tools such as EPA's 
             <a href=\"https://www.epa.gov/ejscreen\">EJScreen</a> 
             serve to identify communities that face disproportionate negative 
             environmental impacts, or are particularly vulnerable to those 
             impacts. EJMap is designed to supplement EJScreen with data 
             from additional sources and to allow local communities to build and 
             design custom maps that meet their needs and concerns.</p>"),
        h3("NBEP's Environmental Justice Map"),
        HTML("<p><i>NBEP Map</i> displays the Narragansett Bay Estuary
          Program's official environmental justice map. This map forms the 
          basis of <a 
          href=\"https://www.nbep.org/s/2023-NBEP-Equity-Strategy-Final.pdf\">
          NBEP's 2023 Equity Strategy</a>, which describes how NBEP will sustain 
          and increase investments in underserved communities and the benefits 
          that flow to them using NBEP's Bipartisan Infrastructure Law (BIL) 
          funding from fiscal year 2024 to 2026. NBEP has set a baseline target 
          of investing 50% of 2024-2026 BIL funds in projects that benefit 
          communities identified as Environmental Justice areas in EJMap.</p>"),
        h3("Build a Custom Map"),
        HTML("<p><i>Custom Map</i> lets you build your own environmental
          justice map. Simply select a location, up to", nrow(metric_table), 
          "indicators, and at the push of button EJMap will calculate an 
          environmental justice score for each block group. The map can be 
          further customized under Advanced Options - set how each variable is 
          weighted, raise or lower the minimum percentile, and more. 
          </p><p>Maps can be downloaded as a spreadsheet or shapefile. All 
          downloads include metadata.</p>"
          ),
        h2("About NBEP"),
        HTML('<p>Founded in 1985, the <a href="https://www.nbep.org/">Narragansett 
             Bay Estuary Program</a> (NBEP) is a stakeholder-led organization 
             pursuing place-based conservation across the Narragansett Bay 
             region.</p> <p>NBEP is part of the <a href="https://www.epa.gov/nep">
             National Estuary Program</a>, a network of 28 "estuaries of national 
             significance" across the United States. NBEP is hosted by <a 
             href="https://www.rwu.edu/">Roger Williams University</a>.'),
        h2("Links and Technical Documentation"),
        tags$a(
          href="www/EJMap_TechnicalDocumentation.pdf",
          "Technical Documentation"),
        tags$a(
          href="https://narragansett-bay-estuary-program-nbep.hub.arcgis.com/search?q=ejmap",
          "Archived Data"),
        tags$a(
          href="https://github.com/NBEP/EJmap",
          "EJMap Code"),
        h2("FAQ"),
        "Coming soon")
    ),
    
    # Tab: NBEP map ----
    bslib::nav_panel(
      "NBEP Map",
      value = "nbep_tab",
      class = "bslib-page-dashboard",
      bslib::layout_sidebar(
        # * Sidebar ----
        sidebar = bslib::sidebar(
          width = 300,
          map_sidebar_ui("nbep_sidebar")
        ),
        # * Card ----
        bslib::card(
          min_height = 250,
          full_screen = FALSE,
          bslib::accordion(
            multiple = TRUE,
            open = TRUE,
            bslib::accordion_panel(
              # * About ----
              title = h2("About"),
              value = "p_about_nbep",
              HTML(
              '<p>NBEP defines underserved communities as census block groups 
              that:</p>
              <p>1. Meet or exceed the 80th percentile for one or more of the
              following social vulnerability indicators:</p>
              <ul><li>People of Color</li>
              <li>Low Income</li>
              <li>Less than High School Education</li>
              <li>Limited English Speaking</li></ul>
              <p><b>OR</b></p>
              <p>2. Have an environmental burden score of 80 or higher. The 
              following indicators are used to calculate environmental 
              burden:<p>
              <ul><li>Ozone (weight = 0.5)</li>
              <li>Particulate Matter 2.5 (weight = 0.5)</li>
              <li>Traffic Proximity (weight = 1)</li>
              <li>Hazardous Waste Facilities (weight = 1)</li>
              <li>Water Pollution (weight = 1)</li>
              <li>Impervious Surfaces (weight = 0.5)</li>
              <li>Lack of Trees (weight = 0.5)</li></ul>
              <p><a href="www/METADATA_EJAREAS_', ejmap_year, "_NBEP", 
              ejmap_year, '.html">View Metadata</a><p>')),
            # * Map ----
            bslib::accordion_panel(
              title = h2("Map"),
              value = "p_map_nbep",
              map_ui("nbep_map",
                input_shp = shp_nbep_simple,
                default_layer = "EJAREA")
            ),
            # * Table ----
            bslib::accordion_panel(
              title = h2("Table"),
              value = "p_table_nbep",
              table_ui("nbep_table",
                       input_shp = shp_nbep_simple))
            )
          )
        )
      ),
    
    # Tab: Custom map ----
    bslib::nav_panel(
      "Custom Map",
      value = "custom_tab",
      class = "bslib-page-dashboard",
      bslib::layout_sidebar(
        # * Sidebar ----
        sidebar = bslib::sidebar(
          width = 300,
          map_sidebar_ui("custom_sidebar")
          ),
        # * Card ----
        bslib::card(
          min_height = 250,
          full_screen = FALSE,
          bslib::accordion(
            multiple = TRUE,
            open = TRUE,
            bslib::accordion_panel(
              title = h2("About"),
              value = "p_about_custom",
              HTML(
                "<p>Build your own environmental justice map!</p>
                <p>Use the sidebar to select indicators that are relevant to 
                your local needs and create a custom map. You can use Advanced
                Options to change how each variable is weighted, raise or lower 
                the minimum percentile, and more.</p>
                <p>Maps can be downloaded as a spreadsheet or shapefile. All
                downloads include a metadata summary of selected options.</p>")
              ),
            bslib::accordion_panel(
              title = h2("Map"),
              value = "p_map_custom",
              map_ui("custom_map",
                input_shp = shp_default_simple)
              ),
            bslib::accordion_panel(
              title = h2("Table"),
              value = "p_table_custom",
              table_ui("custom_table",
                input_shp = shp_default_simple)
              )
            )
          )
        )
      )#,
    
    # bslib::nav_spacer(),
    # # Toggle: light mode/dark mode
    # bslib::nav_item(
    #   bslib::input_dark_mode(id = "dark_mode", mode = NULL)
    #   ) 
    )
  
  # Set language -----
  attr(ui, "lang")="en"
  
  # Server --------------------------------------------------------------------
  
  server <- function(input, output, session) {
    
    # Allow links to items in www folder
    shiny::addResourcePath("www", here::here("www"))
    
    # Set variables
    selected_tab <- reactive({ input$main_tabs })
    
    # Add module servers ----
    # * NBEP sidebar, map, table ----
    nbep_score <- map_sidebar_server("nbep_sidebar",
      input_shp = shp_nbep, 
      input_shp_simple = shp_nbep_simple,
      select_metrics = FALSE,
      percentile_type = "N_")
    map_server("nbep_map",
      ejvar = nbep_score,
      active_tab = selected_tab,
      map_tab = "nbep_tab",
      default_layer = "EJAREA")
    table_server("nbep_table", 
      ejvar = nbep_score)
    
    # * Custom sidebar, map, table ----
    custom_score <- map_sidebar_server("custom_sidebar",
      input_shp = shp_raw, 
      input_shp_simple = shp_default_simple)
    map_server("custom_map", 
      ejvar = custom_score, 
      active_tab = selected_tab, 
      map_tab = "custom_tab")
    table_server("custom_table", 
      ejvar = custom_score)
  }
  
  # Run app -------------------------------------------------------------------.
  shinyApp(ui, server)
}

#  TITLE: mod_download.R
#  DESCRIPTION: Module to download data
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-08-25
#  GIT REPO: NBEP/EJmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
# -----------------------------------------------------------------------------.

# UI --------------------------------------------------------------------------

download_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    shinyWidgets::pickerInput(
      ns('downloadSelect'),
      label = h3('Format'),
      choices = c('Shapefile', 'GeoJSON', 'Excel', 'CSV', 'TSV'),
      selected = 'Shapefile'
    ),
    downloadButton(ns('download'), 'Download')
  )
  
}

# Server -----------------------------------------------------------------------

download_server <- function(id, input_shp, select_metrics, percentile_type,
                            metrics, location) {
  moduleServer(id, function(input, output, session) {
    
    # Set variables ----
    if (select_metrics == TRUE) {
      title = paste0('EJMAP_NBEP', ejmap_year, '_', 
                     format(Sys.Date(), '%Y%m%d'))
    } else {
      title = paste0('EJAREAS_', ejmap_year, '_NBEP', ejmap_year)
    }

    # Format, zip, download data ----
    output$download <- downloadHandler(
      filename = function() { paste0(title, '.zip') },
      
      content = function(file) {
        
        # Set temp directory ----
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        
        # Filter location -----
        df_shp <- filter_location(input_shp, location$selected_towns(), 
                                  location$selected_watersheds()) %>%
          select(-Town_Code)
        
        # Calculate score, add metadata ----
        if(select_metrics == TRUE){
          # Calculate score
          df_shp <- calculate_score(
            input_shp = df_shp, 
            percentile_min = metrics$percentile_min(), 
            prefix_list = percentile_type, 
            min_pass = metrics$min_pass(), 
            min_ej = metrics$min_ej(), 
            df_metrics = metrics$df_metric(), 
            df_categories = metrics$df_cat())
          
          # Add metadata columns
          df_shp <- add_metadata(
            input_shp = df_shp, 
            df_metrics = metrics$df_metric())
          
          # Generate HTML, XML metadata files, save to temp directory
          add_metadata_files(
            input_rmd = normalizePath(system.file('rmd', 'metadata.Rmd', 
                                                  package = 'EJmap')), 
            input_xml = NULL, 
            output_path = temp_directory, 
            title = title, 
            epa_funding = epagrants, 
            df_metrics = metrics$df_metric(), 
            df_cats = metrics$df_cat(), 
            min_percentile = metrics$percentile_min(), 
            percentile_type = percentile_type, 
            min_score = metrics$min_ej(), 
            min_pass = metrics$min_pass()
          )
        } else {
          # Copy HTML, XML metadata to temp directory ----
          html_src = system.file('www', paste0('METADATA_', 
                                          title, '.html'),
                            package = 'EJmap')
          html_temp = paste0(temp_directory, '/METADATA_', title, '.html')
          file.copy(html_src, html_temp, overwrite = TRUE)
          
          # xml_src = system.file('www', paste0(title, '.xml'),
          #                        package = 'EJmap')
          # xml_temp = paste0(temp_directory, '/', title, '.xml')
          # file.copy(xml_src, xml_temp, overwrite = TRUE)
        }
        
        # Format data for download ----
        df_shp_nogeom <- sf::st_drop_geometry(df_shp)
        
        # Create file
        if (input$downloadSelect == 'Shapefile'){
          shp_temp <- paste0(temp_directory, '/', title, '.shp')
          sf::st_write(df_shp, shp_temp, append=FALSE)
        } else if (input$downloadSelect == 'GeoJSON'){
          geojson_temp <- paste0(temp_directory, '/', title, '.geojson')
          sf::st_write(df_shp, geojson_temp, append=FALSE)
        } else if (input$downloadSelect == 'Excel'){
          temp_xls <- paste0(temp_directory, '/', title, '.xlsx')
          writexl::write_xlsx(df_shp_nogeom, path = temp_xls)
        } else if (input$downloadSelect == 'CSV'){
          temp_csv <- paste0(temp_directory, '/', title, '.csv')
          write.csv(df_shp_nogeom, temp_csv, row.names=FALSE)
        } else if (input$downloadSelect == 'TSV'){
          temp_tsv <- paste0(temp_directory, '/', title, '.tsv')
          write.table(df_shp_nogeom, temp_tsv, sep='\t', row.names = FALSE)
        }
        
        # Create zip ----
        zip::zip(
          zipfile = file,
          files = dir(temp_directory),
          root = temp_directory
        )
      }
    )
    
  })
}

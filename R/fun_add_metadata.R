#  TITLE: fun_add_metadata.R
#  DESCRIPTION: Generates metadata for shapefile
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2025-02-05
#  GIT REPO: nbep/ejmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
# -----------------------------------------------------------------------------.

# function: add_metadata ------------------------------------------------------
# Adds DATA_SOURCE, SOURCE_YEAR columns to shapefile

# Variables
# input_shp: input shapefile
# df_metrics: dataframe of metric columns, names, and weights

add_metadata <- function(input_shp, df_metrics){
  
  # Define variables ----
  metric_list <- df_metrics$METRIC_CODE
  
  # Filter metric list, isolate DATA_SOURCE, SOURCE_YEAR
  df_metrics <- metric_table %>%
    filter(METRIC_CODE %in% metric_list) %>%
    select(DATA_SOURCE, SOURCE_YEAR) %>%
    arrange(SOURCE_YEAR)
  
  # Drop duplicates
  df_metrics <- unique(df_metrics) %>%
    group_by(DATA_SOURCE) %>%
    summarize(SOURCE_YEAR = toString(SOURCE_YEAR))
  
  # Add NBEP as a data source (for block groups)
  df_nbep <- source_table %>%
    filter(DATA_SOURCE == 'NBEP')
  
  df_metrics <- df_metrics %>%
    add_row(DATA_SOURCE = 'NBEP', 
            SOURCE_YEAR = toString(df_nbep$SOURCE_YEAR))
  
  DATA_SOURCE <- paste(df_metrics$DATA_SOURCE, collapse = '; ')
  SOURCE_YEAR <- paste(df_metrics$SOURCE_YEAR, collapse= '; ')
  
  output_shp <- input_shp %>%
    select(-c(DataSource, SourceYear)) %>%
    tibble::add_column(DataSource = DATA_SOURCE) %>%
    tibble::add_column(SourceYear = SOURCE_YEAR) %>%
    relocate(c(NBEPYear, Study_Area, geometry), .after = last_col())
  
  return(output_shp)
}

# function: add_metadata_files --------------------------------------------------
# Generates HTML, XML metadata summaries

add_metadata_files <- function( 
    input_rmd, input_xml = NULL, output_path, title, epa_funding, df_metrics, 
    df_cats, min_percentile = 80, percentile_type = c('N_', 'P_'), 
    min_score = 0, min_pass = 4
    ){
  
  # Remove row names -----
  rownames(df_metrics) <- NULL
  rownames(df_cats) <- NULL
  
  # Define variables -----
  # Sources
  df_sources <- df_metrics %>%
    select(DATA_SOURCE) %>%
    add_row(DATA_SOURCE = 'NBEP') %>%
    unique()
  
  # Source name substitutions
  df_source_names <- left_join(x=df_sources, 
                               y=source_table, 
                               by='DATA_SOURCE') %>%
    mutate(DATA_SOURCE_LONG = ifelse(is.na(DATA_SOURCE_LONG), 
                                     DATA_SOURCE, 
                                     DATA_SOURCE_LONG))
  
  # Fields
  list_fields <- c('OBJECTID', 'BlockGroup', 'Town', 'State', 'HUC10', 
                   'HUC10_Name', 'ALAND', 'AWATER', 'ACSTOTPOP', 'DataSource',
                   'SourceYear', 'NBEPYear', 'Study_Area', 'Shape_Length',
                   'Shape_Area', df_metrics$METRIC_CODE)
  for(x in percentile_type){
    list_fields <- c(list_fields, paste0(x, df_metrics$METRIC_CODE),
                     paste0(x, df_cats$CAT_CODE), paste0(x, 'SCORE'),
                     paste0(x, 'EJAREA'))
  }
  
  # Generate HTML file from Rmd -----
  # Copy input to temp directory
  src <- input_rmd
  tempReport <- file.path(tempdir(), 'metadata.Rmd')
  file.copy(src, tempReport, overwrite = TRUE)
  
  # Set output path, name
  pdf_output <- paste0(output_path, '/METADATA_', title)
  
  # Set up parameters to pass to Rmd document
  params <- list(
    title = title,
    epa_funding = epa_funding,
    df_metrics = df_metrics,
    df_cats = df_cats,
    df_sources = source_table,
    min_percentile = min_percentile,
    percentile_type = percentile_type,
    min_score = min_score,
    min_pass = min_pass
  )
  
  # Generate PDF
  rmarkdown::render(
    input = tempReport, 
    output_file = pdf_output,
    params = params,
    envir = new.env(parent = globalenv())
  )
  
  # Generate XML file ----
  # Read in XML file
  input_xml <- xml2::read_xml(
    normalizePath(paste0(getwd(), '/inst/extdata/metadata.xml')))
  
  # Modify XML
  # * Title ----
  node_title <- xml2::xml_find_all(input_xml, '//idCitation/resTitle')
  node_enttypl <- xml2::xml_find_all(input_xml, '//enttypl')
  node_detailed <- xml2::xml_find_all(input_xml, '//detailed')
  xml2::xml_text(node_title) <- title
  xml2::xml_text(node_enttypl) <- title
  xml2::xml_attr(node_detailed, 'Name') <- title
  
  # * Temp title ----
  node_altTitle <- xml2::xml_find_all(input_xml, '//idCitation/resAltTitle')
  text_altTitle <- paste0('Environmental Justice Priority Areas, ', ejmap_year,
                         ', NBEP ', ejmap_year) 
  xml2::xml_text(node_altTitle) <- text_altTitle
  
  # * Publication date ----
  node_pubDate <- xml2::xml_find_all(input_xml, '//idCitation/date/pubDate')
  text_pubDate <- format(Sys.time(), '%Y-%m-%dT%H:%M:%S')
  xml2::xml_text(node_pubDate) <- text_pubDate
  
  # * Citation ----
  node_citation <- xml2::xml_find_all(input_xml, '//otherCitDet')
  text_citation <- paste0(
    'Suggested bibliographic reference: NBEP, ', ejmap_year, '. ', 
    text_altTitle, '; ', title, 
    '. Narragansett Bay Estuary Program (NBEP), URL: http://www.nbep.org, Providence, Rhode Island (publication date: ', 
    Sys.Date(), ')'
    )
  xml2::xml_text(node_citation) <- text_citation
  
  # * Credit ----
  node_credit <- xml2::xml_find_all(input_xml, '//idCredit')
  text_credit <- paste(df_source_names$DATA_SOURCE_LONG, collapse = '; ')
  xml2::xml_text(node_credit) <- text_credit
  
  # * Constraints ----
  node_useLimit <- xml2::xml_find_all(input_xml, '//useLimit')
  text_useLimit <- 'This dataset is provided "as is". The producer(s) of this dataset, contributors to this dataset, and the Narragansett Bay Estuary Program (NBEP) do not make any warranties of any kind for this dataset, and are not liable for any loss or damage however and whenever caused by any use of this dataset.'
  if('First Street' %in% df_metrics$DATA_SOURCE){
    text_useLimit <- paste(text_useLimit,
      'This data is provided under the Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) license.'
      )
  } else {
    text_useLimit <- paste(text_useLimit,
      'There are no restrictions or legal prerequisites for using the data.'
      )
  }
  text_useLimit <- paste(text_useLimit, 
    'Once acquired, any modification made to the data must be noted in the metadata. Please acknowledge both NBEP and the primary producer(s) of this dataset or any derived products. These data are intended for use as a tool for reference, display, and general GIS analysis purposes only. It is the responsibility of the data user to use the data appropriately and consistent with the limitations of geospatial data in general and these data in particular. The information contained in these data may be dynamic and could change over time. The data accuracy is checked against best available sources which may be dated. The data are not better than the original sources from which they are derived. These data are not designed for use as a primary regulatory tool in permitting or siting decisions and are not a legally authoritative source for the location of natural or manmade features. The depicted boundaries, interpretations, and analysis derived from have not been verified at the site level them and do not eliminate the need for onsite sampling, testing, and detailed study of specific sites. This project was funded by agreements by the Environmental Protection Agency (EPA) to Roger Williams University (RWU) in partnership with the Narragansett Bay Estuary Program. Although the information in this document has been funded wholly or in part by EPA under the agreements', 
    epagrants,
    'to RWU, it has not undergone the Agency’s publications review process and therefore, may not necessarily reflect the views of the Agency and no official endorsement should be inferred. The viewpoints expressed here do not necessarily represent those of the Narragansett Bay Estuary Program, RWU, or EPA nor does mention of trade names, commercial products, or causes constitute endorsement or recommendation for use.'
    )
  xml2::xml_text(node_useLimit) <- text_useLimit
  
  # * Entity definition ----
  node_enttypd <- xml2::xml_find_all(input_xml, '//enttypd')
  text_enttypd <- 'Environmental justice areas in the Narragansett Bay region.'
  xml2::xml_text(node_enttypd) <- text_enttypd
  
  # * Data Source ----
  # List extra sources
  list_source_nodes <- xml2::xml_text(
    xml2::xml_find_all(
      input_xml, '//dataSource/srcCitatn/citRespParty/rpOrgName'
      )
    )
  extra_source_nodes <- list_source_nodes[
    which(
      !list_source_nodes %in% df_source_names$DATA_SOURCE_XML
      )
    ]
  
  # Drop extra sources
  for(source in extra_source_nodes) {
    # Find node
    node_rpOrgName <- xml2::xml_find_all(
      input_xml, 
      paste0('//dataSource/srcCitatn/citRespParty/rpOrgName[text()="', 
             source,'"]'
             )
      )
    # Find parent node
    node_dataSource <- xml2::xml_parent(
      xml2::xml_parent(
        xml2::xml_parent(node_rpOrgName)
      )
    )
    # Drop parent node
    xml2::xml_remove(node_dataSource)
  }
  
  # * Fields ----
  # List extra fields
  list_field_nodes <- xml2::xml_text(
    xml2::xml_find_all(
      input_xml, '//attr/attrlabl'
      )
    )
  extra_field_nodes <- list_field_nodes[which(
    !list_field_nodes %in% list_fields)]
  
  # Drop extra fields
  for(field in extra_field_nodes) {
    # Find node
    node_attrlabl <- xml2::xml_find_all(
      input_xml, 
      paste0('//attr/attrlabl[text()="', field,'"]')
    )
    # Find parent node
    node_attr <- xml2::xml_parent(node_attrlabl)
    # Drop parent node
    xml2::xml_remove(node_attr)
  }
  
  # * Process step description ----
  # Select node
  step_number <- length(xml2::xml_find_all(input_xml, '//prcStep'))
  node_stepDesc <- xml2::xml_find_all(input_xml, '//stepDesc')[step_number]
  
  # Summarize category score calculations
  df_cat_formula <- df_metrics %>%
    # Add column for WEIGHT(METRIC)
    mutate(math = paste0(WEIGHT, '(', METRIC, ')')) %>%
    # For each cat, add columns WEIGHT(METRIC) + WEIGHT(METRIC) & sum WEIGHT
    group_by(CATEGORY) %>%
    summarise(sum_weight = sum(WEIGHT), 
              cat_string = paste(math, collapse = ' + ')) %>%
    # Create string equation for each category
    mutate(cat_string = paste0(CATEGORY, ' = [', cat_string, ']/', sum_weight))
  text_cat_formula <- paste0(
    '\n\n', paste(df_cat_formula$cat_string, collapse='\n\n'))
  
  # Summarize minimum category scores
  df_cats_summary <- df_cats %>%
    filter(MIN_SCORE > 0) %>%
    mutate(cat_string = paste(MIN_SCORE, 'for', tolower(CATEGORY)))
  if (nrow(df_cats_summary) > 0){
    text_cats_summary <- paste0(
      '\n\nThe following minimum category scores were applied: ', 
      paste(df_cats_summary$cat_string, collapse=', '), 
      '. Block groups that failed to pass at least ', min_pass, 
      ' minimum category score')
    if(min_pass > 1) {
      text_cats_summary <- paste0(text_cats_summary, 's')
    } 
    text_cats_summary <- paste(
      text_cats_summary,
      'were assigned an environmental justice score of 0. For all other block groups, the environmental justice score was calculated as:')
  } else {
    text_cats_summary <- '\n\nNo minimum category scores were assigned. For each block group, an environmental justice score was calculated as:'
  }
  
  # Summarize ej calculations
  df_ej_formula <- df_cats %>%
    mutate(ej_string = paste0(WEIGHT, '(', CATEGORY, ')')) 
  
  text_ej_formula <- paste0(
    '\n\nEJ Score = [', paste(df_ej_formula$ej_string, collapse=' + '),
    '] / ', sum(df_ej_formula$WEIGHT))
  
  # Summarize ej areas
  text_ejarea <- '\n\nBlock groups with an environmental justice score'
  if (min_score > 0) {
    text_ejarea <- paste(text_ej_area, 'at or above', min_score)
  } else {
    text_ejarea <- paste(text_ejarea, 'above 0')
  }
  text_ejarea <- paste(
    text_ejarea, 
    'are considered to be environmental justice areas and have EJAREA marked "Yes".')
  
  # Update text
  text_stepDesc <- paste0(
    'EJmap (https://github.com/nbep/ejmap) was used to produce an environmental justice map. Extraneous data was dropped and the following calculations were run: 
    \nIndicators that meet or exceed the ', min_percentile, 'th percentile are assigned a hidden score of 100; those that fail to do so are assigned a hidden score of 0. These hidden scores are used to calculate category scores as follows:',
    text_cat_formula, text_cats_summary, text_ej_formula, text_ejarea)
  xml2::xml_text(node_stepDesc) <- text_stepDesc
  
  # * Process date ----
  node_stepDate <- xml2::xml_find_all(input_xml, '//stepDateTm')[step_number]
  text_stepDate <- format(Sys.time(), '%Y-%m-%dT%H:%M:%S')
  xml2::xml_text(node_stepDate) <- text_stepDate
  
  # Save XML ----
  xml2::write_xml(input_xml, paste0(output_path, '/', title, '.xml'))
}

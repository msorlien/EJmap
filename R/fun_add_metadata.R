#  TITLE: fun_add_metadata.R
#  DESCRIPTION: Generates metadata for shapefile
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-08-25
#  GIT REPO: nbep/ejmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
# -----------------------------------------------------------------------------.

library(dplyr)
library(tibble)
library(glue)

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
}
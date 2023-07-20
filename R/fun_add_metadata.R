################################### HEADER ###################################
#  TITLE: fun_add_metadata.R
#  DESCRIPTION: Adds metadata to shapefile
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-23
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(dplyr)
library(tibble)
library(glue)

# VARIABLES
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

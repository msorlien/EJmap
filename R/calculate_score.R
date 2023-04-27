################################### HEADER ###################################
#  TITLE: calculate_score.R
#  DESCRIPTION: Calculates scores
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-04-27
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(dplyr)

calculate_score <- function(percentile_type, percentile_min, df_metrics, 
                            df_categories){
  
  
  # Define variables ----
  keep_columns <- c("Block_Group", "Town", "State", "ALAND", "AWATER", 
                    "ACSTOTPOP", "Study_Area", "DataSource", "SourceYear", 
                    "NBEPYear")
  
  if (percentile_type == "State") {
    col_prefix <- "P_"
  } else {
    col_prefix <- "N_"
  }
  
  var_list <- df_metrics$METRIC_CODE
  cat_list <- df_categories$CAT_CODE
  percentile_list <- paste0(col_prefix, var_list)
  
  col_list <- c(keep_columns, var_list, percentile_list)

  # Drop extra columns from shapefile ----
  df_ej <- shp_ejmetrics %>%
    select(col_list)
  
  # Calculate category scores ----
  
  # Calculate total score ----
  
}


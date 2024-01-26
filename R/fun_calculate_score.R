#  TITLE: fun_calculate_score.R
#  DESCRIPTION: Calculates scores
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2024-01-25
#  GIT REPO: nbep/ejmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
# -----------------------------------------------------------------------------.

library(dplyr)
library(tidyr)
library(glue)

# VARIABLES
# input_shp: input shapefile
# percentile_min: minimum percentile
# prefix_list: list of column prefixes (P_ -> state, N_ -> study area)
# min_pass: number of minimum category scores EJ areas must meet or exceed
# min_ej: minimum overall score to be considered an EJ area (0-100)
# df_metrics: dataframe of metric columns, names, and weights
# df_categories: dataframe of category columns, names, weights, and min scores

calculate_score <- function(
    input_shp, percentile_min = 80, prefix_list=c('P_', 'N_'), min_pass = 4, 
    min_ej = 0, df_metrics, df_categories){
  
  # Define variables ----
  metric_list <- df_metrics$METRIC_CODE
  cat_list <- df_categories$CAT_CODE
  df_ej <- input_shp
  
  cat_length <- df_categories %>%
    filter(MIN_SCORE > 0)
  
  if(prefix_list == "N_") {
    df_ej <- input_shp %>%
      filter(!Study_Area == "Outside Study Area")
  }
  
  if (min_pass > nrow(cat_length)) {
    min_pass <- nrow(cat_length)
  }
  
  if (min_ej < 0) {
    min_ej <- 0
  } else if (min_ej > 100) {
    min_ej <- 100
  }
  
  # Calculate scores for state, NBEP ----
  for (col_prefix in prefix_list){
    
    # Define variables ----
    final_score <- paste0(col_prefix, 'SCORE')
    ejarea <- paste0(col_prefix, 'EJAREA')
    
    # Add columns for each category ----
    for (cat in cat_list) {
      cat_col <- paste0(col_prefix, cat)
      cat_temp <- paste0('temp_', cat)
      cat_max <- paste0('temp_', cat, '_max')
      
      df_ej[[cat_col]] <- 0
      df_ej[[cat_temp]] <- 0 
      df_ej[[cat_max]] <- 0 
      
    }
    
    # Calculate raw category scores ----
    for (metric in metric_list) {
      
      # * Define variables ----
      metric_info <- df_metrics %>%
        filter(METRIC_CODE == metric)
      metric_weight <- metric_info$WEIGHT[1]
      metric_score <- paste0(col_prefix, metric)
      
      cat <- metric_info$CAT_CODE[1]
      cat_col <- paste0(col_prefix, cat)
      cat_temp <- paste0('temp_', cat)
      cat_max <- paste0('temp_', cat, '_max')
      
      # * Calculate raw score ----
      df_ej[[cat_temp]] <- if_else(
        df_ej[[metric_score]] >= percentile_min & !is.na(df_ej[[metric_score]]),
        df_ej[[cat_temp]] + metric_weight,
        df_ej[[cat_temp]])
      # * Calculate max score ----
      # Do not add weight if metric percentile is NA!
      df_ej[[cat_max]] <- if_else(
        !is.na(df_ej[[metric_score]]),
        df_ej[[cat_max]] + metric_weight,
        df_ej[[cat_max]])
    }
  
    # Add columns ----
    df_ej[[final_score]] <- 0
    df_ej[[ejarea]] <- 'No Data'
    df_ej[['temp_score']] <- 0
    df_ej[['temp_score_max']] <- 0
    df_ej[['temp_pass']] <- 0
    
    # Calculate final scores ----
    for (cat in cat_list) {

      # * Define variables ----
      cat_score <- paste0(col_prefix, cat)
      cat_temp <- paste0('temp_', cat)
      cat_max <- paste0('temp_', cat, '_max')

      cat_info <- df_categories %>%
        filter(CAT_CODE == cat)
      cat_weight = cat_info$WEIGHT[1]
      cat_min = cat_info$MIN_SCORE[1]

      # * Calculate category score ----
      df_ej[[cat_score]] <- if_else(
        df_ej[[cat_max]] > 0,
        df_ej[[cat_temp]] / df_ej[[cat_max]] * 100,
        -999999)

      # * Calculate raw overall score ----
      df_ej[['temp_score']] <- if_else(
        df_ej[[cat_score]] >= 0,
        df_ej[['temp_score']] + cat_weight * df_ej[[cat_score]],
        df_ej[['temp_score']])

      # * Calculate max overall score ----
      # Do not add weight if category score is NA! (eg -999999)
      df_ej[['temp_score_max']] <- if_else(
        df_ej[[cat_score]] >= 0,
        df_ej[['temp_score_max']] + cat_weight,
        df_ej[['temp_score_max']]
        )

      # * Check if above min cat score ----
      df_ej[['temp_pass']] <- if_else(
        cat_min > 0 & 
          df_ej[[cat_score]] >= cat_min,  # Filter out NA values
        df_ej[['temp_pass']] + 1,
        df_ej[['temp_pass']]
        )
    }

    # * Calculate overall score ----
    df_ej[[final_score]] <- case_when(
      df_ej[['temp_score_max']] == 0 ~ -999999,
      df_ej[['temp_pass']] < min_pass ~ 0,
      TRUE ~ df_ej[['temp_score']] / df_ej[['temp_score_max']]
    )
    
    # * Calculate if EJ area ----
    df_ej[[ejarea]] <- case_when(
      df_ej[[final_score]] < 0 ~ 'No Data',
      df_ej[[final_score]] > 0 & df_ej[[final_score]] >= min_ej ~ 'Yes',
      TRUE ~ 'No'
    )
  }

  # List skipped metrics ----
  drop_columns <- metric_table %>%
    filter(!METRIC_CODE %in% metric_list)
  
  # Tidy data ----
  extra_col <- c("temp_", "N_", "P_")
  extra_col <- extra_col[!extra_col %in% prefix_list]
  
  df_ej <- df_ej %>%
    # Drop temp columns, skipped metrics
    select(-starts_with(extra_col)) %>%
    select(-contains(drop_columns$METRIC_CODE)) %>%
    # Replace NA values with -999999
    mutate(across(where(is.numeric), ~tidyr::replace_na(.x, -999999))) %>%
    # Round category scores to 2 decimals
    mutate(across(
      expand.grid(prefix_list, c(cat_list, "SCORE")) %>%
        purrr::pmap_chr(paste0),
      ~round(.x, 2)
      ))

  return(df_ej)
}

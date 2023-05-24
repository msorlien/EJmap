################################### HEADER ###################################
#  TITLE: fun_calculate_score.R
#  DESCRIPTION: Calculates scores
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-15
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(dplyr)
library(glue)

# VARIABLES
# input_shp: input shapefile
# percentile_min: minimum percentile
# prefix_list: list of column prefixes (P_ -> state, N_ -> study area)
# exceed_all_min_scores: TRUE, FALSE
#  Determines whether final score needs to meet ALL category min scores,
#  or only ONE OR MORE category min scores
# df_metrics: dataframe of metric columns, names, and weights
# df_categories: dataframe of category columns, names, weights, and min scores

calculate_score <- function(
    input_shp, percentile_min, prefix_list=c('P_', 'N_'),
    exceed_all_min_scores=TRUE, df_metrics, df_categories){
  
  # Define variables ----
  metric_list <- df_metrics$METRIC_CODE
  cat_list <- df_categories$CAT_CODE
  df_ej <- input_shp
  
  # Calculate scores for state, NBEP ----
  for (col_prefix in prefix_list){
    
    # Define variables ----
    final_score <- paste0(col_prefix, 'SCORE')
    
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
    df_ej[['temp_score']] <- 0
    df_ej[['temp_score_max']] <- 0
    df_ej[['temp_fail']] <- 0
    df_ej[['temp_fail_max']] <- 0
    
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
        df_ej[[cat_score]] >= cat_min,
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
      df_ej[['temp_fail']] <- if_else(
        cat_min > 0 & 
          df_ej[[cat_score]] < cat_min &
          df_ej[[cat_score]] >= 0,  # Filter out NA values
        df_ej[['temp_fail']] + 1,
        df_ej[['temp_fail']]
        )

      # * Check max # of fail cat score ----
      # Check for NA! 
      df_ej[['temp_fail_max']] <- if_else(
        cat_min > 0 & df_ej[[cat_score]] >= 0,
        df_ej[['temp_fail_max']] + 1,
        df_ej[['temp_fail_max']]
      )
    }

    # * Calculate overall score ----
    df_ej[[final_score]] <- case_when(
      df_ej[['temp_score_max']] == 0 ~ -999999,
      exceed_all_min_scores == TRUE & df_ej[['temp_fail']] > 0 ~ 0,
      df_ej[['temp_fail_max']] > 0 & 
        df_ej[['temp_fail']] >= df_ej[['temp_fail_max']] ~ 0,
      TRUE ~ df_ej[['temp_score']] / df_ej[['temp_score_max']]
    )
  }

  # List skipped metrics ----
  drop_columns <- metric_table %>%
    filter(!METRIC_CODE %in% metric_list)
  
  # Tidy data ----
  df_ej <- df_ej %>%
    # Drop temp columns, skipped metrics
    select(-starts_with('temp_')) %>%
    select(-contains(drop_columns$METRIC_CODE)) %>%
    # Replace NA values with -999999
    mutate(across(where(is.numeric), ~replace_na(.x, -999999)))

  return(df_ej)
}

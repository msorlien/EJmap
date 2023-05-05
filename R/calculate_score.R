################################### HEADER ###################################
#  TITLE: calculate_score.R
#  DESCRIPTION: Calculates scores
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-02
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(dplyr)
library(glue)

calculate_score <- function(shp_blockgroups, percentile_type, percentile_min, 
                            df_metrics, df_categories){
  
  # Define variables ----
  keep_columns <- c("Block_Group", "Town", "State", "ALAND", "AWATER", 
                    "ACSTOTPOP", "Study_Area", "DataSource", "SourceYear", 
                    "NBEPYear")
  
  if (percentile_type == "State") {
    col_prefix <- "P_"
  } else {
    col_prefix <- "N_"
    shp_blockgroups <- shp_blockgroups %>%
      filter(Study_Area != "Outside Study Area")
  }

  metric_list <- df_metrics$METRIC_CODE
  cat_list <- df_categories$CAT_CODE
  percentile_list <- paste0(col_prefix, metric_list)

  col_list <- c(keep_columns, metric_list, percentile_list)

  # Adjust columns ----
  df_ej <- shp_blockgroups %>%
    # Drop extra columns
    select(col_list) %>%
    # Add new columns
    add_column(S_SCORE = 0, temp_score = 0, temp_score_max = 0, 
               temp_status = "keep")

  # Add columns for each category ----
  for (cat in cat_list) {
    cat_col <- paste0("S_", cat)
    cat_temp <- paste0("temp_", cat)
    cat_max <- paste0("temp_", cat, "_max")

    df_ej <- df_ej %>%
      add_column("{cat_col}":=0, "{cat_temp}":=0, "{cat_max}":=0)
  }

  # Calculate raw category scores ----
  for (metric in metric_list) {
  
    # * Define variables ----
    metric_info <- df_metrics %>%
      filter(METRIC_CODE == metric)
    metric_weight <- metric_info$WEIGHT[1]
    metric_score <- paste0(col_prefix, metric)
  
    cat <- metric_info$CAT_CODE[1]
    cat_temp <- paste0("temp_", cat)
    cat_max <- paste0("temp_", cat, "_max")
    
    # * Calculate raw score ----
    df_ej[[cat_temp]] <- if_else(df_ej[[metric_score]] >= percentile_min &
                                   !is.na(df_ej[[metric_score]]),
                                 df_ej[[cat_temp]] + metric_weight,
                                 df_ej[[cat_temp]])
    # * Calculate max score ----
    # Do not add weight if metric percentile is NA!
    df_ej[[cat_max]] <- if_else(!is.na(df_ej[[metric_score]]),
                                df_ej[[cat_max]] + metric_weight,
                                df_ej[[cat_max]])
  }
  
  # Calculate final scores ----
  for (cat in cat_list) {
    
    # * Define variables ----
    cat_score <- paste0("S_", cat)
    cat_temp <- paste0("temp_", cat)
    cat_max <- paste0("temp_", cat, "_max")
    
    cat_info <- df_categories %>%
      filter(CAT_CODE == cat)
    cat_weight = cat_info$WEIGHT[1]
    cat_min = cat_info$MIN_SCORE[1]
    
    # * Calculate category score ----
    df_ej[[cat_score]] <- if_else(df_ej[[cat_max]] > 0,
                                df_ej[[cat_temp]]/df_ej[[cat_max]],
                                -999999)
    
    # * Calculate raw overall score ----
    df_ej[["temp_score"]] <- if_else(
      df_ej[[cat_score]] >= cat_min,
      df_ej[["temp_score"]] + cat_weight*df_ej[[cat_score]],
      df_ej[["temp_score"]])
    
    # * Calculate max overall score ----
    # Do not add weight if category score is NA! (eg -999999)
    df_ej[["temp_score_max"]] <- if_else(
      df_ej[[cat_score]] >= 0,
      df_ej[["temp_score_max"]] + cat_weight,
      df_ej[["temp_score_max"]])
    
    # * Mark status as "drop" if below min cat score
    df_ej[["temp_status"]] <- if_else(
      cat_min > 0 & df_ej[[cat_score]] < cat_min,
      "drop",
      df_ej[["temp_status"]]
    )
  }

  # * Calculate overall score ----
  df_ej <- df_ej %>%
    # Replace -999999 with NA
    mutate(across(where(is.numeric), ~na_if(., -999999))) %>%
    # Calculate score; mark as 0 if status marked as "drop"
    mutate(S_SCORE = case_when(
      temp_score_max == 0 ~ -999999,
      temp_status == "drop" ~ 0,
      TRUE ~ temp_score/temp_score_max
      ))
    
  # Tidy data ----
  df_ej <- df_ej %>%
    # Drop temp columns
    select(-starts_with('temp_')) %>%
    # Set NA values to -999999
    mutate(across(where(is.numeric), ~replace_na(.x, -999999)))
    
  return(df_ej)
}

#  TITLE: fun_col_codes.R
#  DESCRIPTION: List percentile codes, assign names
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-24
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
# -----------------------------------------------------------------------------

list_percentile_codes <- function(input_percentiles){
  
  # Set var ----
  percentile_codes <- c('N_', 'P_')
  percentile_names <- c('Compare to Region',
                        'Compare to State')
  
  # Create table, filter for input ----
  df_percentiles <- data.frame(percentile_codes, percentile_names) %>%
    filter(percentile_codes %in% input_percentiles)
  
  # Create list ----
  percentile_list <- df_percentiles$percentile_codes
  names(percentile_list) <- df_percentiles$percentile_names
  
  return(percentile_list)
}

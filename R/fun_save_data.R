#  TITLE: fun_save_data.R
#  DESCRIPTION: Update dataframe with new data
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2025-02-05
#  GIT REPO: nbep/ejmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
# -----------------------------------------------------------------------------.

# VARIABLES
# df_old: dataframe with saved data
# df_new: dataframe with new values
# col_name: name of column with new data ('WEIGHT', 'MIN_SCORE')
# cat_or_metric: type of table being updated ('CATEGORY', 'METRIC')

update_table <- function(df_old, df_new, col_name, cat_or_metric){
  
  # Update df_new (rownames as a column, remove <br>)
 if (cat_or_metric == 'CATEGORY') {
    df_new <- df_new %>%
      tibble::rownames_to_column('CATEGORY') %>%
      mutate(CATEGORY = gsub('<br>', ' ', CATEGORY))
  } else if (cat_or_metric == 'METRIC') {
    df_new <- df_new %>%
      tibble::rownames_to_column('METRIC') %>%
      mutate(METRIC = gsub('<br>', ' ', METRIC))
  }
  
  # Rename columns, reset NA values
  if (col_name == 'WEIGHT'){
    df_new <- rename(df_new, WEIGHT = Weight) %>%
      mutate(WEIGHT = ifelse(is.na(WEIGHT), 1, WEIGHT))
  } else if (col_name == 'MIN_SCORE'){
    df_new <- df_new %>% 
      rename(MIN_SCORE = 'Minimum<br>Score') %>%
      mutate(MIN_SCORE = ifelse(is.na(MIN_SCORE), 0, MIN_SCORE))
  }
  
  # Join tables
  df_join <- left_join(df_old, df_new, by=cat_or_metric)
  
  if (col_name == 'WEIGHT'){
    df_join <- df_join %>%
      mutate(WEIGHT = ifelse(is.na(WEIGHT.y), WEIGHT.x, WEIGHT.y)) %>%
      select(!c(WEIGHT.x, WEIGHT.y))
  } else if (col_name == 'MIN_SCORE'){
    df_join <- df_join %>%
      mutate(MIN_SCORE = ifelse(is.na(MIN_SCORE.y), 
                                MIN_SCORE.x, MIN_SCORE.y)) %>%
      select(!c(MIN_SCORE.x, MIN_SCORE.y))
  }

  # Add row names
  if (cat_or_metric == 'CATEGORY') {
    row.names(df_join) = wrap_text(df_join$CATEGORY, 15, '<br>')
  } else if (cat_or_metric == 'METRIC') {
    row.names(df_join) = wrap_text(df_join$METRIC, 15, '<br>')
  }
  
  return(df_join)
}

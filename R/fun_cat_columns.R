#  TITLE: fun_cat_columns.R
#  DESCRIPTION: List column codes & names for all metrics in a selected category
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2024-01-23
#  GIT REPO: NBEP/EJmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
# -----------------------------------------------------------------------------

# function: list_column_codes
# Generates lists of map layers, neatly grouped for pickerInput dropdown

list_cat_columns <- function(input_shp, category){
  
  # List col names in input_shp, minus first two letters
  # (drops N_, P_)
  input_col <- stringr::str_sub(colnames(input_shp), 3, -1)
  
  # Filter column table for col in input_shp
  df_columns <- column_table %>%
    filter(COL_CODE %in% input_col) %>%
    mutate(COL_NAME = ifelse(startsWith(COL_NAME, "-"), 
                             stringr::str_sub(COL_NAME, 3, -1), 
                             COL_NAME))
  
  if (category != "EJ"){
    df_columns <- df_columns %>%
      filter(CAT_CODE == category)
  } else {
    df_columns <- df_columns %>%
      filter(COL_CODE %in% unique(column_table$CAT_CODE) | CAT_CODE == category)
  }
  
  cat_cols <- df_columns$COL_CODE
  names(cat_cols) <- df_columns$COL_NAME
  
  return(cat_cols)
}
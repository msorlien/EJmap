#  TITLE: fun_col_codes.R
#  DESCRIPTION: List metric & category column codes, assign column_table names
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-18
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
# -----------------------------------------------------------------------------

list_col_codes <- function(input_shp){
  
  # List col names in input_shp, minus first two letters
  # (drops N_, P_)
  input_col <- str_sub(colnames(input_shp), 3, -1)
  
  # Filter column table for col in input_shp
  df_columns <- column_table %>%
    filter(COL_CODE %in% input_col)
  
  # Make named list
  col_codes <- df_columns$COL_CODE
  names(col_codes) <- df_columns$COL_NAME
  
  return(col_codes)
}

#  TITLE: fun_col_codes.R
#  DESCRIPTION: List metric & category column codes, assign names
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2024-01-26
#  GIT REPO: NBEP/EJmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
# -----------------------------------------------------------------------------

# function: list_column_codes
# Generates lists of map layers, neatly grouped for pickerInput dropdown

list_column_codes <- function(input_shp){
  
  # List col names in input_shp, minus "N_", "P_", or "_"
  input_col <- input_shp %>%
    sf::st_drop_geometry() %>%
    select(starts_with(c("N_", "P_", "_")))
  colnames(input_col) <- gsub("N_", "_", colnames(input_col))
  colnames(input_col) <- gsub("P_", "_", colnames(input_col))
  input_col <- stringr::str_sub(colnames(input_col), 2, -1)
  
  # Filter column table for col in input_shp
  df_columns <- column_table %>%
    filter(COL_CODE %in% input_col)
  
  # Create a named list for each category
  temp_df <- filter(df_columns, CAT_CODE == 'SOCVUL')
  socvul <- temp_df$COL_CODE
  names(socvul) <- temp_df$COL_NAME
  
  temp_df <- filter(df_columns, CAT_CODE == 'HEALTH')
  health <- temp_df$COL_CODE
  names(health) <- temp_df$COL_NAME
  
  temp_df <- filter(df_columns, CAT_CODE == 'ENVBUR')
  envbur <- temp_df$COL_CODE
  names(envbur) <- temp_df$COL_NAME
  
  temp_df <- filter(df_columns, CAT_CODE == 'CLIMATE')
  climate <- temp_df$COL_CODE
  names(climate) <- temp_df$COL_NAME
  
  temp_df <- filter(df_columns, CAT_CODE == 'EJ')
  ej <- temp_df$COL_CODE
  names(ej) <- temp_df$COL_NAME
  
  # Create list of lists
  col_codes <- list(
    'Social Vulnerability' = socvul,
    'Health' = health,
    'Environmental Burden' = envbur,
    'Climate Risk' = climate,
    'Overall Score' = ej
  )
  
  # Remove empty lists
  col_codes <- purrr::map(col_codes, ~ purrr::compact(.)) %>% 
    purrr::keep(~length(.) != 0)
  
  return(col_codes)
  
}
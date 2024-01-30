#' List indicators
#' 
#' `list_indicator_codes()` lists codes for all indicators in shapefile. Helper
#' function for [list_category_codes()], [list_indicators_grouped()], 
#' [list_indicators_ungrouped()]
#'
#' @param input_shp A shapefile.
#' @returns A list.
#' @seealso [list_category_codes()], [list_indicators_grouped()], 
#'   [list_indicators_ungrouped()]
list_indicator_codes <- function(input_shp) {
  col_list <- input_shp %>%
    sf::st_drop_geometry() %>%
    select(starts_with(c("N_", "P_", "_")))
  colnames(col_list) <- gsub("N_", "_", colnames(col_list))
  colnames(col_list) <- gsub("P_", "_", colnames(col_list))
  col_list <- stringr::str_sub(colnames(col_list), 2, -1)
  col_list <- unique(col_list)
  
  return(col_list)
}

#' List categories
#' 
#' `list_category_codes()` lists codes for all categories in shapefile. Formats 
#' data for `shinyWidgets::pickerInput()` in `table_ui`, `table_server`.
#'
#' @param input_shp A shapefile.
#' @returns A list.
#' @seealso [list_indicator_codes()], [list_indicators_grouped()], 
#'   [list_indicators_ungrouped()]
list_category_codes <- function(input_shp) {
  metric_list <- list_indicator_codes(input_shp)
  
  cat_list <- cat_table %>%
    filter(CAT_CODE %in% metric_list)
  
  col_list <- cat_list$CAT_CODE  
  names(col_list) <- cat_list$CATEGORY
  
  return(col_list)
}

#' Group indicator list by category
#' 
#' `list_indicators_map()` creates list of indicator codes and names, 
#' grouped by category. Formats data for `shinyWidgets::pickerInput()` in 
#' `map_ui`, `map_server`
#'
#' @param input_shp A shapefile.
#' @returns A grouped list with named elements.
#' @seealso [list_category_codes()], [list_indicator_codes()], 
#' [list_indicators_ungrouped()]
list_indicators_grouped <- function(input_shp){
  
  # Filter column_table for selected indicators
  input_col <- list_indicator_codes(input_shp)
  df_columns <- column_table %>% filter(COL_CODE %in% input_col)
  
  # Create a named list for each category
  temp_df <- filter(df_columns, CAT_CODE == "SOCVUL")
  socvul <- temp_df$COL_CODE
  names(socvul) <- temp_df$COL_NAME
  
  temp_df <- filter(df_columns, CAT_CODE == "HEALTH")
  health <- temp_df$COL_CODE
  names(health) <- temp_df$COL_NAME
  
  temp_df <- filter(df_columns, CAT_CODE == "ENVBUR")
  envbur <- temp_df$COL_CODE
  names(envbur) <- temp_df$COL_NAME
  
  temp_df <- filter(df_columns, CAT_CODE == "CLIMATE")
  climate <- temp_df$COL_CODE
  names(climate) <- temp_df$COL_NAME
  
  temp_df <- filter(df_columns, CAT_CODE == "EJ")
  ej <- temp_df$COL_CODE
  names(ej) <- temp_df$COL_NAME
  
  # Create list of lists
  col_codes <- list(
    "Social Vulnerability" = socvul,
    "Health" = health,
    "Environmental Burden" = envbur,
    "Climate Risk" = climate,
    "Overall Score" = ej
  )
  
  # Remove empty lists
  col_codes <- purrr::map(col_codes, ~ purrr::compact(.)) %>% 
    purrr::keep(~length(.) != 0)
  
  return(col_codes)
}

#' List indicators in category
#' 
#' `list_indicators_ungrouped()` creates list of indicator codes and names 
#' within a single category. If category is set to "EJ", lists all category and
#' EJ columns instead. Formats data for table in `table_ui`, `table_server`.
#'
#' @param input_shp A shapefile.
#' @param category A string. Acceptable values are "SOCVUL", "HEALTH", 
#'   "ENVBUR", "CLIMATE", and "EJ".
#' @returns A list with named elements.
#' @seealso [list_category_codes()], [list_indicator_codes()], 
#' [list_indicators_grouped()]
list_indicators_ungrouped <- function(input_shp, category){
  
  # Filter column table for col in input_shp
  input_col <- list_indicator_codes(input_shp)
  df_columns <- column_table %>%
    filter(COL_CODE %in% input_col) %>%
    mutate(COL_NAME = case_when(
      COL_NAME == "EJ Areas" ~ "EJ Area",
      startsWith(COL_NAME, "-") ~ stringr::str_sub(COL_NAME, 3, -1),
      TRUE ~ COL_NAME))
  
  if (category == "EJ"){
    df_columns <- df_columns %>%
      filter(COL_CODE %in% cat_table$CAT_CODE | CAT_CODE == "EJ")
  } else {
    df_columns <- df_columns %>%
      filter(CAT_CODE == category)
  }
  
  cat_cols <- df_columns$COL_CODE
  names(cat_cols) <- df_columns$COL_NAME
  
  return(cat_cols)
}
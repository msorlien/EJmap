#' Leaflet popup text
#' 
#' `add_popup_text()` adds columns with pre-formatted popup text for leaflet. 
#'
#' @param input_shp A shapefile.
#' @param prefix A character. Acceptable values are "N_", "P_", "_".
#' @returns A shapefile.
add_popup_text <- function(input_shp, prefix) {
  
  # List categories
  cat_list <- list_category_codes(input_shp)
  cat_list <- c(cat_list, "EJ")
  
  for (cat in cat_list) {
    # Define variables
    col_list <- list_category_indicators(input_shp, cat)
    new_col <- paste0("T_", cat)
    
    # Add new column for each category
    input_shp <- input_shp %>%
      mutate("{new_col}" := paste0("<b>Block Group:</b> ", BlockGroup,
                                   "<br/><b>Town:</b> ", Town, ", ", State,
                                   "<br/><b>Watersheds:</b> ", HUC10_Name, 
                                   "<br/>"))
    
    # Concatenate indicator names, values in new column
    for (indicator in col_list) {  
      col_code <- paste0(prefix, indicator)
      col_name <- column_table %>%
        filter(COL_CODE == indicator) %>%
        mutate(COL_NAME = case_when(
          COL_NAME == "EJ Areas" ~ "EJ Area",
          TRUE ~ COL_NAME))
      col_name <- col_name$COL_NAME
      
      input_shp <- input_shp %>%
        mutate("{new_col}" := paste0(.data[[new_col]], "</br><b>", col_name, 
                                     ":</b> ", .data[[col_code]]))
    }
  }
  
  return(input_shp)
}
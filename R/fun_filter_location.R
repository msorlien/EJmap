################################### HEADER ###################################
#  TITLE: filter_location.R
#  DESCRIPTION: Filters data for selected towns/watersheds
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-24
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
##############################################################################.

library(dplyr)
library(glue)

# VARIABLES
# input_shp: input shapefile
# town_or_watershed: town, watershed
#  filter for selected towns, or for selected watersheds
# town_list: list of towns (Town, St)
# watershed_list: list of watersheds

filter_location <- function(input_shp, selected_towns, selected_watersheds) {
  
  df_ej <- input_shp %>%
    # Filter for selected towns
    filter(Town_Code %in% selected_towns) %>%
    # Generate extra rows for multi-watershed entries
    mutate(temp_watersheds = stringr::str_split(HUC10_Name, '; ')) %>%
    tidyr::unnest(temp_watersheds) %>%
    # Filter for selected watersheds
    filter(temp_watersheds %in% selected_watersheds) %>%
    # Drop temp column
    select(-temp_watersheds) %>%
    # Drop duplicate rows
    distinct()
  
  return(df_ej)
}

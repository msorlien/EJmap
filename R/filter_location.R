################################### HEADER ###################################
#  TITLE: filter_location.R
#  DESCRIPTION: Filters data for selected towns/watersheds
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-15
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

filter_location <- function(input_shp, town_or_watershed, selected_towns, 
                            selected_watersheds){
  
  if(town_or_watershed == 'town') {
    df_ej <- input_shp %>%
      filter(Town_Code %in% selected_towns)
  } else {
    df_ej <- input_shp %>%
      filter(
        length(intersect(str_split(HUC10_Name), selected_watersheds)) > 0
      )
  }
  
  return(df_ej)
}

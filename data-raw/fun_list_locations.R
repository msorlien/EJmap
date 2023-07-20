################################### HEADER ###################################
#  TITLE: list_town_watershed.R
#  DESCRIPTION: Generates list of towns, watersheds
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-15
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt) x86_64
##############################################################################.

library(tidyverse)

# Import shapefile
input_shp <- read_sf(dsn = 'data-raw',
                     layer = 'EJMETRICS_2023_NBEP2023') %>%
  # Add new column (Town_Code)
  add_column(Town_Code = 'ABC', .after='State') %>%
  mutate(
    Town_Code = case_when(
      State == 'Rhode Island' ~ paste0(Town, ', RI'),
      State == 'Massachusetts' ~ paste0(Town, ', MA'),
      State == 'Connecticut' ~ paste0(Town, ', CT'),
      TRUE ~ paste0(Town, ', ', State)
      )
  )

# List towns
town_list <- sort(unique(input_shp$Town_Code))

usethis::use_data(town_list, overwrite=TRUE)

# List watersheds
watershed_list <- sort(unique(unlist(str_split(input_shp$HUC10_Name, '; '))))

usethis::use_data(watershed_list, overwrite=TRUE)
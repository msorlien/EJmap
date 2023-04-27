################################### HEADER ###################################
#  TITLE: shp_ejmetrics.R
#  DESCRIPTION: Adds EJ metric shapefile 
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-04-21
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt) x86_64
##############################################################################.

library(sf)
library(tidyverse)

shp_ejmetrics <- read_sf(dsn = "data-raw", 
                         layer = "EJMETRICS_2022_NBEP2023") %>%
  # Rename column
  rename(Block_Group=GEOID) %>%
  # Add new column (Town_Code)
  add_column(Town_Code = "ABC", .after="State") %>%
  mutate(Town_Code = paste0(Town, ", ", State)) %>%
  # Replace -999999 with NA
  mutate(across(where(is.numeric), ~na_if(., -999999)))

usethis::use_data(shp_ejmetrics, overwrite=TRUE)

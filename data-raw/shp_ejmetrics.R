################################### HEADER ###################################
#  TITLE: shp_ejmetrics.R
#  DESCRIPTION: Adds EJ metric shapefile 
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-04-21
#  GIT REPO:
#  R version 4.2.0 (2022-04-22 ucrt) x86_64
##############################################################################.

library(sf)
library(tidyverse)

shp_ejmetrics <- read_sf(dsn = "data-raw", layer = "EJMETRICS_2022_NBEP2023")

usethis::use_data(shp_ejmetrics, overwrite=TRUE)

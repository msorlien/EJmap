################################### HEADER ###################################
#  TITLE: shp_ejmetrics.R
#  DESCRIPTION: Adds EJ metric shapefile 
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-04-28
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt) x86_64
##############################################################################.

library(sf)
library(tidyverse)

source('R/calculate_score.R')

# Read in csv data ----
df_metrics <- read.csv('data-raw/metric_list.csv')
df_nbep_metrics <- read.csv('data-raw/nbep_metrics.csv')
df_nbep_cats <- read.csv('data-raw/nbep_cats.csv')

# Isolate category data ----
df_cats <- df_metrics %>%
  select('CATEGORY', 'CAT_CODE') %>%
  distinct()  # Drop duplicate rows

# Shp raw ----
shp_raw <- read_sf(dsn = "data-raw", 
                         layer = "EJMETRICS_2022_NBEP2023") %>%
  # Rename column
  rename(Block_Group=GEOID) %>%
  # Add new column (Town_Code)
  add_column(Town_Code = "ABC", .after="State") %>%
  mutate(Town_Code = paste0(Town, ", ", State)) %>%
  # Replace -999999 with NA
  mutate(across(where(is.numeric), ~na_if(., -999999)))

usethis::use_data(shp_raw, overwrite=TRUE)

# Shp default ----
# * Default metric, category weights
df_metrics_default <- df_metrics %>%
  add_column(WEIGHT = 1)

df_cats_default <- df_cats %>%
  add_column(WEIGHT = 1, MIN_SCORE = 0)

# * Modify shapefile, save
shp_default <- calculate_score(shp_raw, "NBEP", 80, df_metrics_default, 
                               df_cats_default)

usethis::use_data(shp_default, overwrite=TRUE)

# Shp NBEP ----
# * NBEP metric, category weights ----
df_metrics_nbep <- merge(df_metrics, df_nbep_metrics, by="METRIC_CODE")
df_cats_nbep <- merge(df_cats, df_nbep_cats, by="CAT_CODE")

# * Modify shapefile, save
shp_NBEP <- calculate_score(shp_raw, "NBEP", 80, df_metrics_nbep, df_cats_nbep)

usethis::use_data(shp_NBEP, overwrite=TRUE)

################################### HEADER ###################################
#  TITLE: shp_ejmetrics.R
#  DESCRIPTION: Adds EJ metric shapefiles
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-18
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt) x86_64
##############################################################################.

library(sf)
library(tidyverse)

source('R/fun_calculate_score.R')

# Read in csv data ------------------------------------------------------------
df_metrics <- read.csv('data-raw/metric_list.csv')
df_nbep_metrics <- read.csv('data-raw/nbep_metrics.csv')
df_nbep_cats <- read.csv('data-raw/nbep_cats.csv')

# Isolate category data
df_cats <- df_metrics %>%
  select('CATEGORY', 'CAT_CODE') %>%
  distinct()  # Drop duplicate rows

# Shp raw ---------------------------------------------------------------------

shp_raw <- read_sf(dsn = 'data-raw', 
                   layer = 'EJMETRICS_2022_NBEP2023') %>%
  # Rename column
  rename(BlockGroup=GEOID) %>%
  # Add new column (Town_Code)
  add_column(Town_Code = 'ABC', .after='State') %>%
  mutate(
    Town_Code = case_when(
      State == 'Rhode Island' ~ paste0(Town, ', RI'),
      State == 'Massachusetts' ~ paste0(Town, ', MA'),
      State == 'Connecticut' ~ paste0(Town, ', CT'),
      TRUE ~ paste0(Town, ', ', State)
    )
  ) %>%
  # Replace -999999 with NA
  mutate(across(where(is.numeric), ~na_if(., -999999)))

usethis::use_data(shp_raw, overwrite=TRUE)

# Shp raw (simple) ------------------------------------------------------------

shp_raw_simple <- read_sf(dsn='data-raw',
                          layer = 'EJMETRICS_2022_LOWRES_NBEP2023') %>%
  # Rename column
  rename(BlockGroup=GEOID) %>%
  # Add new column (Town_Code)
  add_column(Town_Code = 'ABC', .after='State') %>%
  mutate(
    Town_Code = case_when(
      State == 'Rhode Island' ~ paste0(Town, ', RI'),
      State == 'Massachusetts' ~ paste0(Town, ', MA'),
      State == 'Connecticut' ~ paste0(Town, ', CT'),
      TRUE ~ paste0(Town, ', ', State)
    )
  ) %>%
  # Replace -999999 with NA
  mutate(across(where(is.numeric), ~na_if(., -999999)))

# Shp default (simple) --------------------------------------------------------

# Define metric/cat weights
df_metrics_default <- df_metrics %>%
  add_column(WEIGHT = 1)

df_cats_default <- df_cats %>%
  add_column(WEIGHT = 1, MIN_SCORE = 0)

# Calculate score
shp_default_simple <- calculate_score(
  input_shp = shp_raw_simple, 
  percentile_min = 80, 
  df_metrics = df_metrics_default, 
  df_categories = df_cats_default)

usethis::use_data(shp_default_simple, overwrite=TRUE)

# Shp NBEP --------------------------------------------------------------------
# Define metric/cat weights
df_metrics_nbep <- merge(df_metrics, df_nbep_metrics, by='METRIC_CODE')
df_cats_nbep <- merge(df_cats, df_nbep_cats, by='CAT_CODE')

# Calculate score
shp_nbep <- calculate_score(
  input_shp = shp_raw,
  percentile_min = 80, 
  prefix_list = 'N_',
  exceed_all_min_scores = FALSE,
  df_metrics = df_metrics_nbep, 
  df_categories = df_cats_nbep)

# Drop extra rows, columns
shp_nbep <- shp_nbep %>%
  filter(Study_Area != 'Outside Study Area')

usethis::use_data(shp_nbep, overwrite=TRUE)

# Shp NBEP (simple) -----------------------------------------------------------
# Calculate score
shp_nbep_simple <- calculate_score(
  input_shp = shp_raw_simple,
  percentile_min = 80, 
  prefix_list = 'N_',
  exceed_all_min_scores = FALSE,
  df_metrics = df_metrics_nbep, 
  df_categories = df_cats_nbep)

# Drop extra rows, columns
shp_nbep_simple <- shp_nbep_simple %>%
  filter(Study_Area != 'Outside Study Area') %>%
  select(-starts_with('P_'))  # Drop state percentiles

usethis::use_data(shp_nbep_simple, overwrite=TRUE)

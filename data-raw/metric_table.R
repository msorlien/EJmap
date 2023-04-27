################################### HEADER ###################################
#  TITLE: metric_table.R
#  DESCRIPTION: Creates dataframes of metrics + metric categories
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-04-27
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt) x86_64
##############################################################################.

library(tidyverse)

metric_csv <- read.csv('data-raw/metric_list.csv')

# Create dataframe of metrics
metric_table <- metric_csv %>%
  # Select columns
  select('METRIC', 'METRIC_CODE', 'CAT_CODE', 'DATA_SOURCE', 'SOURCE_YEAR') %>%
  # Add new column
  add_column('WEIGHT' = 1, .before = "DATA_SOURCE")
  
# Create dataframe of categories
cat_table <- metric_csv %>%
  # Select columns
  select('CATEGORY', 'CAT_CODE') %>%
  # Drop duplicate rows
  distinct() %>%
  # Add columns
  add_column('WEIGHT' = 1, 'MIN_VALUE' = 0)

usethis::use_data(metric_table, overwrite=TRUE)
usethis::use_data(cat_table, overwrite=TRUE)

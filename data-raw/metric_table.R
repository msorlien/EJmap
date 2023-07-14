################################### HEADER ###################################
#  TITLE: metric_table.R
#  DESCRIPTION: Imports table of metric column names and metadata
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-01
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt) x86_64
##############################################################################.

library(tidyverse)

# Import table of metric column names and metadata

metric_table <- read.csv('data-raw/metric_list.csv')

usethis::use_data(metric_table, overwrite=TRUE)

# Create list of column/name substitutions 

# List metrics
df_cats <- metric_table %>%
  select(CATEGORY, CAT_CODE) %>%
  rename(COL_NAME = CATEGORY) %>%
  add_column(TYPE='CATEGORY', COL_CODE = 'placeholder') %>%
  mutate(COL_CODE = CAT_CODE)

# List categories
df_metrics <- metric_table %>%
  select(METRIC, METRIC_CODE, CAT_CODE) %>%
  rename(COL_CODE = METRIC_CODE, COL_NAME = METRIC) %>%
  mutate(COL_NAME = paste('-', COL_NAME)) %>%
  add_column(TYPE='METRIC')

# Join tables
df_all <- rbind(unique(df_cats), df_metrics) 

# Set order
cat_order <- c('SOCVUL', 'HEALTH', 'ENVBUR', 'CLIMATE')
type_order = c('CATEGORY', 'METRIC')

# Tweak table
column_table <- df_all %>%
  arrange(match(CAT_CODE, cat_order), 
          match(TYPE, type_order))%>%
  select(COL_CODE, COL_NAME) %>%
  add_row(COL_CODE = 'SCORE', COL_NAME = 'Overall Score')

# Drop row names
rownames(column_table) <- NULL
  
usethis::use_data(column_table, overwrite=TRUE)
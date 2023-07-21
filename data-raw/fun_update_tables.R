################################### HEADER ###################################
#  TITLE: metric_table.R
#  DESCRIPTION: Imports table of metric column names and metadata
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-07-17
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt) x86_64
##############################################################################.

library(tidyverse)

# Create metric table ---------------------------------------------------------

metric_table <- read.csv('data-raw/metric_list.csv')

usethis::use_data(metric_table, overwrite=TRUE)

# Create category table -------------------------------------------------------

cat_table <- metric_table %>%
  select(CATEGORY, CAT_CODE) %>%
  distinct() %>% # Drop duplicate rows
  add_column(WEIGHT = 1, MIN_SCORE = NA_integer_)

usethis::use_data(cat_table, overwrite=TRUE)

# Create table of layer names --------------------------------------------------

# List metrics
df_cats <- cat_table %>%
  select(CATEGORY, CAT_CODE) %>%
  rename(COL_NAME = CATEGORY) %>%
  add_column(TYPE = 'CATEGORY', COL_CODE = 'placeholder') %>%
  mutate(COL_CODE = CAT_CODE)

# List categories
df_metrics <- metric_table %>%
  select(METRIC, METRIC_CODE, CAT_CODE) %>%
  rename(COL_CODE = METRIC_CODE, COL_NAME = METRIC) %>%
  mutate(COL_NAME = paste('-', COL_NAME)) %>%
  add_column(TYPE = 'METRIC')

# Join tables
df_all <- rbind(df_cats, df_metrics) 

# Set order
cat_order <- c('SOCVUL', 'HEALTH', 'ENVBUR', 'CLIMATE')
type_order = c('CATEGORY', 'METRIC')

# Tweak table
column_table <- df_all %>%
  arrange(match(CAT_CODE, cat_order), 
          match(TYPE, type_order))%>%
  select(COL_CODE, COL_NAME) %>%
  add_row(COL_CODE = 'SCORE', COL_NAME = 'Overall Score') %>%
  add_row(COL_CODE = 'EJAREA', COL_NAME = 'EJ Areas')

# Drop row names
rownames(column_table) <- NULL
  
usethis::use_data(column_table, overwrite=TRUE)

# Create metadata table --------------------------------------------------------

# TO DO
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

COL_CODE <- c(paste0("N_", metric_table$METRIC_CODE),
               paste0("P_", metric_table$METRIC_CODE),
               paste0("S_", unique(metric_table$CAT_CODE)),
               "S_SCORE")
COL_NAME <- c(metric_table$METRIC, 
               metric_table$METRIC, 
               paste(unique(metric_table$CATEGORY), "Score"),
               "Overall Score")

column_table <- data.frame(COL_CODE, COL_NAME)
usethis::use_data(column_table, overwrite=TRUE)
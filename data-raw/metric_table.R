################################### HEADER ###################################
#  TITLE: metric_table.R
#  DESCRIPTION: Imports table of metric column names and metadata
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-04-27
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt) x86_64
##############################################################################.

library(tidyverse)

metric_table <- read.csv('data-raw/metric_list.csv')

usethis::use_data(metric_table, overwrite=TRUE)

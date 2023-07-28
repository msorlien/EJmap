#  TITLE: fun_wrap_text.R
#  DESCRIPTION: Wrap text, add custom divisor
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-07-21
#  GIT REPO: nbep/ejmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64

library(stringr)

# VARIABLES
# old_list: input list
# str_len: length of string before wrap
# linebreak: string to place in line breaks

wrap_text <- function(old_list, str_len, linebreak){
  new_list <- stringr::str_wrap(old_list, width = str_len)
  new_list <- gsub('\n', linebreak, new_list)
}



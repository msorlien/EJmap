#  TITLE: fun_color_pal
#  DESCRIPTION: Calculates scores
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-05-19
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
# -----------------------------------------------------------------------------

library(leaflet)


pal_colors <- function(var_name, percentile_min, min_overall_score) {
  
  # Define var
  pal_min <- c(0, 50, 60, 70, 80, 90, 95)
  pal_roy <- c('#FED976', '#FEB24C', '#FD8D3C', '#FC4E2A', '#E31A1C', 
               '#BD0026', '#6C0000') 
  pal_grey <- c('#F0F0F0', '#D7D7D7', '#B7B7B7', '#999999', '#7C7C7C', 
                '#626262', '#404040')
  
  df_pal <- data.frame(pal_min, pal_roy, pal_grey) %>%
    mutate(pal_roy = if_else(pal_min >= percentile_min, pal_roy, pal_grey))
  
  # Default palette
  pal_score <- colorBin(
    palette = df_pal$pal_roy,
    bins = c(0, 50, 60, 70, 80, 90, 95, 100),
    na.color = '#FFFFFF',
    domain = c(0,100)
  )

  # Palette for cat score, final score columns
  if (var_name == 'SCORE' & min_overall_score > 0) {
    pal_score <- colorBin(
      palette = c('#FFEDA0', '#F03B20'),
      bins = c(0, min_overall_score, 100),
      na.color = '#CCCCCC',
      domain = c(0, 100)
    )
  } else if (var_name %in% c('SOCVUL', 'HEALTH', 'ENVBUR', 'CLIMATE', 
                             'SCORE')) {
    pal_score <- colorBin(
      palette = c('#FFFFCC', '#FFEDA0', '#FED976', '#FEB24C', '#FD8D3C', 
                  '#FC4E2A', '#E31A1C', '#BD0026', '#800026', '#400015'),
      bins = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
      na.color = '#CCCCCC',
      domain = c(0, 100)
    )
  }
  
  return(pal_score)
}






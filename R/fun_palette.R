#  TITLE: fun_color_pal
#  DESCRIPTION: Calculates scores
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2024-01-26
#  GIT REPO: NBEP/EJmap
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64
# -----------------------------------------------------------------------------

library(leaflet)

pal_colors <- function(var_name, percentile_min = 80) {
  
  # Define var
  pal_min <- c(0, 50, 60, 70, 80, 90, 95)
  pal_roy <- c('#FED976', '#FEB24C', '#FD8D3C', '#FC4E2A', '#E31A1C', 
               '#BD0026', '#6C0000') 
  pal_grey <- c('#f2f2f2', '#d9d9d9', '#c2c2c2', '#ababab', '#949494', 
                '#7a7a7a', '#636363')
  
  df_pal <- data.frame(pal_min, pal_roy, pal_grey) %>%
    mutate(pal_roy = if_else(pal_min >= percentile_min, pal_roy, pal_grey))
  
  # Default palette
  pal_score <- colorBin(
    palette = df_pal$pal_roy,
    bins = c(0, 50, 60, 70, 80, 90, 95, 100),
    na.color = rgb(0,0,0,0),
    domain = c(0,100)
  )

  # Palette for cat score, final score columns
  if (var_name == 'EJAREA') {
    pal_score <- colorFactor(
      palette = c('#F03B20', '#FFEDA0', '#B1B1B1'),
      levels = c('Yes', 'No', 'No Data')
    )
  } else if (var_name %in% c('SOCVUL', 'HEALTH', 'ENVBUR', 'CLIMATE', 
                             'SCORE')) {
    pal_score <- colorBin(
      palette = c('#FFFFCC', '#FFEDA0', '#FED976', '#FEB24C', '#FD8D3C', 
                  '#FC4E2A', '#E31A1C', '#BD0026', '#800026', '#400015'),
      bins = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
      na.color = rgb(0,0,0,0),
      domain = c(0, 100)
    )
  }
  
  return(pal_score)
}






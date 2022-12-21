library(rvest)
library(tidyverse)

# https://community.rstudio.com/t/convert-wind-direction-degrees-into-factors-in-a-data-frame/14636

url <- 'http://snowfence.umn.edu/Components/winddirectionanddegreeswithouttable3.htm'
page <- rvest::read_html(url)
directions_raw <- page %>% rvest::html_node('td table') %>% rvest::html_table(header = TRUE)

directions <- directions_raw %>%
  purrr::set_names(~tolower(sub(' Direction', '', .x))) %>%
  dplyr::slice(-1) %>%
  tidyr::separate(degree, c('degree_min', 'degree_max'), sep = '\\s+-\\s+', convert = TRUE)

wshds_test <- wshds %>%
  filter(stream_crossing_id == 198064) %>%
  elevatr::get_elev_raster(., 14) %>%
  raster::crop(., wshds %>% filter(stream_crossing_id == 198064)) %>%
  raster::terrain(opt = c('aspect'), unit = 'degrees') %>%
  rayshader::raster_to_matrix()

try <- wshds %>%
  filter(stream_crossing_id == 198064) %>%
  mutate(asp_median = median(wshds_test, na.rm = T),
         asp_mean = mean(wshds_test, na.rm = T)) %>%
  mutate(asp_cardinal = cut(
    asp_median,
    breaks = c(0, directions$degree_max, 360),
    labels = c(directions$cardinal, 'N')
  ))


a <- wshds %>%
  filter(stream_crossing_id == 198064) %>%
  select(stream_crossing_id) %>%
  fpr::fpr_sp_elev()







table_phase2_overview <- function(dat, caption_text = '', font = font_set, scroll = TRUE){
  dat2 <- dat %>%
    kable(caption = caption_text, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed"),
                              full_width = T,
                              font_size = font) %>%
    kableExtra::column_spec(column = c(9), width_min = '1.5in') %>%
    kableExtra::column_spec(column = c(5), width_max = '1in')

  if(identical(scroll,TRUE)){
    dat2 <- dat2 %>%
      kableExtra::scroll_box(width = "100%", height = "500px")
  }
  dat2
}

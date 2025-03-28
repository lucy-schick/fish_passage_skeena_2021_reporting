# install.packages('pak')


pkgs_cran <- c(
  #'plotKML', #this takes forever to load so going to leave it out for now
  'raster', #load this dog before dplyr yo
  'tidyverse',
  'readwritesqlite',
  'sf',
  'readxl',
  'janitor',
  'leafem',
  'leaflet',
  'httr',
  'RPostgres',
  'RPostgreSQL',
  'DBI',
  'magick',
  'bcdata',
  'jpeg',
  'datapasta',
  'knitr',
  'data.table',
  'lubridate',
  'forcats',
  'bookdown',
  'fasstr',
  'tidyhydat',
  'elevatr',
  'rayshader',
  'geojsonio',
  'english',
  'leaflet.extras',
  'ggdark',
  'pdftools',
  'chron',
  'leafpop',
  'exifr',
  'pagedown',
  'kableExtra'
  # rgl,
  # geojsonsf,
  # bit64 ##to make integer column type for pg
  # gert  ##to track git moves
  )


pkgs_gh <- c(
  "poissonconsulting/fwapgr",
  'poissonconsulting/poisspatial',
  "poissonconsulting/fishbc",
  "newgraphenvironment/fpr"
  # "haozhu233/kableExtra@a9c509a" ## 2024 update, this prevents black text in dark mode
)

pkgs_all <- c(pkgs_cran,
              pkgs_gh)

if(params$update_packages){
  lapply(pkgs_all,
         pak::pkg_install,
         ask = FALSE)
}


# load all the packages
pkgs_ld <- c(pkgs_cran,
             basename(pkgs_gh))

lapply(pkgs_ld,
       require,
       character.only = TRUE)

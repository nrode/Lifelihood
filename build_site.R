
# https://roxygen2.r-lib.org/
# https://pkgdown.r-lib.org/articles/pkgdown.html

rm(list=ls())
devtools::document()
devtools::install()
pkgdown::build_site()

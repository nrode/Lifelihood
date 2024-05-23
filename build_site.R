# https://roxygen2.r-lib.org/
# https://pkgdown.r-lib.org/articles/pkgdown.html

rm(list=ls())
library(roxygen2)
library(Lifelihood)
Sys.setenv(RSTUDIO_PANDOC = "/opt/homebrew/bin/")

# generate documentation
devtools::document()

# generate pkgdown site
devtools::install()
pkgdown::build_site()

library(devtools)
document()

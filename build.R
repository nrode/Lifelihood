rm(list=ls()) # clean the entire environment

devtools::load_all() # load the package (to use for development mode)

devtools::document() # create all .Rd files using source files

devtools::build(path = ".") # create bundled file of the pkg
devtools::install() # install local development of the pkg (to allow `library(lifelihood)`)

Sys.setenv(RSTUDIO_PANDOC = "/opt/homebrew/bin/") # explicit pandoc location to avoid error
pkgdown::build_site() # build documentation site
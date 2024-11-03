devtools::load_all(compile = FALSE) # load the package (to use for development mode)

devtools::test() # run tests

devtools::document() # create all .Rd files using source files
Sys.setenv(RSTUDIO_PANDOC = "/opt/homebrew/bin/") # explicit pandoc location to avoid error
pkgdown::build_site() # build documentation site

devtools::build(path = ".") # create bundled file of the pkg (compressed in a single file)
devtools::install() # install local development of the pkg (to allow `library(lifelihood)`)

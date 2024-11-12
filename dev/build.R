devtools::load_all(compile = FALSE) # load the package (to use for development mode)

devtools::test() # run tests
#devtools::check() # ovrall package check

devtools::document() # create all .Rd files using source files
Sys.setenv(RSTUDIO_PANDOC = "/opt/homebrew/bin/") # explicit pandoc location to avoid error
pkgdown::build_site(preview = FALSE) # build documentation site

devtools::build(path = ".") # create bundled file of the pkg (compressed in a single one)
devtools::install() # install local development of the pkg (to allow `library(lifelihood)`)

rm(list = ls())
devtools::build()
devtools::load_all()

# If conflicts persist, remove specific objects causing the conflicts
rm(list = c("get_effects", "get_likelihood", "get_param_ranges", "get_ratio_max", "get_seeds", "parse"))

devtools::load_all()
devtools::update_packages()
devtools::install()


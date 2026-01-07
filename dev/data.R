#############################################################
# Script that generage the datasets available in lifelihood #
# This is mostly useful for examples in the documentation   #
#############################################################

# Make `datapierrick` available
datapierrick <- read.csv(here::here(
  "data_internals/raw_data/DataPierrick/100%mort_Pierrick211genoparinteraction.csv"
))
usethis::use_data(datapierrick)

# Make `fakesample` available
fakesample <- read.csv(here::here("data_internals/fake_sample.csv"))
usethis::use_data(fakesample)

# Make `datalenski` available
datalenski <- read.csv(here::here("data_internals/lenski.csv"))
usethis::use_data(datalenski)


devtools::load_all()
lifelihood::datapierrick

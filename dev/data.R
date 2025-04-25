# Script that generage the datasets available in lifelihood

datapierrick <- read.csv(here::here(
  "data_internals/raw_data/DataPierrick/100%mort_Pierrick211genoparinteraction.csv"
))
usethis::use_data(datapierrick)

fakesample <- read.csv(here::here("data_internals/fake_sample.csv"))
usethis::use_data(fakesample)

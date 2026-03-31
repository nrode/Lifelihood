#############################################################
# Script that generage the datasets available in lifelihood #
# This is mostly useful for examples in the documentation   #
#############################################################

# Make `datapierrick` available
datapierrick <- read.csv(here::here(
  "data_internals/raw_data/DataPierrick/100%mort_Pierrick211genoparinteraction.csv"
)) |>
  rename_with(~ sub("^pon_", "clutch_", .x), starts_with("pon_"))
usethis::use_data(datapierrick)

# Make `datadaphnia` available
datadaphnia <- read.csv(here::here(
  "data_internals/raw_data/DataPierrick/DataPierrick_interceptonlynewfemellessponteavec0newm(1).csv"
)) |>
  rename_with(~ sub("^pon_", "clutch_", .x), starts_with("pon_"))
usethis::use_data(datadaphnia)

# Make `fakesample` available
fakesample <- read.csv(here::here("data_internals/fake_sample.csv"))
usethis::use_data(fakesample)

# Make `datalenski` available
datalenski <- read.csv(here::here("data_internals/lenski.csv")) |>
  rename_with(~ sub("^pon_", "clutch_", .x), starts_with("pon_"))
usethis::use_data(datalenski)

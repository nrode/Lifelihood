devtools::load_all()
library(tidyverse)

df <- datadaphnia |>
  as_tibble() |>
  mutate(
    par = as.factor(par),
    geno = as.factor(geno),
    spore = as.factor(spore)
  ) |>
  filter(par == 0, !is.na(clutch_start_2)) |>
  select(
    geno,
    spore,
    par,
    sex_start,
    sex_end,
    sex,
    mat_start,
    mat_end,
    mat,
    clutch_start_1,
    clutch_end_1,
    clutch_size_1,
    clutch_start_2,
    clutch_start_3,
    clutch_end_2,
    clutch_end_3,
    clutch_size_2
  ) |>
  mutate(
    death_start = clutch_start_2,
    death_end = clutch_end_2,
    clutch_start_2 = if_else(clutch_start_1 < 16, NA, clutch_start_2),
    clutch_end_2 = if_else(clutch_start_1 < 16, NA, clutch_end_2),
    clutch_size_2 = if_else(clutch_start_1 < 16, NA, clutch_size_2),
  )

lifelihoodData <- as_lifelihoodData(
  df = df,
  matclutch = FALSE,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "mat_start",
  maturity_end = "mat_end",
  clutchs = c(
    "clutch_start_1",
    "clutch_end_1",
    "clutch_size_1",
    "clutch_start_2",
    "clutch_end_2",
    "clutch_size_2"
  ),
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("par", "geno"),
  model_specs = c("wei", "gam", "lgn")
)

results_sans_tradeoff <- lifelihood(
  lifelihoodData = lifelihoodData,
  path_config = use_test_config("config_tradeoff"),
  delete_temp_files = FALSE
)

#######################

results_avec_tradeoff <- lifelihood(
  lifelihoodData = lifelihoodData,
  path_config = use_test_config("config_tradeoff"),
  delete_temp_files = FALSE
)

AIC(results_sans_tradeoff)
AIC(results_avec_tradeoff)

devtools::load_all()
library(tidyverse)

generate_clutch_vector <- function(N) {
  return(paste(
    "clutch",
    rep(c("start", "end", "size"), N),
    rep(1:N, each = 3),
    sep = "_"
  ))
}
clutchs <- generate_clutch_vector(3)


df <- data_lin_decrease_hazard |> as_tibble() |> mutate(block = 1)
df_100 <- bind_rows(replicate(100, df, simplify = FALSE))

lifelihoodData <- as_lifelihoodData(
  df = df_100,
  matclutch = FALSE,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  block = "block",
  maturity_start = "mat_start",
  maturity_end = "mat_end",
  clutchs = clutchs,
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("Temp", "Bip"),
  dist = c("wei", "wei", "wei")
)

results_without_tradeoff <- lifelihood(
  lifelihoodData = lifelihoodData,
  path_config = use_test_config("config_without_tradeoff"),
  delete_temp_files = FALSE,
  n_fit = 10
)

results_with_tradeoff <- lifelihood(
  lifelihoodData = lifelihoodData,
  path_config = use_test_config("config_with_tradeoff"),
  delete_temp_files = FALSE,
  n_fit = 2
)

AIC(results_without_tradeoff)
AIC(results_with_tradeoff)

prediction(results_with_tradeoff, "lin_decrease_hazard", type = "response")

simul_with_tradeoff <- simulate_life_history(results_with_tradeoff) |>
  mutate(sex = df$sex, Temp = df$Temp, Bip = df$Bip)


clutchs <- generate_clutch_vector(9)

lifelihoodData <- as_lifelihoodData(
  df = simul_with_tradeoff,
  matclutch = FALSE,
  sex = "sex",
  sex_start = "death_start",
  sex_end = "death_end",
  maturity_start = "maturity_start",
  maturity_end = "maturity_end",
  clutchs = clutchs,
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("Temp", "Bip"),
  dist = c("wei", "wei", "wei")
)

results_with_tradeoff <- lifelihood(
  lifelihoodData = lifelihoodData,
  path_config = use_test_config("config_with_tradeoff"),
  delete_temp_files = FALSE,
  n_fit = 10
)

################################

devtools::load_all()
library(tidyverse)


df <- data_no_lin_decrease_hazard |> as_tibble()

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
  covariates = c("Temp", "Bip"),
  dist = c("wei", "wei", "wei")
)

results_without_tradeoff <- lifelihood(
  lifelihoodData = lifelihoodData,
  path_config = use_test_config("config_without_tradeoff"),
  delete_temp_files = FALSE,
  n_fit = 10
)

results_with_tradeoff <- lifelihood(
  lifelihoodData = lifelihoodData,
  path_config = use_test_config("config_with_tradeoff"),
  delete_temp_files = FALSE,
  n_fit = 10
)

AIC(results_without_tradeoff)
AIC(results_with_tradeoff)

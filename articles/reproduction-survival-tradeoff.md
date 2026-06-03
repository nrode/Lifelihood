# Reproduction survival tradeoff (TODO)

``` r

# library(lifelihood)
# library(tidyverse)

# generate_clutch_vector <- function(N) {
#   return(paste(
#     "clutch",
#     rep(c("start", "end", "size"), N),
#     rep(1:N, each = 3),
#     sep = "_"
#   ))
# }
# clutchs <- generate_clutch_vector(3)

# df <- data_lin_decrease_hazard |> as_tibble() |> mutate(block = 1)

# lifelihoodData <- as_lifelihoodData(
#   df = df,
#   sex = "sex",
#   sex_start = "sex_start",
#   sex_end = "sex_end",
#   block = "block",
#   maturity_start = "mat_start",
#   maturity_end = "mat_end",
#   matclutch = TRUE,
#   matclutch_size = "mat",
#   clutchs = clutchs,
#   death_start = "death_start",
#   death_end = "death_end",
#   covariates = c("Temp", "Bip"),
#   dist = c("wei", "wei", "wei")
# )

# results_with_tradeoff <- lifelihood(
#   lifelihoodData = lifelihoodData,
#   path_config = use_test_config("config_with_tradeoff"),
#   delete_temp_files = FALSE,
#   n_fit = 5
# )

# results_without_tradeoff <- lifelihood(
#   lifelihoodData = lifelihoodData,
#   path_config = use_test_config("config_without_tradeoff"),
#   delete_temp_files = FALSE,
#   n_fit = 5
# )

# AIC(results_with_tradeoff)
# AIC(results_without_tradeoff)
# simulate_life_history(results_without_tradeoff)
# simulate_life_history(results_with_tradeoff)
```

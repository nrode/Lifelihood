path_config <- "tests/testthat/config_tradeoff.yaml"

df <- datapierrick |>
  as_tibble() |>
  mutate(
    par = as.factor(par),
    geno = as.factor(geno),
    spore = as.factor(spore)
  )

clutchs <- generate_clutch_vector(28)

lifelihoodData <- as_lifelihoodData(
  df = df,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "mat_start",
  maturity_end = "mat_end",
  clutchs = clutchs,
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("par", "geno"),
  model_specs = c("wei", "gam", "lgn")
)

results <- lifelihood(
  lifelihoodData,
  path_config = path_config,
  raise_estimation_warning = FALSE
)

sim_reproduction <- simulate_life_history(
  results,
  event = "reproduction",
  seed = 1
)

sim_mortality <- simulate_life_history(results, event = "mortality", seed = 1)

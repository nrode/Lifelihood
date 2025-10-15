devtools::load_all()
library(tidyverse)


df <- datapierrick |>
  as_tibble() |>
  mutate(
    par = as.factor(par),
    geno = as.factor(geno),
    spore = as.factor(spore)
  )

generate_clutch_vector <- function(N) {
  return(paste(
    "pon",
    rep(c("start", "end", "size"), N),
    rep(1:N, each = 3),
    sep = "_"
  ))
}
clutchs <- generate_clutch_vector(28)

lifelihoodData <- lifelihoodData(
  df = df,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "mat_start",
  maturity_end = "mat_end",
  clutchs = clutchs,
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("par", "geno", "spore"),
  model_specs = c("wei", "gam", "exp")
)

results <- lifelihood(
  lifelihoodData = lifelihoodData,
  path_config = get_config_path("config_pierrick"),
  delete_temp_files = FALSE,
  seeds = c(1, 2, 3, 4)
)

results <- lifelihood(
  lifelihoodData = lifelihoodData,
  path_config = get_config_path("config_pierrick"),
  delete_temp_files = FALSE,
  n_fit = 3
)

# results$vcov <- -data.frame(
#   c(-0.00410321, 0.00400797, 0.00402230, -0.00046523),
#   c(0.00400797, -0.00478376, -0.00407384, -0.00037870),
#   c(0.00402230, -0.00407384, -0.00579095, -0.00025174),
#   c(-0.00046523, -0.00037870, -0.00025174, -0.00412250)
# )
# prediction(results, "expt_death", se.fit = TRUE) |> head()

coef(results)
coeff(results, "expt_death")
coeff(results, "survival_param2")
coeff(results, "n_offspring")
AIC(results)
BIC(results)
logLik(results)

prediction(results, parameter_name = "n_offspring") |> head()
prediction(results, parameter_name = "expt_death") |> head()
prediction(results, parameter_name = "expt_death", type = "response") |> head()

simulate_life_history(results) |> head()
simulate_life_history(results, event = "mortality") |> head()
simulate_life_history(results, event = "maturity") |> head()
simulate_life_history(results, event = "reproduction") |> head()
parallel.simulate(results, nsim = 10, parallel_seed = 1)

## PLot fitted/observed mortality rates
plot_fitted_mortality_rate(
  lifelihoodResults = results,
  interval_width = 10,
  add_observed_mortality_rate = TRUE,
  groupby = c("spore", "par"),
  use_facet = TRUE,
  newdata = NULL,
  max_time = NULL,
  xlab = "Age (days)",
  ylab = "Fitted Mortality Rate"
)

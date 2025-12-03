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
  covariates = c("par", "spore"),
  model_specs = c("wei", "gam", "exp")
)

results <- lifelihood(
  lifelihoodData = lifelihoodData,
  se.fit = TRUE,
  path_config = get_config_path("config_pierrick"),
  delete_temp_files = FALSE,
  seeds = c(1, 2, 3, 4),
)

prediction(results, "expt_death", se.fit = TRUE)
prediction(
  results,
  "expt_death",
  se.fit = TRUE,
  mcmc.fit = TRUE,
  type = "response"
) |>
  head(15)
prediction(results, "expt_death", type = "response") |>
  head(15)

summary(results)


coef(results)
coeff(results, "expt_death")
coeff(results, "survival_param2")
coeff(results, "n_offspring")
AIC(results)
BIC(results)
logLik(results)

prediction(
  results,
  se.fit = FALSE,
  parameter_name = "expt_death",
  type = "quantile",
  p = 0.1
)
prediction(
  results,
  se.fit = TRUE,
  parameter_name = "expt_death"
)
prediction(results, parameter_name = "n_offspring") |> head()
prediction(results, parameter_name = "expt_death", type = "response") |> head()

simulate_life_history(results) |> head()
simulate_life_history(results, event = "mortality") |> head()
simulate_life_history(results, event = "maturity") |> head()
simulate_life_history(results, event = "reproduction") |> head()
parallel.simulate(results, nsim = 10, parallel_seed = 1)

## PLot fitted/observed mortality rates
lifelihoodResults <- results
interval_width = 10
event = "mortality"
use_facet = TRUE
groupby = "spore"
xlab = "Age (days)"
ylab = "Fitted Maturity Rate"

rate_df <- compute_fitted_event_rate(
  lifelihoodResults = lifelihoodResults,
  interval_width = interval_width,
  event = event,
  newdata = NULL,
  max_time = NULL,
  groupby = groupby
)

plot_fitted_event_rate(
  results,
  interval_width = 10,
  event = "mortality",
  use_facet = TRUE,
  groupby = "all",
  xlab = "Age (days)",
  ylab = "Fitted Maturity Rate"
)

plot_observed_event_rate(
  lifelihoodData,
  interval_width = 10,
  groupby = "par",
  event = "maturity",
  use_facet = TRUE,
  xlab = "Age (days)",
  ylab = "Observed Mortality Rate"
)

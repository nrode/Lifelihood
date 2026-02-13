devtools::load_all()
library(tidyverse)

df <- datapierrick |>
  as_tibble() |>
  mutate(
    par = as.factor(par),
    geno = as.factor(geno),
    spore = as.factor(spore),
    block = rep(1:2, each = nrow(datapierrick) / 2)
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
  covariates = c("par", "spore"),
  model_specs = c("wei", "gam", "lgn")
)

results <- lifelihood(
  lifelihoodData = lifelihoodData,
  path_config = use_test_config("config_gbg"),
  delete_temp_files = FALSE,
  group_by_group = TRUE,
  n_fit = 10
)
summary(results)

prediction(results, "expt_death", type = "response", mcmc.fit = TRUE)
prediction(results, "expt_death", type = "response", se.fit = TRUE)

prediction(results, "survival_param2", type = "response", mcmc.fit = TRUE)
prediction(results, "expt_death", mcmc.fit = TRUE, se.fit = TRUE) |> head()
prediction(
  results,
  "expt_death",
  se.fit = TRUE,
  type = "response"
) |>
  head()
prediction(results, "expt_death", type = "response") |> head()

coef(results)
coeff(results, "expt_death")
coeff(results, "survival_param2")
coeff(results, "n_offspring")
AIC(results)
BIC(results)
logLik(results)

prediction(
  results,
  parameter_name = "n_offspring",
  mcmc.fit = TRUE,
  type = "response"
) |>
  head()
prediction(results, parameter_name = "expt_death", type = "response") |> head()
prediction(results, parameter_name = "expt_reproduction", type = "response") |>
  head()
prediction(
  results,
  parameter_name = "reproduction_param2",
  type = "response"
) |>
  tail()

simulate_life_history(results) |> head()
z <- simulate_life_history(
  results,
  event = c("mortality", "maturity"),
  use_censoring = TRUE,
  seed = 1
)
simulate_life_history(results, event = "mortality") |> head()
simulate_life_history(results, event = "maturity") |> head()
simulate_life_history(results, event = "reproduction") |> head()
parallel.simulate(results, nsim = 10, parallel_seed = 1)

interval_width <- 15
newdata <- expand.grid(
  par = c(0, 1, 2),
  time = seq(
    from = 0,
    to = (10 - 1) * interval_width,
    by = interval_width
  )
) |>
  mutate(
    Interval_start = time,
    Interval_end = time + interval_width,
    Mean_Interval = time + interval_width / 2
  )

rate_df <- compute_fitted_event_rate(
  lifelihoodResults = results,
  interval_width = 15,
  mcmc.ci.fit = TRUE,
  event = "mortality",
  groupby = "par"
)

rate_df <- compute_observed_event_rate(
  lifelihoodData = lifelihoodData,
  interval_width = 15,
  event = "reproduction"
)


plot_observed_event_rate(
  lifelihoodData,
  interval_width = 2,
  groupby = "par",
  event = "reproduction",
  use_facet = TRUE,
  xlab = "Time since last reproduction (days)",
  ylab = "Observed Reproduction Rate"
)

plot_fitted_event_rate(
  results,
  interval_width = 2,
  event = "mortality",
  use_facet = TRUE,
  groupby = "par",
  xlab = "Age (days)",
  ylab = "Fitted Mortality Rate",
)

# Create simulation inputs

[`create_simulation_input()`](https://nrode.github.io/Lifelihood/reference/create_simulation_input.md)
lets you simulate life histories from parameter values that you choose
yourself. It is useful when you want to explore a model before fitting
it, create a known synthetic population, or understand how covariate
effects change simulated maturity, reproduction, and mortality.

``` mermaid
flowchart LR
  data["data<br/>covariates + sex + counts"] --> input["create_simulation_input()"]
  effects["effects<br/>chosen parameter values"] --> input
  config["config<br/>which formulas are fitted"] --> input
  input --> results["lifelihoodResults-like object"]
  results --> pred["prediction()"]
  results --> sim["simulate_life_history()"]
```

## Load packages

``` r

devtools::load_all()
#> ℹ Loading lifelihood
#> Loading required package: tidyverse
#> 
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.2.1     ✔ readr     2.2.0
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
#> ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
#> ✔ purrr     1.2.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
library(tidyverse)
```

## Create the population to simulate

The `data` argument describes the individuals you want to simulate. It
must contain the covariate columns and the sex column. Here, each row is
one combination of factor levels, and `n_individuals` says how many
individuals to create for that combination.

``` r

population <- crossing(
  par = as.factor(c("none", "low", "high")),
  spore = as.factor(c("absent", "present")),
  sex = 0
) |>
  mutate(n_individuals = 100)

population
#> # A tibble: 6 × 4
#>   par   spore     sex n_individuals
#>   <fct> <fct>   <dbl>         <dbl>
#> 1 high  absent      0           100
#> 2 high  present     0           100
#> 3 low   absent      0           100
#> 4 low   present     0           100
#> 5 none  absent      0           100
#> 6 none  present     0           100
```

## Define the fitted formulas

The config tells `lifelihood` which parameters exist and which
covariates each parameter uses. This example uses:

- `par` and `spore` for mortality
- `par` for maturity
- `par` and `spore` for reproduction timing
- `par` for the number of offspring

``` r

simulation_config <- list(
  mortality = list(
    expt_death = 1,
    survival_param2 = 1,
    ratio_expt_death = "not_fitted",
    prob_death = "not_fitted",
    sex_ratio = "not_fitted"
  ),
  maturity = list(
    expt_maturity = 1,
    maturity_param2 = 1,
    ratio_expt_maturity = "not_fitted"
  ),
  reproduction = list(
    expt_reproduction = 1,
    reproduction_param2 = 1,
    n_offspring = 1,
    increase_death_hazard = "not_fitted",
    tof_decay = "not_fitted",
    increase_death_hazard_n_offspring = "not_fitted",
    lin_decrease_hazard = "not_fitted",
    quad_decrease_hazard = "not_fitted",
    lin_change_n_offspring = "not_fitted",
    quad_change_n_offspring = "not_fitted",
    tof_n_offspring = "not_fitted",
    fitness = "not_fitted"
  )
)
```

## Choose the effects

Effects are written on the lifelihood link scale. A negative value
pushes the response toward the lower bound, and a positive value pushes
it toward the upper bound.

For categorical covariates, give one effect for every non-reference
level. In this example, `par` has levels `none`, `low`, and `high`, so
`par = c(...)` contains two effects: one for `low` and one for `high`.

``` r

effects <- list(
  expt_death = list(intercept = 0), #, par = c(0.5, -0.4), spore = -0.5),
  survival_param2 = 0,
  expt_maturity = list(intercept = 0), # par = c(-0.3, -0.6)),
  maturity_param2 = 0,
  expt_reproduction = list(intercept = 0), # par = c(0.3, 0.6), spore = -0.25),
  reproduction_param2 = 0,
  n_offspring = list(intercept = 0) #, par = c(0.2, 0.5))
)
```

Be careful that an intercept set at 0 on the Lifelihood scale
corresponds to an intercept set at the mean between the min and max
bounds of the parameter defined via
[`default_bounds_df()`](https://nrode.github.io/Lifelihood/reference/default_bounds_df.md)
or via the `param_bounds_df` argument in
[`create_simulation_input()`](https://nrode.github.io/Lifelihood/reference/create_simulation_input.md).

## Build the simulation input

`covariates` and `sex` are column names in `data`. The
`n_per_combination` argument names the column that should be used to
expand each row into multiple individuals.

If `param_bounds_df` is not supplied,
[`create_simulation_input()`](https://nrode.github.io/Lifelihood/reference/create_simulation_input.md)
uses
[`default_bounds_df()`](https://nrode.github.io/Lifelihood/reference/default_bounds_df.md)
internally.

``` r

pseudo_results <- create_simulation_input(
  effects = effects,
  data = population,
  covariates = c("par", "spore"),
  sex = "sex",
  config = simulation_config,
  dist = c("wei", "wei", "wei"),
  n_per_combination = "n_individuals"
)

bounds_df <- default_bounds_df(pseudo_results$lifelihoodData)
bounds_df$max[bounds_df$param == "expt_death"] <- 100
bounds_df$max[bounds_df$param == "survival_param2"] <- 2
bounds_df$max[bounds_df$param == "expt_maturity"] <- 10
bounds_df$max[bounds_df$param == "maturity_param2"] <- 2
bounds_df$max[bounds_df$param == "expt_reproduction"] <- 5
bounds_df$max[bounds_df$param == "reproduction_param2"] <- 2

pseudo_results$param_bounds_df <- bounds_df
pseudo_results$sample_size
#> [1] 600
```

> If your `data` already has one row per individual, omit
> `n_per_combination`.

## Verify values of the parameters on the response scale

Please check that the values provided on the Lifelihood scales (defined
in `effects` above) are realistic on the response scale.

``` r

prediction(pseudo_results, "expt_maturity", type = "response") |>
  as_tibble() |>
  bind_cols(pseudo_results$lifelihoodData$df) |>
  distinct(par, spore, value)
#> # A tibble: 6 × 3
#>   par   spore   value
#>   <fct> <fct>   <dbl>
#> 1 high  absent   5.00
#> 2 high  present  5.00
#> 3 low   absent   5.00
#> 4 low   present  5.00
#> 5 none  absent   5.00
#> 6 none  present  5.00
```

``` r

prediction(pseudo_results, "maturity_param2", type = "response") |>
  as_tibble() |>
  bind_cols(pseudo_results$lifelihoodData$df) |>
  distinct(par, spore, value)
#> # A tibble: 6 × 3
#>   par   spore   value
#>   <fct> <fct>   <dbl>
#> 1 high  absent   1.02
#> 2 high  present  1.02
#> 3 low   absent   1.02
#> 4 low   present  1.02
#> 5 none  absent   1.02
#> 6 none  present  1.02
```

## Simulate life histories

The object returned by
[`create_simulation_input()`](https://nrode.github.io/Lifelihood/reference/create_simulation_input.md)
can be passed directly to
[`simulate_life_history()`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md).

``` r

visits <- data.frame(block = 1, visit = seq(0.001, 1000, by = 0.1))
pseudo_results$lifelihoodData$block <- "block"
pseudo_results$lifelihoodData$df$block <- 1

simulated <- simulate_life_history(
  pseudo_results,
  seed = 1,
  use_censoring = TRUE,
  visits = visits
)

simulated |> head()
#> # A tibble: 6 × 658
#>   par   spore  block   sex sex_start sex_end mortality mortality_start
#>   <fct> <fct>  <dbl> <dbl>     <dbl>   <dbl>     <dbl>           <dbl>
#> 1 high  absent     1     0       990    1000     16.8            16.8 
#> 2 high  absent     1     0       990    1000     11.9            11.9 
#> 3 high  absent     1     0       990    1000     28.4            28.4 
#> 4 high  absent     1     0       990    1000     49.4            49.4 
#> 5 high  absent     1     0       990    1000      6.79            6.70
#> 6 high  absent     1     0       990    1000      7.67            7.60
#> # ℹ 650 more variables: mortality_end <dbl>, maturity <dbl>,
#> #   maturity_start <dbl>, maturity_end <dbl>, clutch_1 <dbl>,
#> #   clutch_start_1 <dbl>, clutch_end_1 <dbl>, clutch_size_1 <int>,
#> #   clutch_2 <dbl>, clutch_start_2 <dbl>, clutch_end_2 <dbl>,
#> #   clutch_size_2 <int>, clutch_3 <dbl>, clutch_start_3 <dbl>,
#> #   clutch_end_3 <dbl>, clutch_size_3 <int>, clutch_4 <dbl>,
#> #   clutch_start_4 <dbl>, clutch_end_4 <dbl>, clutch_size_4 <int>, …
```

## Refit the simulated data

The previous section generated one synthetic dataset from the chosen
parameter values. We can fit the same model to that dataset with
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
and compare the refitted estimates with the values used for simulation.

``` r

simulation_config_path <- tempfile(fileext = ".yaml")
yaml::write_yaml(simulation_config, simulation_config_path)

simulated_for_fit <- simulated |>
  mutate(
    sex_start = 0,
    sex_end = pseudo_results$lifelihoodData$right_censoring_date
  )

max_n_clutches <- max(simulated$total_n_clutches, na.rm = TRUE)
clutchs <- generate_clutch_vector(max_n_clutches)

simulated_lifelihood_data <- as_lifelihoodData(
  df = simulated_for_fit,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "maturity_start",
  maturity_end = "maturity_end",
  clutchs = clutchs,
  # `simulate_life_history()` names the mortality event `mortality_start` /
  # `mortality_end`, so point the death columns at those.
  death_start = "mortality_start",
  death_end = "mortality_end",
  covariates = c("par", "spore"),
  dist = c("wei", "wei", "wei"),
  matclutch = FALSE
)

refit <- lifelihood(
  lifelihoodData = simulated_lifelihood_data,
  path_config = simulation_config_path,
  param_bounds_df = pseudo_results$param_bounds_df,
  raise_estimation_warning = FALSE,
  delete_temp_files = FALSE,
  n_fit = 3
)
```

Because this is one finite simulated sample, the refitted estimates are
not expected to be exactly equal to the values used for simulation.

``` r

pseudo_results$effects |>
  select(parameter, name, expected = estimation) |>
  inner_join(
    refit$effects |>
      select(parameter, name, fitted = estimation),
    by = c("parameter", "name")
  ) |>
  mutate(relative_difference = (expected - fitted) / expected) |>
  arrange(parameter, name) |>
  mutate(across(where(is.numeric), \(x) round(x, digits = 3)))
#>             parameter                    name expected fitted
#> 1          expt_death          int_expt_death        0 -0.046
#> 2       expt_maturity       int_expt_maturity        0 -0.092
#> 3   expt_reproduction   int_expt_reproduction        0 -0.063
#> 4     maturity_param2     int_maturity_param2        0  0.084
#> 5         n_offspring         int_n_offspring        0  0.033
#> 6 reproduction_param2 int_reproduction_param2        0  0.105
#> 7     survival_param2     int_survival_param2        0 -0.056
#>   relative_difference
#> 1                 Inf
#> 2                 Inf
#> 3                 Inf
#> 4                -Inf
#> 5                -Inf
#> 6                -Inf
#> 7                 Inf
```

``` r

prediction(pseudo_results, "expt_reproduction", type = "response") |>
  as_tibble() |>
  bind_cols(pseudo_results$lifelihoodData$df) |>
  distinct(par, spore, value)
#> # A tibble: 6 × 3
#>   par   spore   value
#>   <fct> <fct>   <dbl>
#> 1 high  absent   2.50
#> 2 high  present  2.50
#> 3 low   absent   2.50
#> 4 low   present  2.50
#> 5 none  absent   2.50
#> 6 none  present  2.50

prediction(refit, "expt_reproduction", type = "response") |>
  as_tibble() |>
  bind_cols(refit$lifelihoodData$df) |>
  distinct(par, spore, value)
#> # A tibble: 6 × 3
#>   par   spore   value
#>   <fct> <fct>   <dbl>
#> 1 high  absent   2.42
#> 2 high  present  2.42
#> 3 low   absent   2.42
#> 4 low   present  2.42
#> 5 none  absent   2.42
#> 6 none  present  2.42
```

The fitted model also has a regular log-likelihood, AIC, and BIC:

``` r

c(
  logLik = logLik(refit),
  AIC = AIC(refit),
  BIC = BIC(refit)
)
#>    logLik       AIC       BIC 
#> -87464.05 174942.09 174972.87
```

## Common patterns

- Use one row per individual when you want full control of the simulated
  sample.
- Use one row per factor-level combination plus `n_per_combination` when
  you want a balanced or deliberately unbalanced population design.
- Omit `param_bounds_df` for defaults from
  [`default_bounds_df()`](https://nrode.github.io/Lifelihood/reference/default_bounds_df.md).
- Supply `param_bounds_df` when you want tighter or custom parameter
  ranges for a teaching example, sensitivity analysis, or simulation
  study.

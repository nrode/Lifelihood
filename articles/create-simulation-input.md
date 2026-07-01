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

library(lifelihood)
#> Loading required package: tidyverse
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
    expt_death = "par + spore",
    survival_param2 = 1,
    ratio_expt_death = "not_fitted",
    prob_death = "not_fitted",
    sex_ratio = "not_fitted"
  ),
  maturity = list(
    expt_maturity = "par",
    maturity_param2 = 1,
    ratio_expt_maturity = "not_fitted"
  ),
  reproduction = list(
    expt_reproduction = "par + spore",
    reproduction_param2 = 1,
    n_offspring = "par",
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
  expt_death = list(intercept = -2.3, par = c(0.5, -0.4), spore = -0.5),
  survival_param2 = -1.2,
  expt_maturity = list(intercept = -3.2, par = c(-0.3, -0.6)),
  maturity_param2 = -1.2,
  expt_reproduction = list(intercept = -3.5, par = c(0.3, 0.6), spore = -0.25),
  reproduction_param2 = -1,
  n_offspring = list(intercept = -1.5, par = c(0.2, 0.5))
)
```

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

simulation_input <- create_simulation_input(
  effects = effects,
  data = population,
  covariates = c("par", "spore"),
  sex = "sex",
  config = simulation_config,
  dist = c("lgn", "wei", "exp"),
  n_per_combination = "n_individuals"
)

simulation_input$sample_size
#> [1] 600

simulation_input$lifelihoodData$block <- 1
```

> If your `data` already has one row per individual, omit
> `n_per_combination`.

## Simulate life histories

The object returned by
[`create_simulation_input()`](https://nrode.github.io/Lifelihood/reference/create_simulation_input.md)
can be passed directly to
[`simulate_life_history()`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md).

``` r

visits <- data.frame(
  block = 1,
  visit = 1:simulation_input$lifelihoodData$right_censoring_date
)
simulated <- simulate_life_history(simulation_input, seed = 1, visits = visits)

simulated |> head()
#> # A tibble: 6 × 73
#>   par   spore    sex sex_start sex_end total_n_offspring maturity_start
#>   <fct> <fct>  <dbl>     <dbl>   <dbl>             <dbl>          <dbl>
#> 1 high  absent     0       990    1000               176           98.9
#> 2 high  absent     0       990    1000                96           87.1
#> 3 high  absent     0       990    1000               105           68.0
#> 4 high  absent     0       990    1000               158           31.9
#> 5 high  absent     0       990    1000               130          107. 
#> 6 high  absent     0       990    1000               155           33.4
#> # ℹ 66 more variables: maturity_end <dbl>, clutch_start_1 <dbl>,
#> #   clutch_end_1 <dbl>, clutch_size_1 <int>, clutch_start_2 <dbl>,
#> #   clutch_end_2 <dbl>, clutch_size_2 <int>, clutch_start_3 <dbl>,
#> #   clutch_end_3 <dbl>, clutch_size_3 <int>, clutch_start_4 <dbl>,
#> #   clutch_end_4 <dbl>, clutch_size_4 <int>, clutch_start_5 <dbl>,
#> #   clutch_end_5 <dbl>, clutch_size_5 <int>, clutch_start_6 <dbl>,
#> #   clutch_end_6 <dbl>, clutch_size_6 <int>, clutch_start_7 <dbl>, …
```

## Refit the simulated data

The previous section generated one synthetic dataset from the chosen
parameter values. We can fit the same model to that dataset with
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
and compare the refitted estimates with the values used for simulation.

``` r

simulation_config_path <- tempfile(fileext = ".yaml")
yaml::write_yaml(simulation_config, simulation_config_path)

generate_clutch_vector <- function(ids) {
  return(paste(
    "clutch",
    rep(c("start", "end", "size"), length(ids)),
    rep(ids, each = 3),
    sep = "_"
  ))
}

simulated_for_fit <- simulated |>
  mutate(
    sex_start = 0,
    sex_end = simulation_input$lifelihoodData$right_censoring_date
  )

clutch_start_ids <- sub(
  "^clutch_start_",
  "",
  grep("^clutch_start_[0-9]+$", names(simulated_for_fit), value = TRUE)
)
clutch_end_ids <- sub(
  "^clutch_end_",
  "",
  grep("^clutch_end_[0-9]+$", names(simulated_for_fit), value = TRUE)
)
clutch_size_ids <- sub(
  "^clutch_size_",
  "",
  grep("^clutch_size_[0-9]+$", names(simulated_for_fit), value = TRUE)
)
clutch_ids <- Reduce(
  intersect,
  list(clutch_start_ids, clutch_end_ids, clutch_size_ids)
)
clutchs <- generate_clutch_vector(sort(as.integer(clutch_ids)))

simulated_lifelihood_data <- as_lifelihoodData(
  df = simulated_for_fit,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "maturity_start",
  maturity_end = "maturity_end",
  clutchs = clutchs,
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("par", "spore"),
  dist = c("lgn", "wei", "exp"),
  matclutch = FALSE
)

refit <- lifelihood(
  lifelihoodData = simulated_lifelihood_data,
  path_config = simulation_config_path,
  param_bounds_df = simulation_input$param_bounds_df,
  raise_estimation_warning = FALSE,
  delete_temp_files = FALSE,
  n_fit = 3
)
```

Because this is one finite simulated sample, the refitted estimates are
not expected to be exactly equal to the values used for simulation.

``` r

simulation_input$effects |>
  select(parameter, name, simulated = estimation) |>
  inner_join(
    refit$effects |>
      select(parameter, name, refitted = estimation),
    by = c("parameter", "name")
  ) |>
  mutate(difference = refitted - simulated) |>
  arrange(parameter, name) |>
  mutate(across(where(is.numeric), \(x) round(x, digits = 3)))
#>              parameter                                name simulated refitted
#> 1           expt_death              eff_expt_death_par_low      0.50   -0.440
#> 2           expt_death             eff_expt_death_par_none     -0.40   -1.254
#> 3           expt_death        eff_expt_death_spore_present     -0.50    0.556
#> 4           expt_death                      int_expt_death     -2.30    3.000
#> 5        expt_maturity           eff_expt_maturity_par_low     -0.30    0.574
#> 6        expt_maturity          eff_expt_maturity_par_none     -0.60    3.726
#> 7        expt_maturity                   int_expt_maturity     -3.20    2.457
#> 8    expt_reproduction       eff_expt_reproduction_par_low      0.30    0.164
#> 9    expt_reproduction      eff_expt_reproduction_par_none      0.60    2.189
#> 10   expt_reproduction eff_expt_reproduction_spore_present     -0.25   -1.443
#> 11   expt_reproduction               int_expt_reproduction     -3.50   -3.195
#> 12     maturity_param2                 int_maturity_param2     -1.20    0.515
#> 13         n_offspring             eff_n_offspring_par_low      0.20   -1.244
#> 14         n_offspring            eff_n_offspring_par_none      0.50    0.387
#> 15         n_offspring                     int_n_offspring     -1.50   -1.245
#> 16 reproduction_param2             int_reproduction_param2     -1.00   -2.416
#> 17     survival_param2                 int_survival_param2     -1.20   -2.046
#>    difference
#> 1      -0.940
#> 2      -0.854
#> 3       1.056
#> 4       5.300
#> 5       0.874
#> 6       4.326
#> 7       5.657
#> 8      -0.136
#> 9       1.589
#> 10     -1.193
#> 11      0.305
#> 12      1.715
#> 13     -1.444
#> 14     -0.113
#> 15      0.255
#> 16     -1.416
#> 17     -0.846
```

The fitted model also has a regular log-likelihood, AIC, and BIC:

``` r

c(
  logLik = logLik(refit),
  AIC = AIC(refit),
  BIC = BIC(refit)
)
#>    logLik       AIC       BIC 
#> -415202.1  830438.3  830513.0
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

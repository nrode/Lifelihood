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

theme_set(theme_minimal(base_size = 12))
```

## Create the population to simulate

The `data` argument describes the individuals you want to simulate. It
must contain the covariate columns and the sex column. Here, each row is
one combination of factor levels, and `n_individuals` says how many
individuals to create for that combination.

``` r

population <- crossing(
  par = factor(c("none", "low", "high"), levels = c("none", "low", "high")),
  spore = factor(c("absent", "present"), levels = c("absent", "present")),
  sex = c(0, 1)
) |>
  mutate(
    n_individuals = case_when(
      sex == 1 ~ 25L,
      spore == "present" ~ 35L,
      TRUE ~ 30L
    ),
    sex_label = if_else(sex == 1, "male", "female")
  )

population
#> # A tibble: 12 × 5
#>    par   spore     sex n_individuals sex_label
#>    <fct> <fct>   <dbl>         <int> <chr>    
#>  1 none  absent      0            30 female   
#>  2 none  absent      1            25 male     
#>  3 none  present     0            35 female   
#>  4 none  present     1            25 male     
#>  5 low   absent      0            30 female   
#>  6 low   absent      1            25 male     
#>  7 low   present     0            35 female   
#>  8 low   present     1            25 male     
#>  9 high  absent      0            30 female   
#> 10 high  absent      1            25 male     
#> 11 high  present     0            35 female   
#> 12 high  present     1            25 male
```

If your `data` already has one row per individual, omit
`n_per_combination`.

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
  model = "lgn",
  n_per_combination = "n_individuals"
)

simulation_input$sample_size
#> [1] 345
```

## Simulate life histories

The object returned by
[`create_simulation_input()`](https://nrode.github.io/Lifelihood/reference/create_simulation_input.md)
can be passed directly to
[`simulate_life_history()`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md).

``` r

simulated <- simulate_life_history(simulation_input, seed = 1)

simulated |>
  select(
    death_start,
    death_end,
    maturity_start,
    maturity_end,
    total_n_offspring
  ) |>
  head()
#> # A tibble: 6 × 5
#>   death_start death_end maturity_start maturity_end total_n_offspring
#>         <dbl>     <dbl>          <dbl>        <dbl>             <dbl>
#> 1        90.9      90.9           38.2         38.2               349
#> 2        90.2      90.2           39.4         39.4               416
#> 3        90.6      90.6           37.9         37.9               377
#> 4        94.4      94.4           41.6         41.6               329
#> 5        90.2      90.2           39.6         39.6               356
#> 6        93.0      93.0           37.9         37.9               313
```

## Refit the simulated data

The previous section generated one synthetic dataset from the chosen
parameter values. We can fit the same model to that dataset with
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
and compare the refitted estimates with the values used for simulation.

``` r

simulation_config_path <- tempfile(fileext = ".yaml")
yaml::write_yaml(simulation_config, simulation_config_path)

clutch_numbers <- simulated |>
  select(matches("^clutch_start_[0-9]+$")) |>
  names() |>
  str_extract("[0-9]+$") |>
  as.integer() |>
  sort()

simulated_clutchs <- as.vector(rbind(
  paste0("clutch_start_", clutch_numbers),
  paste0("clutch_end_", clutch_numbers),
  paste0("clutch_size_", clutch_numbers)
))

simulated_for_fit <- bind_cols(
  simulation_input$lifelihoodData$df |>
    select(par, spore, sex, .simulation_sex_start, .simulation_sex_end),
  simulated
) |>
  mutate(
    .simulation_sex_start = 0,
    .simulation_sex_end = simulation_input$lifelihoodData$right_censoring_date
  )

simulated_lifelihood_data <- as_lifelihoodData(
  df = simulated_for_fit,
  sex = "sex",
  sex_start = ".simulation_sex_start",
  sex_end = ".simulation_sex_end",
  maturity_start = "maturity_start",
  maturity_end = "maturity_end",
  clutchs = simulated_clutchs,
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("par", "spore"),
  model_specs = rep("lgn", 3),
  matclutch = FALSE
)

refit <- lifelihood(
  lifelihoodData = simulated_lifelihood_data,
  path_config = simulation_config_path,
  param_bounds_df = simulation_input$param_bounds_df,
  seeds = c(1, 2, 3, 4),
  raise_estimation_warning = FALSE
)
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos-aarch64 /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1_2_3_4/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1_2_3_4/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 1 2 3 4 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
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
#> 1           expt_death             eff_expt_death_par_high     -0.40    0.000
#> 2           expt_death              eff_expt_death_par_low      0.50    0.000
#> 3           expt_death        eff_expt_death_spore_present     -0.50    0.000
#> 4           expt_death                      int_expt_death     -2.30   -0.370
#> 5        expt_maturity          eff_expt_maturity_par_high     -0.60    0.000
#> 6        expt_maturity           eff_expt_maturity_par_low     -0.30    0.000
#> 7        expt_maturity                   int_expt_maturity     -3.20   -1.960
#> 8    expt_reproduction      eff_expt_reproduction_par_high      0.60    0.000
#> 9    expt_reproduction       eff_expt_reproduction_par_low      0.30    0.000
#> 10   expt_reproduction eff_expt_reproduction_spore_present     -0.25    0.000
#> 11   expt_reproduction               int_expt_reproduction     -3.50   -3.036
#> 12     maturity_param2                 int_maturity_param2     -1.20   -3.450
#> 13         n_offspring            eff_n_offspring_par_high      0.50    0.000
#> 14         n_offspring             eff_n_offspring_par_low      0.20    0.000
#> 15         n_offspring                     int_n_offspring     -1.50   -2.600
#> 16 reproduction_param2             int_reproduction_param2     -1.00   -3.350
#> 17     survival_param2                 int_survival_param2     -1.20   -2.230
#>    difference
#> 1       0.400
#> 2      -0.500
#> 3       0.500
#> 4       1.930
#> 5       0.600
#> 6       0.300
#> 7       1.240
#> 8      -0.600
#> 9      -0.300
#> 10      0.250
#> 11      0.464
#> 12     -2.250
#> 13     -0.500
#> 14     -0.200
#> 15     -1.100
#> 16     -2.350
#> 17     -1.030
```

The fitted model also has a regular log-likelihood, AIC, and BIC:

``` r

c(
  logLik = logLik(refit),
  AIC = AIC(refit),
  BIC = BIC(refit)
)
#>    logLik       AIC       BIC 
#> -178185.5  356405.1  356470.4
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

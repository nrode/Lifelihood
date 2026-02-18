# Simulation and visit masks

`lifelihood` offers a
[`lifelihood::simulate_life_history()`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md)
function that lets you simulate new observations based on estimates you
made.

## Fitting

First, we need to fit the model with
[`lifelihood::lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md):

``` r

library(lifelihood)
#> Loading required package: tidyverse
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.2.0     ✔ readr     2.1.6
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
#> ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
#> ✔ purrr     1.2.1     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
library(tidyverse)

df <- datapierrick |>
  mutate(
    par = as.factor(par),
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
  block = "block",
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("par", "spore"),
  model_specs = c("wei", "lgn", "wei")
)

results <- lifelihood(
  lifelihoodData,
  path_config = use_test_config("config_pierrick")
)
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_7999_6246_1294_6247/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_7999_6246_1294_6247/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 7999 6246 1294 6247 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"

summary(results)
#> 
#> === LIFELIHOOD RESULTS ===
#> 
#> Sample size: 550 
#> 
#> --- Model Fit ---
#> Log-likelihood:  -32234.438
#> AIC:             64516.9
#> BIC:             64620.3
#> 
#> --- Key Parameters ---
#> 
#> Mortality:
#>   expt_death (Intercept)    -0.909 (0.000)
#>   expt_death eff_expt_death_par_1 -0.488 (0.000)
#>   expt_death eff_expt_death_par_2 -0.535 (0.000)
#>   expt_death eff_expt_death_spore_1 -0.492 (0.000)
#>   expt_death eff_expt_death_spore_2 -0.101 (0.000)
#>   expt_death eff_expt_death_spore_3 0.128 (0.000)
#>   expt_death eff_expt_death_par_1:spore_1 0.151 (0.000)
#>   expt_death eff_expt_death_par_2:spore_1 -0.370 (0.000)
#>   expt_death eff_expt_death_par_1:spore_2 -0.165 (0.000)
#>   expt_death eff_expt_death_par_2:spore_2 -3.428 (0.000)
#>   expt_death eff_expt_death_par_1:spore_3 2.832 (0.000)
#>   expt_death eff_expt_death_par_2:spore_3 -0.403 (0.000)
#>   survival_param2 (Intercept) -4.877 (0.000)
#> 
#> Maturity:
#>   expt_maturity (Intercept) -1.520 (0.000)
#>   expt_maturity eff_expt_maturity_par_1 0.149 (0.000)
#>   expt_maturity eff_expt_maturity_par_2 0.077 (0.000)
#>   maturity_param2 (Intercept) -7.407 (0.000)
#> 
#> Reproduction:
#>   expt_reproduction (Intercept) -1.789 (0.000)
#>   expt_reproduction eff_expt_reproduction_par_1 -1.694 (0.000)
#>   expt_reproduction eff_expt_reproduction_par_2 -1.105 (0.000)
#>   reproduction_param2 (Intercept) -1.123 (0.000)
#>   reproduction_param2 eff_reproduction_param2_par_1 -0.439 (0.000)
#>   reproduction_param2 eff_reproduction_param2_par_2 -0.800 (0.000)
#>   n_offspring (Intercept)   -2.544 (0.000)
#> 
#> --- Convergence ---
#> All parameters within bounds
#> 
#> ======================
```

## Default simulations

By default, `lifelihood` will simulate all life history events
(maturity, reproduction, and death):

``` r

simulate_life_history(results) |> head()
#> # A tibble: 6 × 143
#>   mortality maturity clutch_1 n_offspring_clutch_1 clutch_2 n_offspring_clutch_2
#>       <dbl>    <dbl>    <dbl>                <int>    <dbl>                <int>
#> 1      46.8     11.5     14.4                    1     22.4                    4
#> 2     119.      12.4     16.7                    3     20.2                    7
#> 3     109.      12.2     18.8                    2     28.2                    3
#> 4     102.      12.4     20.1                    4     22.5                    7
#> 5      83.6     14.0     19.2                    3     24.0                    4
#> 6      69.9     13.1     20.4                    5     24.1                    1
#> # ℹ 137 more variables: clutch_3 <dbl>, n_offspring_clutch_3 <int>,
#> #   clutch_4 <dbl>, n_offspring_clutch_4 <int>, clutch_5 <dbl>,
#> #   n_offspring_clutch_5 <int>, clutch_6 <dbl>, n_offspring_clutch_6 <int>,
#> #   clutch_7 <dbl>, n_offspring_clutch_7 <int>, clutch_8 <dbl>,
#> #   n_offspring_clutch_8 <int>, clutch_9 <dbl>, n_offspring_clutch_9 <int>,
#> #   clutch_10 <dbl>, n_offspring_clutch_10 <int>, clutch_11 <dbl>,
#> #   n_offspring_clutch_11 <int>, clutch_12 <dbl>, …
```

But you can specify which event you want:

``` r

simulate_life_history(results, event = "maturity") |> head()
#> # A tibble: 6 × 1
#>   maturity
#>      <dbl>
#> 1     13.0
#> 2     13.4
#> 3     12.4
#> 4     12.1
#> 5     12.5
#> 6     12.0
```

## Simulations with visit masks

`lifelihood` lets you specify visit masks that are used to simulate data
that more closely reflects how the original data was measured by adding
constraints to the interval dates.

If you scroll to the top, you’ll see that we passed a column name from
our dataframe to the `block` argument. This column represents the block
to which each individual belongs. Since we provided this value, we just
need to add `use_censoring = TRUE` to include censoring time intervals
in the simulation:

``` r

results |>
  simulate_life_history(event = "maturity", use_censoring = TRUE) |>
  head()
#> # A tibble: 6 × 3
#>   maturity maturity_start maturity_end
#>      <dbl>          <int>        <int>
#> 1     12.9             12           13
#> 2     12.6             12           13
#> 3     13.5             13           14
#> 4     12.9             12           13
#> 5     12.5             12           13
#> 6     12.8             12           13
```

Right now, visit masks are “deduced” from the original dataset, but you
can provide your own visit masks with the `visits` argument.

It must be a dataframe with 2 columns: `block` (the same name as passed
to
[`lifelihood::as_lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md)
in the `block` argument) and exactly `visit`. For each block, `visit`
corresponds to the ages at which the events of individuals were
recorded.

Let’s define this visits dataframe:

``` r

visits <- tibble(
  block = rep(1:2, nrow(datapierrick) / 2),
  visit = 1:nrow(datapierrick)
)
visits |> head()
#> # A tibble: 6 × 2
#>   block visit
#>   <int> <int>
#> 1     1     1
#> 2     2     2
#> 3     1     3
#> 4     2     4
#> 5     1     5
#> 6     2     6
```

Now we can pass this to the
[`simulate_life_history()`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md)
function:

``` r

results |>
  simulate_life_history(
    event = "maturity",
    use_censoring = TRUE,
    visits = visits
  ) |>
  head()
#> # A tibble: 6 × 3
#>   maturity maturity_start maturity_end
#>      <dbl>          <int>        <int>
#> 1     11.6             11           13
#> 2     13.0             11           13
#> 3     12.9             11           13
#> 4     13.0             11           13
#> 5     13.1             13           15
#> 6     13.4             13           15
```

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
#> ✔ dplyr     1.2.0     ✔ readr     2.2.0
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
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_4212_9376_8900_6300/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_4212_9376_8900_6300/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 4212 9376 8900 6300 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"

summary(results)
#> 
#> === LIFELIHOOD RESULTS ===
#> 
#> Sample size: 550 
#> 
#> --- Model Fit ---
#> Log-likelihood:  -32231.951
#> AIC:             64511.9
#> BIC:             64615.3
#> 
#> --- Key Parameters ---
#> 
#> Mortality:
#>   expt_death (Intercept)    -0.897 (0.000)
#>   expt_death eff_expt_death_par_1 -0.161 (0.000)
#>   expt_death eff_expt_death_par_2 0.297 (0.000)
#>   expt_death eff_expt_death_spore_1 -0.301 (0.000)
#>   expt_death eff_expt_death_spore_2 -0.056 (0.000)
#>   expt_death eff_expt_death_spore_3 -0.881 (0.000)
#>   expt_death eff_expt_death_par_1:spore_1 -0.380 (0.000)
#>   expt_death eff_expt_death_par_2:spore_1 1.258 (0.000)
#>   expt_death eff_expt_death_par_1:spore_2 -0.549 (0.000)
#>   expt_death eff_expt_death_par_2:spore_2 -0.862 (0.000)
#>   expt_death eff_expt_death_par_1:spore_3 -1.212 (0.000)
#>   expt_death eff_expt_death_par_2:spore_3 -0.241 (0.000)
#>   survival_param2 (Intercept) -4.868 (0.000)
#> 
#> Maturity:
#>   expt_maturity (Intercept) -1.507 (0.000)
#>   expt_maturity eff_expt_maturity_par_1 0.204 (0.000)
#>   expt_maturity eff_expt_maturity_par_2 0.075 (0.000)
#>   maturity_param2 (Intercept) -7.441 (0.000)
#> 
#> Reproduction:
#>   expt_reproduction (Intercept) -1.790 (0.000)
#>   expt_reproduction eff_expt_reproduction_par_1 -1.692 (0.000)
#>   expt_reproduction eff_expt_reproduction_par_2 -1.091 (0.000)
#>   reproduction_param2 (Intercept) -1.124 (0.000)
#>   reproduction_param2 eff_reproduction_param2_par_1 -0.438 (0.000)
#>   reproduction_param2 eff_reproduction_param2_par_2 -0.798 (0.000)
#>   n_offspring (Intercept)   -2.545 (0.000)
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
#> # A tibble: 6 × 137
#>   mortality maturity clutch_1 n_offspring_clutch_1 clutch_2 n_offspring_clutch_2
#>       <dbl>    <dbl>    <dbl>                <int>    <dbl>                <int>
#> 1      51.9     13.0     16.9                    1     18.9                    4
#> 2      74.2     12.3     16.6                    6     22.9                    4
#> 3      80.2     12.7     15.5                    5     22.9                    6
#> 4      94.1     13.6     17.2                    4     19.6                    7
#> 5     144.      13.6     15.8                    4     19.1                    9
#> 6     130.      12.8     19.1                    3     25.9                    8
#> # ℹ 131 more variables: clutch_3 <dbl>, n_offspring_clutch_3 <int>,
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
#> 1     12.9
#> 2     13.3
#> 3     12.2
#> 4     12.8
#> 5     12.7
#> 6     12.8
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
#> 1     11.6             11           12
#> 2     12.8             12           13
#> 3     12.7             12           13
#> 4     14.0             13           14
#> 5     12.5             12           13
#> 6     11.1             11           12
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
#> 1     12.3             11           13
#> 2     12.9             11           13
#> 3     13.4             13           15
#> 4     11.8             11           13
#> 5     13.6             13           15
#> 6     13.5             13           15
```

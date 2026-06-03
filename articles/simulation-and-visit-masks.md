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

df <- datapierrick |>
  mutate(
    par = as.factor(par),
    spore = as.factor(spore),
    block = rep(1:2, each = nrow(datapierrick) / 2)
  )

generate_clutch_vector <- function(N) {
  return(paste(
    "clutch",
    rep(c("start", "end", "size"), N),
    rep(1:N, each = 3),
    sep = "_"
  ))
}
clutchs <- generate_clutch_vector(28)

lifelihoodData <- as_lifelihoodData(
  df = df,
  matclutch = FALSE,
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
  dist = c("wei", "lgn", "wei")
)

results <- lifelihood(
  lifelihoodData,
  path_config = use_test_config("config_pierrick")
)
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos-aarch64 /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1397_5758_4327_7914/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1397_5758_4327_7914/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 1397 5758 4327 7914 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"

summary(results)
#> 
#> === LIFELIHOOD RESULTS ===
#> 
#> Sample size: 550 
#> 
#> --- Model Fit ---
#> Log-likelihood:  -32471.089
#> AIC:             64962.2
#> BIC:             65005.3
#> 
#> --- Key Parameters ---
#> 
#> Mortality:
#>   expt_death (Intercept)    -0.912 (0.000)
#>   expt_death eff_expt_death_par_1 -2.373 (0.000)
#>   expt_death eff_expt_death_par_2 -2.394 (0.000)
#>   survival_param2 (Intercept) -4.880 (0.000)
#>   ratio_expt_death (Intercept) -3.108 (0.000)
#> 
#> Maturity:
#>   expt_maturity (Intercept) -1.481 (0.000)
#>   maturity_param2 (Intercept) -7.348 (0.000)
#> 
#> Reproduction:
#>   expt_reproduction (Intercept) -1.805 (0.000)
#>   reproduction_param2 (Intercept) -1.151 (0.000)
#>   n_offspring (Intercept)   -2.553 (0.000)
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
#> # A tibble: 6 × 110
#>   death_start death_end total_n_offspring maturity_start maturity_end
#>         <dbl>     <dbl>             <dbl>          <dbl>        <dbl>
#> 1        93.2      93.2                78           11.9         11.9
#> 2       117.      117.                106           12.4         12.4
#> 3        27.0      27.0                21           11.8         11.8
#> 4       101.      101.                 68           12.8         12.8
#> 5        85.8      85.8                75           13.2         13.2
#> 6        93.2      93.2                74           13.9         13.9
#> # ℹ 105 more variables: clutch_size_1 <int>, clutch_size_2 <int>,
#> #   clutch_size_3 <int>, clutch_size_4 <int>, clutch_size_5 <int>,
#> #   clutch_size_6 <int>, clutch_size_7 <int>, clutch_size_8 <int>,
#> #   clutch_size_9 <int>, clutch_size_10 <int>, clutch_size_11 <int>,
#> #   clutch_size_12 <int>, clutch_size_13 <int>, clutch_size_14 <int>,
#> #   clutch_size_15 <int>, clutch_size_16 <int>, clutch_size_17 <int>,
#> #   clutch_size_18 <int>, clutch_size_19 <int>, clutch_size_20 <int>, …
```

But you can specify which event you want:

``` r

simulate_life_history(results, event = "maturity") |> head()
#> # A tibble: 6 × 2
#>   maturity_start maturity_end
#>            <dbl>        <dbl>
#> 1           12.6         12.6
#> 2           12.3         12.3
#> 3           13.3         13.3
#> 4           13.2         13.2
#> 5           13.7         13.7
#> 6           13.1         13.1
```

## Simulations with visit masks

`lifelihood` lets you specify visit masks that are used to simulate data
that more closely reflects how the original data was measured by adding
constraints to the interval dates.

If you scroll to the top, you’ll see that we passed a column name from
our dataframe to the `block` argument. This column represents the block
to which each individual belongs. Use
[`get_visits()`](https://nrode.github.io/Lifelihood/reference/get_visits.md)
to retrieve the visit masks inferred from the original dataset:

``` r

visits <- get_visits(lifelihoodData)
visits |> head()
#> # A tibble: 6 × 2
#>   block visit
#>   <int> <int>
#> 1     1     0
#> 2     1     7
#> 3     1     8
#> 4     1     9
#> 5     1    10
#> 6     1    11
```

Then pass these visits explicitly with `use_censoring = TRUE` to include
censoring time intervals in the simulation:

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
#>      <dbl>          <dbl>        <dbl>
#> 1     13.2             13           14
#> 2     13.7             13           14
#> 3     12.5             12           13
#> 4     11.8             11           12
#> 5     13.5             13           14
#> 6     12.9             12           13
```

You can also provide your own visit masks with the `visits` argument. It
must be a dataframe with 2 columns: `block` (the same name as passed to
[`lifelihood::as_lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md)
in the `block` argument) and exactly `visit`. For each block, `visit`
corresponds to the ages at which the events of individuals were
recorded.

Let’s define a custom visits dataframe:

``` r

custom_visits <- tibble(
  block = rep(1:2, nrow(datapierrick) / 2),
  visit = seq_len(nrow(datapierrick))
)
custom_visits |> head()
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
    visits = custom_visits
  ) |>
  head()
#> # A tibble: 6 × 3
#>   maturity maturity_start maturity_end
#>      <dbl>          <dbl>        <dbl>
#> 1     12.9             11           13
#> 2     13.2             13           15
#> 3     13.7             13           15
#> 4     14.2             13           15
#> 5     13.3             13           15
#> 6     11.5             11           13
```

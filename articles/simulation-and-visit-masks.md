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
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos-aarch64 /Users/runner/work/Lifelihood/Lifelihood/lifelihood_53_4772_3768_1124/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_53_4772_3768_1124/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 53 4772 3768 1124 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"

summary(results)
#> 
#> === LIFELIHOOD RESULTS ===
#> 
#> Sample size: 550 
#> 
#> --- Model Fit ---
#> Log-likelihood:  -75326.278
#> AIC:             150672.6
#> BIC:             150715.7
#> 
#> --- Key Parameters ---
#> 
#> Mortality:
#>   expt_death (Intercept)    -0.928 (0.000)
#>   expt_death eff_expt_death_par_1 -2.807 (0.000)
#>   expt_death eff_expt_death_par_2 -2.802 (0.000)
#>   survival_param2 (Intercept) -4.897 (0.000)
#>   ratio_expt_death (Intercept) -2.658 (0.000)
#> 
#> Maturity:
#>   expt_maturity (Intercept) -2.264 (0.000)
#>   maturity_param2 (Intercept) -1.439 (0.000)
#> 
#> Reproduction:
#>   expt_reproduction (Intercept) -1.810 (0.000)
#>   reproduction_param2 (Intercept) -1.157 (0.000)
#>   n_offspring (Intercept)   -2.550 (0.000)
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
#> # A tibble: 6 × 119
#>   death_start death_end total_n_offspring maturity_start maturity_end
#>         <dbl>     <dbl>             <dbl>          <dbl>        <dbl>
#> 1        99.6      99.6                85          7.86         7.86 
#> 2        88.4      88.4                90          2.04         2.04 
#> 3        85.4      85.4                81          1.03         1.03 
#> 4       110.      110.                 97          0.461        0.461
#> 5        67.6      67.6                59          8.89         8.89 
#> 6        79.9      79.9               105          5.76         5.76 
#> # ℹ 114 more variables: clutch_size_1 <int>, clutch_size_2 <int>,
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
#> 1           4.64         4.64
#> 2           2.24         2.24
#> 3           8.86         8.86
#> 4          14.6         14.6 
#> 5           9.31         9.31
#> 6           2.67         2.67
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
#> 1     3.58              0            7
#> 2     4.83              0            7
#> 3     2.32              0            7
#> 4     2.60              0            7
#> 5     2.73              0            7
#> 6     3.82              0            7
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
#> 1     1.35              1            3
#> 2     3.15              3            5
#> 3     1.34              1            3
#> 4    18.6              17           19
#> 5    60.5              59           61
#> 6     2.37              1            3
```

# Standard errors and MCMC

## Create a `lifelihoodData` object

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
  as_tibble() |>
  mutate(
    par = as.factor(par),
    geno = as.factor(geno),
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
  dist = c("wei", "gam", "lgn")
)
```

## Standard errors

By default, lifelihood will not try to fit standard errors. But, you can
use the `se.fit` argument for this purpose:

``` r

results <- lifelihood(
  lifelihoodData = lifelihoodData,
  path_config = use_test_config("example_config_se"),
  se.fit = TRUE,
)
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos-aarch64 /Users/runner/work/Lifelihood/Lifelihood/lifelihood_4882_1037_7197_4670/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_4882_1037_7197_4670/temp_param_range_path.txt 0 25 TRUE 0 FALSE 0 4882 1037 7197 4670 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
summary(results)
#> 
#> === LIFELIHOOD RESULTS ===
#> 
#> Sample size: 550 
#> 
#> --- Model Fit ---
#> Log-likelihood:  -343816.293
#> AIC:             687640.6
#> BIC:             687657.8
#> 
#> --- Key Parameters ---
#> 
#> Mortality:
#>   expt_death (Intercept)    -1.382 (-1.000)
#>   expt_death eff_expt_death_par_1 -0.324 (-1.000)
#>   expt_death eff_expt_death_par_2 -0.336 (-1.000)
#>   survival_param2 (Intercept) -0.490 (-1.000)
#> 
#> --- Convergence ---
#> All parameters within bounds
#> 
#> ======================
```

Now if we have a look at the estimations we have standard errors:

``` r

results$effects |> as_tibble()
#> # A tibble: 4 × 6
#>   name                 estimation stderror parameter       kind            event
#>   <chr>                     <dbl>    <dbl> <chr>           <chr>           <chr>
#> 1 int_expt_death           -1.38        -1 expt_death      intercept       mort…
#> 2 eff_expt_death_par_1     -0.324       -1 expt_death      coefficient_ca… mort…
#> 3 eff_expt_death_par_2     -0.336       -1 expt_death      coefficient_ca… mort…
#> 4 int_survival_param2      -0.490       -1 survival_param2 intercept       mort…
```

### Prediction

We can predict with standard errors.

- Default scale

``` r

prediction(results, "expt_death", se.fit = TRUE) |>
  as_tibble() |>
  sample_n(5)
#> Lifelihood parameter estimate(s) for males are identical to that of females. Use type='response', to get the right parameter estimate(s) for males on the response scale.
#> Warning in sqrt(var_cov_fitted_predictors): NaNs produced
#> # A tibble: 5 × 2
#>   fitted se.fitted
#>    <dbl>     <dbl>
#> 1  -1.72    0.0483
#> 2  -1.71    0.0303
#> 3  -1.38  NaN     
#> 4  -1.38  NaN     
#> 5  -1.72    0.0483
```

- Response scale

``` r

prediction(results, "expt_death", type = "response", se.fit = TRUE) |>
  as_tibble() |>
  sample_n(5)
#> Warning in sqrt(var_fitted_predictors): NaNs produced
#> # A tibble: 5 × 2
#>   fitted se.fitted
#>    <dbl>     <dbl>
#> 1   65.0    NaN   
#> 2   65.0    NaN   
#> 3   65.0    NaN   
#> 4   49.8      1.28
#> 5   65.0    NaN
```

## MCMC

> MCMC stands for **M**arkov **C**hain **M**onte **C**arlo.

### Fitting with MCMC

``` r

results <- lifelihood(
  lifelihoodData = lifelihoodData,
  path_config = use_test_config("example_config_mcmc"),
  MCMC = 30
)
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos-aarch64 /Users/runner/work/Lifelihood/Lifelihood/lifelihood_8107_5484_4855_5092/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_8107_5484_4855_5092/temp_param_range_path.txt 30 25 FALSE 0 TRUE 0 8107 5484 4855 5092 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
```

### Visualization

We can represent the confidence interval computed thanks to the standard
errors:

``` r

plot_fitted_event_rate(
  results,
  interval_width = 10,
  event = "mortality",
  use_facet = TRUE,
  groupby = "par",
  xlab = "Age (days)",
  ylab = "Fitted Mortality Rate",
  se.fit = TRUE
)
#> Warning in value[[3L]](cond): Could not compute MCMC standard errors. Number of
#> MCMC iterations (30) must be higher than number of individuals (48).
#> Warning in value[[3L]](cond): Could not compute MCMC standard errors. Number of
#> MCMC iterations (30) must be higher than number of individuals (48).
#> Warning: Removed 20 rows containing missing values or values outside the scale range
#> (`geom_point()`).
```

![](prediction-with-standard-errors-and-mcmc_files/figure-html/unnamed-chunk-6-1.png)

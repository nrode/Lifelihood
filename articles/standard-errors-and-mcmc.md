# Standard errors and MCMC

## Create a `lifelihoodData` object

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
  block = "block",
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("par", "spore"),
  model_specs = c("wei", "gam", "lgn")
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
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_/temp_param_range_path.txt FALSE 0 25 TRUE 0 FALSE 0 5537 7354 3389 3319 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
summary(results)
#> 
#> === LIFELIHOOD RESULTS ===
#> 
#> Sample size: 550 
#> 
#> --- Model Fit ---
#> Log-likelihood:  -343787.940
#> AIC:             687583.9
#> BIC:             687601.1
#> 
#> --- Key Parameters ---
#> 
#> Mortality:
#>   expt_death (Intercept)    -1.879 (0.091)
#>   expt_death eff_expt_death_par_1 0.199 (0.098)
#>   expt_death eff_expt_death_par_2 0.188 (0.103)
#>   survival_param2 (Intercept) -0.242 (0.119)
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
#> 1 int_expt_death           -1.88    0.0910 expt_death      intercept       mort…
#> 2 eff_expt_death_par_1      0.199   0.0985 expt_death      coefficient_ca… mort…
#> 3 eff_expt_death_par_2      0.188   0.103  expt_death      coefficient_ca… mort…
#> 4 int_survival_param2      -0.242   0.119  survival_param2 intercept       mort…
```

### Prediction

We can predict with standard errors.

- Default scale

``` r

prediction(results, "expt_death", se.fit = TRUE) |>
  as_tibble() |>
  sample_n(5)
#> # A tibble: 5 × 2
#>   fitted se.fitted
#>    <dbl>     <dbl>
#> 1  -1.88    0.0910
#> 2  -1.88    0.0910
#> 3  -1.88    0.0910
#> 4  -1.88    0.0910
#> 5  -1.68    0.0298
```

- Response scale

``` r

prediction(results, "expt_death", type = "response", se.fit = TRUE) |>
  as_tibble() |>
  sample_n(5)
#> # A tibble: 5 × 2
#>   fitted se.fitted
#>    <dbl>     <dbl>
#> 1   42.9      3.39
#> 2   42.9      3.39
#> 3   42.9      3.39
#> 4   42.9      3.39
#> 5   50.4      1.86
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
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_/temp_param_range_path.txt FALSE 30 25 FALSE 0 TRUE 0 3357 1791 2237 7163 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
#> Warning in check_estimation(results): Estimation of 'fitness' is close to the
#> maximum bound: fitness~=999.999916215974. Consider increasing maximum bound.
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
#> Warning: Removed 20 rows containing missing values or values outside the scale range
#> (`geom_point()`).
```

![](standard-errors-and-mcmc_files/figure-html/unnamed-chunk-6-1.png)

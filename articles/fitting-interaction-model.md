# Fit interaction model using `group_by_group = TRUE`

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
    spore = as.factor(spore)
  )

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
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("par", "geno"),
  dist = c("wei", "gam", "lgn")
)

set.seed(42)
```

## Difference between default and interaction models

- Default model

``` r

time_default <- system.time({
  results_default <- lifelihood(
    lifelihoodData,
    path_config = use_test_config("config_gbg"),
    group_by_group = FALSE # Default value
  )
})
time_default
#>    user  system elapsed 
#>  19.374   0.159  19.895
```

- Interaction model using the `group_by_group` argument (default to
  `FALSE`)

``` r

time_gbg <- system.time({
  results_gbg <- lifelihood(
    lifelihoodData,
    path_config = use_test_config("config_gbg"),
    group_by_group = TRUE
  )
})
time_gbg
#>    user  system elapsed 
#>   1.368   0.067   1.454
```

Fitting interaction model with group by group is faster than default
model.

- Comparison

``` r

results_default$likelihood
#> [1] -343782.3
results_gbg$likelihood
#> [1] -343766.5
```

Log-likelihood is also higher with group by group indicating better
convergence towards the maximum log-likelihood value.

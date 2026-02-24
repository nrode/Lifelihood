# How to use lifelihood

## Load libraries

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
```

## Fit a simple model

``` r

df <- datapierrick |>
  as_tibble() |>
  mutate(
    par = as.factor(par),
    geno = as.factor(geno),
    spore = as.factor(spore)
  ) |>
  sample_n(120)

generate_clutch_vector <- function(N) {
  return(paste(
    "pon",
    rep(c("start", "end", "size"), N),
    rep(1:N, each = 3),
    sep = "_"
  ))
}

lifelihoodData <- as_lifelihoodData(
  df = df,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "mat_start",
  maturity_end = "mat_end",
  clutchs = generate_clutch_vector(28),
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("par", "spore"),
  model_specs = c("wei", "gam", "exp")
)

results <- lifelihood(
  lifelihoodData,
  path_config = use_test_config("config_pierrick"),
  raise_estimation_warning = FALSE
)
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1260_9225_707_692/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1260_9225_707_692/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 1260 9225 707 692 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
```

## Goodness of fit

The goodness of fit simulate datasets from a fitted model (`results)`,
refit the model on each simulated dataset (`nsim`), and compare
simulated log-likelihood values to the original fit.

``` r

gof <- goodness_of_fit(results, nsim = 5)
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_4291_7701_6818_1871/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_4291_7701_6818_1871/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 4291 7701 6818 1871 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1808_4217_1150_6501/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1808_4217_1150_6501/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 1808 4217 1150 6501 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1134_898_8315_9939/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1134_898_8315_9939/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 1134 898 8315 9939 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_8725_3633_3840_3782/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_8725_3633_3840_3782/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 8725 3633 3840 3782 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_4794_9411_39_7693/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_4794_9411_39_7693/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 4794 9411 39 7693 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
```

The
[`goodness_of_fit()`](https://nrode.github.io/Lifelihood/reference/goodness_of_fit.md)
function returns an instance of class `lifelihoodGOF`, with the
following attributes:

``` r

gof$original_loglik
#> [1] -7311.691
gof$simulated_loglik
#> [1] -10637.93  -9687.10 -10654.79 -10156.76 -10164.93
gof$n_success
#> [1] 5
gof$n_failed
#> [1] 0
gof$p_lower_or_equal
#> [1] 1
```

You can also read the `gof$fits` attribute for all underlying fits. Use
`gof$fits[[1]]` for the first one, `gof$fits[[2]]` for the second, and
so on.

## Visualization

You can use the [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
S3 method on the output of
[`goodness_of_fit()`](https://nrode.github.io/Lifelihood/reference/goodness_of_fit.md):

``` r

plot(gof)
```

![](goodness-of-fit_files/figure-html/unnamed-chunk-5-1.png)

We can see here that the simulated datasets, when fitted, have a less
good log-likelihood compared to the original fit. This might suggest
that the original fit isn’t that great.

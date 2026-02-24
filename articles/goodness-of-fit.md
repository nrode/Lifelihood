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
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_8191_4083_5001_2676/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_8191_4083_5001_2676/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 8191 4083 5001 2676 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
```

## Goodness of fit

The goodness of fit simulate datasets from a fitted model (`results)`,
refit the model on each simulated dataset (`nsim`), and compare
simulated log-likelihood values to the original fit.

``` r

gof <- goodness_of_fit(results, nsim = 5)
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9422_5314_3139_9412/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9422_5314_3139_9412/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 9422 5314 3139 9412 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_4946_7313_6923_9595/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_4946_7313_6923_9595/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 4946 7313 6923 9595 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_287_2659_4601_7183/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_287_2659_4601_7183/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 287 2659 4601 7183 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_4542_7742_9938_5547/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_4542_7742_9938_5547/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 4542 7742 9938 5547 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9533_1017_3989_2173/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9533_1017_3989_2173/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 9533 1017 3989 2173 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
```

The
[`goodness_of_fit()`](https://nrode.github.io/Lifelihood/reference/goodness_of_fit.md)
function returns an instance of class `lifelihoodGOF`, with the
following attributes:

``` r

gof$original_loglik
#> [1] -7546.43
gof$simulated_loglik
#> [1] -10218.68 -10544.18 -10601.28 -10924.67 -11428.25
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

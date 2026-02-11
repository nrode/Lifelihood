# Fit saturated model using `group_by_group = TRUE`

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
    spore = as.factor(spore)
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
  covariates = c("par", "geno"),
  model_specs = c("wei", "gam", "lgn")
)

set.seed(42)
```

## Difference between default and saturated models

- Default model

``` r

system.time(
  results_default <- lifelihood(
    lifelihoodData,
    path_config = use_test_config("config_gbg"),
    group_by_group = FALSE # Default value
  )
)
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 2369 5273 9290 1252 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
#>    user  system elapsed 
#>  12.373   0.170  13.005
```

- Group by group model

``` r

system.time(
  results_gbg <- lifelihood(
    lifelihoodData,
    path_config = use_test_config("config_gbg"),
    group_by_group = TRUE
  )
)
#> Fitting group: 0.0
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9092_5404_933_9190/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9092_5404_933_9190/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 9092 5404 933 9190 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
#> [1] "Intermediate files are stored at: /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9092_5404_933_9190"
#> Fitting group: 1.0
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9093_5405_934_9191/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9093_5405_934_9191/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 9093 5405 934 9191 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
#> [1] "Intermediate files are stored at: /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9093_5405_934_9191"
#> Fitting group: 2.0
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9094_5406_935_9192/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9094_5406_935_9192/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 9094 5406 935 9192 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
#> [1] "Intermediate files are stored at: /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9094_5406_935_9192"
#> Fitting group: 0.1
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9095_5407_936_9193/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9095_5407_936_9193/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 9095 5407 936 9193 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
#> [1] "Intermediate files are stored at: /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9095_5407_936_9193"
#> Fitting group: 1.1
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9096_5408_937_9194/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9096_5408_937_9194/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 9096 5408 937 9194 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
#> [1] "Intermediate files are stored at: /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9096_5408_937_9194"
#> Fitting group: 0.2
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9097_5409_938_9195/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9097_5409_938_9195/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 9097 5409 938 9195 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
#> [1] "Intermediate files are stored at: /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9097_5409_938_9195"
#> Fitting group: 0.3
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9098_5410_939_9196/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9098_5410_939_9196/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 9098 5410 939 9196 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
#> [1] "Intermediate files are stored at: /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9098_5410_939_9196"
#> Fitting group: 2.3
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9099_5411_940_9197/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9099_5411_940_9197/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 9099 5411 940 9197 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
#> [1] "Intermediate files are stored at: /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9099_5411_940_9197"
#>    user  system elapsed 
#>   1.285   0.064   1.378
```

Fitting saturated model with group by group is faster than default
model.

- Comparison

``` r

results_default$likelihood
#> [1] -343779.8
results_gbg$likelihood
#> [1] -343766.5
```

Log-likelihood is also higher with group by group indicating better
convergence towards the maximum log-likelihood value.

# Get dataframe with default parameter boundaries

Once you have created your `lifelihoodData` object with
[`lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/lifelihoodData.md),
you can call the `default_bounds_df()` function to generate (and load) a
dataframe with default parameter bounds. This is useful when you want to
customise these bounds and then pass this dataframe to the
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
function via the `param_bounds_df` argument (if not, it will
automatically generate it and keep the default values).

## Usage

``` r
default_bounds_df(lifelihoodData)
```

## Arguments

- lifelihoodData:

  `lifelihoodData` object created with
  [`lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/lifelihoodData.md).

## Value

A dataframe with the default parameter boundaries.

## Examples

``` r
library(lifelihood)
library(tidyverse)

df <- fakesample |>
  mutate(
    geno = as.factor(geno),
    type = as.factor(type)
  )
head(df)
#>   type geno sex_start sex_end sex mat_start mat_end clutch_start1 clutch_end1
#> 1    1    0         0    1000   0         0    1000            NA          NA
#> 2    2    0         0    1000   0         2       3             4           3
#> 3    0    1         0    1000   0         3       4             2           4
#> 4    0    1         0    1000   0         3       4             2           4
#> 5    0    1         0    1000   0         3       4             2           4
#> 6    1    0         0    1000   0         0    1000            NA          NA
#>   clutch_size1 clutch_start2 clutch_end2 clutch_size2 death_start death_end
#> 1           NA            NA          NA           NA         0.1         1
#> 2            4             2           4            5         9.0        10
#> 3            5            NA          NA           NA         5.0         6
#> 4            5            NA          NA           NA         5.0         6
#> 5            5            NA          NA           NA         5.0         6
#> 6           NA            NA          NA           NA         0.1         1

clutchs <- c(
  "clutch_start1", "clutch_end1", "clutch_size1",
  "clutch_start2", "clutch_end2", "clutch_size2"
)

dataLFH <- lifelihoodData(
  df = df,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "mat_start",
  maturity_end = "mat_end",
  clutchs = clutchs,
  death_start = "death_end",
  death_end = "death_end",
  covariates = c("geno", "type"),
  model_specs = c("gam", "lgn", "wei")
)

bounds_df <- default_bounds_df(dataLFH)
head(bounds_df)
#>              param   min     max
#> 1       expt_death 0.001      40
#> 2  survival_param2  0.05     500
#> 3 ratio_expt_death  0.01     100
#> 4       prob_death 1e-05 0.99999
#> 5        sex_ratio 1e-05 0.99999
#> 6    expt_maturity 0.001       8

# for example, we want to change this value
bounds_df[bounds_df$name == "increase_death_hazard", "max"] <- 80

# then we pass it to lifelihood()
results <- lifelihood(
  lifelihoodData = dataLFH,
  path_config = get_config_path("config"),
  param_bounds_df = bounds_df,
  raise_estimation_warning = FALSE
)
#> [1] "/private/var/folders/kg/7q73ww8s3llgyl61c9z_j5g40000gn/T/Rtmpe76wja/temp_libpath1e7937d95995/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_/temp_param_range_path.txt FALSE 0 25 FALSE 0 FALSE 0 2607 1476 8166 2245 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
```

# Get dataframe with default parameter boundaries

Once you have created your `lifelihoodData` object with
[`as_lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md),
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
  [`as_lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md).

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

dataLFH <- as_lifelihoodData(
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
  dist = c("gam", "lgn", "wei")
)
#> Error in as_lifelihoodData(df = df, sex = "sex", sex_start = "sex_start",     sex_end = "sex_end", maturity_start = "mat_start", maturity_end = "mat_end",     clutchs = clutchs, death_start = "death_end", death_end = "death_end",     covariates = c("geno", "type"), dist = c("gam", "lgn", "wei")): argument "matclutch" is missing, with no default

bounds_df <- default_bounds_df(dataLFH)
#> Error: object 'dataLFH' not found
head(bounds_df)
#> Error: object 'bounds_df' not found

# for example, we want to change this value
bounds_df[bounds_df$name == "increase_death_hazard", "max"] <- 80
#> Error: object 'bounds_df' not found

# then we pass it to lifelihood()
results <- lifelihood(
  lifelihoodData = dataLFH,
  path_config = use_test_config("config"),
  param_bounds_df = bounds_df,
  raise_estimation_warning = FALSE
)
#> Error: object 'dataLFH' not found
```

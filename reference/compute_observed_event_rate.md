# Compute empirical mortality rate

Calculate the empirical mortality rate over a given interval.

## Usage

``` r
compute_observed_event_rate(
  lifelihoodData,
  interval_width,
  event = c("mortality", "maturity", "reproduction"),
  newdata = NULL,
  max_time = NULL,
  min_sample_size = 1,
  groupby = NULL
)
```

## Arguments

- lifelihoodData:

  `lifelihoodData` object created with
  [`lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/lifelihoodData.md).

- interval_width:

  The interval width used to calculate the mortality rate. For instance,
  if the time unit for deaths in the original dataset is days and
  `interval_width` is set to 10, the mortality rate will be calculated
  every 10 days for each group.

- newdata:

  Data for computation. If absent, predictions are for the subjects used
  in the original fit.

- max_time:

  The maximum time for calculating the mortality rate. If set to NULL,
  the time of the last observed death is used.

- min_sample_size:

  The minimum number of individuals alive at the beggining of a time
  interval for computing the observed mortality rate

- groupby:

  vector of covariate(s) over which mortality rate should be computed
  (default is `NULL`).

  - If NULL, calculates a single overall mortality rate.

  - If `"all"`, calculates mortality rate over each combination of
    covariates listed in the`lifelihoodData` object provided.

  - Otherwise must be a character (`"covariate1"`) or a character vector
    (`c("covariate1", "covariate2")`). Note that the function will
    consider continuous covariates as factors

## Value

A dataframe with 3 columns: Interval (time interval, based on
`interval_width` value), group (identifier of a given subgroup, or
"Overall" if groupby = NULL), and Event_rate (event rate at this time).

## Examples

``` r
library(lifelihood)
library(tidyverse)

df <- fakesample |>
  mutate(
    geno = as.factor(geno),
    type = as.factor(type)
  )

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
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("geno", "type"),
  model_specs = c("gam", "lgn", "wei")
)

mort_df <- compute_observed_event_rate(dataLFH, interval_width = 2)
head(mort_df)
#>   time Interval_start Interval_end Mean_Interval Event_Rate
#> 1    0              0            2             1  0.2307692
#> 2    2              2            4             3  0.0000000
#> 3    4              4            6             5  0.1000000
#> 4    6              6            8             7  0.0000000
#> 5    8              8           10             9  0.0000000
#> 6   10             10           12            11  0.0000000

mort_df <- compute_observed_event_rate(
  dataLFH,
  interval_width = 2,
  groupby = NULL,
  max_time = 170
)
head(mort_df)
#>   time Interval_start Interval_end Mean_Interval Event_Rate
#> 1    0              0            2             1  0.2307692
#> 2    2              2            4             3  0.0000000
#> 3    4              4            6             5  0.1000000
#> 4    6              6            8             7  0.0000000
#> 5    8              8           10             9  0.0000000
#> 6   10             10           12            11  0.0000000
```

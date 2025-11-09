# Compute empirical event rate

Calculate the empirical event rate over a given interval.

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

  The interval width used to calculate the event rate. For instance, if
  the time unit for deaths in the original dataset is days and
  `interval_width` is set to 10, the event rate will be calculated every
  10 days for each group.

- newdata:

  Data for computation. If absent, predictions are for the subjects used
  in the original fit.

- max_time:

  The maximum time for calculating the event rate. If set to NULL, the
  time of the last observed death is used.

- min_sample_size:

  The minimum number of individuals alive at the beggining of a time
  interval for computing the observed event rate

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

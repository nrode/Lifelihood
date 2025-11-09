# Compute fitted mortality rate

Calculate the empirical mortality rate over a given interval on some new
data.

## Usage

``` r
compute_fitted_event_rate(
  lifelihoodResults,
  interval_width,
  event = c("mortality", "maturity", "reproduction"),
  newdata = NULL,
  max_time = NULL,
  groupby = NULL
)
```

## Arguments

- lifelihoodResults:

  A `lifelihoodResults` object

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

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
  groupby = NULL,
  mcmc.ci.fit = FALSE
)
```

## Arguments

- lifelihoodResults:

  output of
  [`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md).

- interval_width:

  The interval width used to calculate the event rate. For instance, if
  the time unit for deaths in the original dataset is days and
  `interval_width` is set to 10, the event rate will be calculated every
  10 days for each group.

- event:

  Which event to compute? Must be one of "mortality", "maturity",
  "reproduction".

- newdata:

  Optional `data.frame` providing covariate values for prediction. If
  `NULL`, the original model data are used.

- max_time:

  The maximum time for calculating the event rate. If set to NULL, the
  time of the last observed death is used.

- groupby:

  One or multiple covariates used to group the computation.

- mcmc.ci.fit:

  Whether or not to retrieve MCMC CI estimations.

## Value

A dataframe with 3 columns: Interval (time interval, based on
`interval_width` value), group (identifier of a given subgroup, or
"Overall" if groupby = NULL), and Event_rate (event rate over the
interval). Note that for reproduction event, the first reproduction
event of each individual cannot be computed if maturity was not observed
(i.e. mat_clutch is true) When the interval between the last
reproduction event of an individual and their death is greater than
`interval_width` the individuals are included in the computation of
reproduction rate

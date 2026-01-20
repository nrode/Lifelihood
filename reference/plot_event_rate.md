# Display change in predicted event rate with age

Useful function for creating a good-quality line graph of changes in
predicted event rate with age.

If you want more control over the style of the graph, use the
[`compute_fitted_event_rate()`](https://nrode.github.io/Lifelihood/reference/compute_fitted_event_rate.md)
function to directly retrieve the predicted data.

Useful function for creating a good-quality line graph of changes in the
empirical mortality rate.

You don't need to fit with
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
to use this function, only to retrieve a lifelihood data object with
[`lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/lifelihoodData.md)

If you want more control over the style of the graph, use the
[`compute_observed_event_rate()`](https://nrode.github.io/Lifelihood/reference/compute_observed_event_rate.md)
function to retrieve data.

Convenient function used in `plot_observed_event_rate()` and
`plot_fitted_event_rate()`.

## Usage

``` r
plot_fitted_event_rate(
  lifelihoodResults,
  interval_width,
  event = c("mortality", "maturity", "reproduction"),
  newdata = NULL,
  add_observed_event_rate = TRUE,
  min_sample_size = 1,
  max_time = NULL,
  groupby = NULL,
  use_facet = FALSE,
  se.fit = FALSE,
  xlab = "Time",
  ylab = "Event Rate",
  type = "points"
)

plot_observed_event_rate(
  lifelihoodData,
  interval_width,
  event = c("mortality", "maturity", "reproduction"),
  max_time = NULL,
  min_sample_size = 1,
  groupby = NULL,
  use_facet = FALSE,
  xlab = "Time",
  ylab = "Mortality Rate"
)

plot_event_rate(
  rate_df,
  max_time,
  type = c("points", "lines"),
  groupby,
  use_facet,
  xlab = "Time",
  ylab = "Event rate",
  fitted_data = FALSE,
  plot_ci = FALSE
)
```

## Arguments

- lifelihoodResults:

  Output of
  [`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md).

- interval_width:

  The interval width used to calculate the event rate.

- event:

  Which event to compute? Must be one of "mortality", "maturity",
  "reproduction".

- newdata:

  Optional `data.frame` providing covariate values for prediction. If
  `NULL`, the original model data are used.

- add_observed_event_rate:

  Boolean to add the observed event rate to the graph (default=TRUE).

- min_sample_size:

  The minimum number of individuals alive at the beggining of a time
  interval for computing the observed event rate.

- max_time:

  The maximum time for calculating the event rate. If set to NULL, the
  time of the last observed death is used.

- groupby:

  Factor(s) whosse levels over which event rate should be represented
  (default=NULL).

- use_facet:

  Use facet_wrap to plot one panel per group (default=FALSE).

- se.fit:

  Whether or not to plot standard errors. Requires to fit MCMC when
  fitting with
  [`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
  with the `MCMC` argument.

- xlab:

  Label for x-axis (default="Time").

- ylab:

  Label for y-axis (default="Event rate").

- type:

  The type of symbol to be used for the plot (either of "points" or
  'lines").

- lifelihoodData:

  `lifelihoodData` object created with
  [`lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/lifelihoodData.md).

- rate_df:

  Dataframe with event rate.

- fitted_data:

  Boolean indicating if the data is fitted (default=FALSE).

- plot_ci:

  Boolean indicating if confidence intervals should be plotted
  (default=FALSE). Requires CI_2.5% and CI_97.5% columns in rate_df.

## Value

a ggplot2 plot

a ggplot2 plot

a ggplot2 plot

## Details

This function requires [ggplot2](https://ggplot2.tidyverse.org/) to be
installed.

This function requires [ggplot2](https://ggplot2.tidyverse.org/) to be
installed.

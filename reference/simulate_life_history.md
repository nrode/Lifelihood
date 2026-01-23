# Simulate outcomes from a fitted lifelihood model

This function generates simulated data from a fitted lifelihood model,
for one or several life history events. By default, all fitted events
are simulated.

## Usage

``` r
simulate_life_history(
  object,
  event = c("all", "mortality", "reproduction", "maturity"),
  newdata = NULL,
  use_censoring = FALSE,
  visits = NULL,
  seed = NULL
)
```

## Arguments

- object:

  A fitted `lifelihoodResults` object.

- event:

  Character string specifying the event(s) to simulate. Must be one of
  `"mortality"`, `"maturity"`, or `"all"` (event=`"reproduction"` is
  equivalent to `"all"` as maturity and mortality are needed to simulate
  reproduction events). Default is `"all"`, which simulates all fitted
  events.

- newdata:

  Optional `data.frame` providing covariate values for prediction. If
  `NULL`, the original model data are used.

- use_censoring:

  Whether to retrieve censoring time interval for each event. For
  example, the time for "mortality_start" and "mortality_end" instead of
  just the time for "mortality". In this case, it's advised to use the
  `visits` argument and provide you own visit data. Otherwise, they are
  determined only using ages where events have been observed not ages
  where people went to the lab and no event was observed.

- visits:

  Optionnal dataframe with 2 columns: "block" (must be the same name as
  passed in
  [`as_lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md)
  `block` argument) and exactly "visit". For each block, "visit"
  corresponds to the ages where the events of individuals have been
  recorded.

- seed:

  Optional integer. If provided, sets the random seed for
  reproducibility.

## Value

A list of `data.frame` with one column per simulated event. Each column
contains simulated values for that event.

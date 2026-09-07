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
  visits = NULL,
  seed = NULL,
  remove_exact_clutch_dates = TRUE
)
```

## Arguments

- object:

  A fitted `lifelihoodResults` object created either with
  [`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
  or
  [`create_simulation_input()`](https://nrode.github.io/Lifelihood/reference/create_simulation_input.md).

- event:

  Character string specifying the event(s) to simulate. Must be one of
  `"mortality"`, `"maturity"`, or `"all"` (event=`"reproduction"` is
  equivalent to `"all"` as maturity and mortality are needed to simulate
  reproduction events). Default is `"all"`, which simulates all fitted
  events.

- newdata:

  Optional `data.frame` providing covariate values for prediction. If
  `NULL`, the original model data are used.

- visits:

  Optional data frame with 2 columns: one column with the same name as
  the `block` argument passed to
  [`as_lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md)
  and one column named exactly `visit`. For each block, `visit`
  corresponds to the ages where the events of individuals have been
  recorded. Supplying `visits` enables censoring; use
  [`get_visits()`](https://nrode.github.io/Lifelihood/reference/get_visits.md)
  to derive visit data from the fitted data, or pass a custom visit data
  frame.

- seed:

  Optional integer. If provided, sets the random seed for
  reproducibility.

- remove_exact_clutch_dates:

  Logical. Whether to remove exact simulated clutch-date columns from
  the output. If `FALSE`, these columns are kept and renamed to
  `exact_clutch_date_{i}`. Default is `TRUE`.

## Value

A list of `data.frame` with one column per simulated event. Each column
contains simulated values for that event.

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

- seed:

  Optional integer. If provided, sets the random seed for
  reproducibility.

## Value

A list of `data.frame` with one column per simulated event. Each column
contains simulated values for that event.

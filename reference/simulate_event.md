# Simulation for a single life event

Internal function used to simulate one of the life history event
(maturity, reproduction, mortality).

## Usage

``` r
simulate_event(object, ev, newdata)
```

## Arguments

- object:

  Output of
  [`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)

- ev:

  A character of the event (must be one of "mortality", "reproduction"
  or "maturity")

- newdata:

  An optional dataset used for prediction

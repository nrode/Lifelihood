# Simulation for a single life event

Internal function used to simulate one of the life history event
(maturity, reproduction, mortality).

## Usage

``` r
simulate_event(object, ev, newdata, lifelihoodData, use_censoring, visits)
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

- lifelihoodData:

  Output of
  [`as_lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md).

- visits:

  Dataframe with 2 columns: "block" (must be the same as passed in
  [`as_lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md)
  `block` argument) and exactly "visit". For each block, "visit"
  corresponds to the ages where the events of individuals have been
  recorded.

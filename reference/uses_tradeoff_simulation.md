# Detect tradeoff-aware simulation requirements

Internal helper that checks whether the fitted model includes any
tradeoff parameters requiring the tradeoff simulation path.

## Usage

``` r
uses_tradeoff_simulation(object)
```

## Arguments

- object:

  A fitted `lifelihoodResults` object (output of
  [`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)).

## Value

A logical scalar indicating whether tradeoff simulation should be used.

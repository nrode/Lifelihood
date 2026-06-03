# Check whether a parameter was fitted

Internal helper used by simulation code to determine whether a parameter
is available in a fitted `lifelihoodResults` object.

## Usage

``` r
is_parameter_fitted(object, parameter_name)
```

## Arguments

- object:

  A fitted `lifelihoodResults` object (output of
  [`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)).

- parameter_name:

  Character scalar with the parameter name to check.

## Value

A logical scalar. `TRUE` if `parameter_name` is present among fitted
effects, `FALSE` otherwise.

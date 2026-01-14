# Akaike Information Criterion

S3 method to compute AIC (Akaike Information Criterion).

## Usage

``` r
# S3 method for class 'lifelihoodResults'
AIC(object, ..., k = length(coef(object)))
```

## Arguments

- object:

  output of
  [`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)

- ...:

  Ignored

- k:

  Number of estimated parameter of the model. Default to
  `length(coef(object))`.

## Value

The AIC

## See also

[`AICc()`](https://nrode.github.io/Lifelihood/reference/AICc.md),
[`BIC()`](https://rdrr.io/r/stats/AIC.html)

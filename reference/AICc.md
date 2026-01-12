# Akaike Information Criterion for small sample size

S3 method to compute AICc (Akaike Information Criterion corrected for
small sample size, see Hurvich and Tsai 1989).

## Usage

``` r
AICc(object, ..., k = length(coef(object)))
```

## Arguments

- object:

  Output of
  [`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md).

- ...:

  Ignored.

- k:

  Number of estimated parameter of the modèle. Default to
  `length(coef(object))`.

## Value

The AICc

## See also

[`AIC()`](https://rdrr.io/r/stats/AIC.html),
[`BIC()`](https://rdrr.io/r/stats/AIC.html)

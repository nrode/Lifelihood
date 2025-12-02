# Coefficient estimates

`coef()` retrieve all coefficients from the output of
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)

`coeff()` retrieve coefficients of one parameter from the output of
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)

## Usage

``` r
# S3 method for class 'lifelihoodResults'
coef(object, ...)

coeff(object, parameter_name)
```

## Arguments

- object:

  output of
  [`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)

- ...:

  Ignored

- parameter_name:

  Name of the parameters to extract the estimate from to extract all
  parameter estimates). All parameters#' can be found
  [here](https://nrode.github.io/articles/setting-up-the-configuration-file.html#parameters).

## Value

A nested list of coefficient estimates

A list of coefficient estimates

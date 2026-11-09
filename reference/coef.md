# Coefficient estimates

`coef()` retrieves all coefficients from the output of
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md).

`coeff()` retrieves the coefficients for one parameter from the output
of
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md).

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

  Name of the parameter whose estimates should be extracted. To extract
  all parameter estimates, use `coef()`. All parameters can be found
  [here](https://nrode.github.io/articles/setting-up-the-configuration-file.html#parameters).

## Value

A nested list of coefficient estimates

A list of coefficient estimates

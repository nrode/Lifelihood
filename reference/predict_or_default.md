# Predict a parameter with fallback values

Internal helper used in simulations to predict a parameter on the
response scale and safely fall back to a default value when the
parameter is not fitted, prediction fails, or predicted values are
non-finite.

## Usage

``` r
predict_or_default(object, parameter_name, newdata, n_obs, default = 0)
```

## Arguments

- object:

  A fitted `lifelihoodResults` object (output of
  [`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)).

- parameter_name:

  Character scalar with the parameter name to predict.

- newdata:

  Optional `data.frame` used for prediction.

- n_obs:

  Integer number of observations expected in the output vector.

- default:

  Numeric default value used as fallback.

## Value

A numeric vector of length `n_obs`.

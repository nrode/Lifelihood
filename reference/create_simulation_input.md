# Create a lifelihood-like object for simulation

Builds a `lifelihoodResults`-like object from manually supplied
parameter effects. The returned object can be passed to
[`simulate_life_history()`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md)
in the same way as the output of
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md).

The `effects` list must contain one entry for every parameter fitted in
`config`. For intercept-only parameters, use a scalar or `list(1)`. For
categorical covariates, use one vector per covariate. When no explicit
`intercept` is supplied, the first value of each covariate vector is
used as the shared intercept and the remaining values are used as
non-reference level effects.

## Usage

``` r
create_simulation_input(
  effects,
  data,
  covariates,
  sex,
  config,
  dist,
  matclutch = FALSE,
  matclutch_size = NULL,
  block = NULL,
  n_per_combination = NULL,
  param_bounds_df = NULL,
  right_censoring_date = 1000,
  max_clutch_size = 100
)
```

## Arguments

- effects:

  Named list of parameter effects on the lifelihood link scale.

- data:

  Data frame containing one row per simulated individual, or one row per
  combination when `n_per_combination` is supplied. It must contain the
  columns named in `covariates` and `sex`.

- covariates:

  Character vector with covariate column names in `data`.

- sex:

  Name of the sex column in `data`.

- config:

  Path to a YAML configuration file or an already-loaded configuration
  list.

- dist:

  Character vector with one distribution family for mortality, maturity
  and reproduction, in that order. Values must be `"wei"`, `"exp"`,
  `"gam"` or `"lgn"`.

- matclutch:

  Whether maturity corresponds to the first clutch.

- matclutch_size:

  Optional name of the first clutch size column when `matclutch = TRUE`.

- block:

  Optional name of the block column in `data`.

- n_per_combination:

  Optional name of a column in `data` containing the number of
  individuals to create for each row.

- param_bounds_df:

  Optional data frame with columns `param`, `min` and `max`. If `NULL`,
  defaults are created with
  [`default_bounds_df()`](https://nrode.github.io/Lifelihood/reference/default_bounds_df.md).

- right_censoring_date:

  Numeric right censoring date stored in the internal `lifelihoodData`
  object.

- max_clutch_size:

  Numeric maximum clutch size stored in the internal `lifelihoodData`
  object.

## Value

A `lifelihoodResults` object suitable for
[`simulate_life_history()`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md).

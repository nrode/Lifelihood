# Compare all fitted models and replicates

Summarises every fit retained by
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md),
including the model's distribution families, random seeds, parameter
estimates, and fit criteria. Rows are sorted by increasing AICc. The
first row can differ from the fit returned by
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md),
which selects the highest log-likelihood.

## Usage

``` r
# S3 method for class 'all_models'
summary(object, ...)
```

## Arguments

- object:

  The `all_models` element of a
  [`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
  result.

- ...:

  Ignored.

## Value

A tibble with one row per fit, sorted by AICc. `fit` identifies the
corresponding entry in `all_models`. The delta AICc column gives the
difference from the lowest AICc in the table.

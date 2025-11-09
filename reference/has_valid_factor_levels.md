# Check for valid factor levels

The purpose of this function is to ensure that when a user makes a
prediction with [`predict()`](https://rdrr.io/r/stats/predict.html), the
`newdata` contains the same factor levels for its covariates as the
training data. If any levels are missing or mismatched, it raises an
error and displays a warning.

## Usage

``` r
has_valid_factor_levels(original_df, newdata, covariates)
```

## Arguments

- original_df:

  Training set passed to
  [`lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/lifelihoodData.md)
  (`df` arg).

- newdata:

  New data passed to [`predict()`](https://rdrr.io/r/stats/predict.html)
  (`newdata` arg).

- covariates:

  Covariates passed to
  [`lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/lifelihoodData.md)
  (`covariates` arg).

## Value

TRUE if all factor levels of each covariate in `newdata` passed to
[`predict()`](https://rdrr.io/r/stats/predict.html) are present in the
training data, FALSE otherwise.

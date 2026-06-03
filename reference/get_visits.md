# Retrieve visit masks from a lifelihood data object

Builds the visit-mask data frame used by
[`simulate_life_history()`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md)
when `use_censoring = TRUE`. The returned visits are inferred from the
observed interval bounds in the original data.

## Usage

``` r
get_visits(lifelihoodData)
```

## Arguments

- lifelihoodData:

  Output of
  [`as_lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md).
  It must include a valid `block` column.

## Value

A data frame with one column named like `lifelihoodData$block` and one
column named `visit`.

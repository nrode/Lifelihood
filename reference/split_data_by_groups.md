# Split data by covariate groups

Creates sub-datasets by splitting the data according to the unique
combinations of group covariates.

## Usage

``` r
split_data_by_groups(lifelihoodData, group_covariates)
```

## Arguments

- lifelihoodData:

  A `lifelihoodData` object.

- group_covariates:

  Character vector of covariate names.

## Value

A named list of `lifelihoodData` objects, one per group.

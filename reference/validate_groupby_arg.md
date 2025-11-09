# Check that the `groupby` argument is valid

Check that `groupby` has an expected value, and returns it

## Usage

``` r
validate_groupby_arg(lifelihoodData, groupby)
```

## Arguments

- lifelihoodData:

  `lifelihoodData` object created with
  [`lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/lifelihoodData.md).

- groupby:

  vector of covariate(s) over which mortality rate should be computed
  (default is `NULL`).

  - If NULL, calculates a single overall mortality rate.

  - If `"all"`, calculates mortality rate over each combination of
    covariates listed in the`lifelihoodData` object provided.

  - Otherwise must be a character (`"covariate1"`) or a character vector
    (`c("covariate1", "covariate2")`). Note that the function will
    consider continuous covariates as factors

## Value

The valid `groupby` value

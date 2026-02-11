# Extract covariate names from a formula string

Parses a formula string (e.g., `"par * geno"`) to extract the individual
covariate names.

## Usage

``` r
extract_group_covariates(common_formula)
```

## Arguments

- common_formula:

  A character string with the formula.

## Value

A character vector of unique covariate names.

# Convert R formula to lifelihood formula

Transforms a character string describing the covariates to be included
into a format which the compiled program can understand. For example,
`"geno + type"` will become `1 2` if `"geno"` is the first element of
`covariables` and `"type"` is the second. This function is used to
create the model part of the input text file.

## Usage

``` r
R_to_lifelihood(R_format, covariates, covar_types)
```

## Arguments

- R_format:

  String representing the covariates to be adjusted. For example,
  "geno + type" will use the covariates geno and type.

- covariates:

  Vector containing the names of the covariates.

## Value

The formatted format for lifelihood to understand which parameter to
fit.

## Examples

``` r
R_to_lifelihood("geno + type", c("geno", "type"))
#> Error in R_to_lifelihood("geno + type", c("geno", "type")): argument "covar_types" is missing, with no default
R_to_lifelihood("geno + type + geno*type", c("geno", "type"))
#> Error in R_to_lifelihood("geno + type + geno*type", c("geno", "type")): argument "covar_types" is missing, with no default
```

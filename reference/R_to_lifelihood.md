# Convert R formula to lifelihood formula

Transforms a character string describing the covariates to be included
in a format that the compiled program can understand. For example,
`"geno + type"` will become `1 2` if `"geno"` is the first element of
`covariates` and `"type"` is the second. This function is used to create
the model part of the input text file.

## Usage

``` r
R_to_lifelihood(R_format, covariates, covar_types)
```

## Arguments

- R_format:

  String representing the covariates to be adjusted. For example,
  `"geno + type"` uses the `geno` and `type` covariates.

- covariates:

  Vector containing the names of the covariates.

- covar_types:

  Vector containing the types of the covariates (either "cat" for
  categorical or "num" for numerical).

## Value

A formatted string that tells lifelihood which parameters to fit.

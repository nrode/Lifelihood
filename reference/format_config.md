# Read and parse the configuration file (YAML).

Safely access the configuration file to use for lifelihood. This
function is used in
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
when creating the input text file.

## Usage

``` r
format_config(path_config, covariates, covar_types)
```

## Arguments

- path_config:

  A character string specifying the file path to the YAML configuration
  file.

- covariates:

  Vector containing the names of the covariates.

- covar_types:

  Vector containing the types of the covariates (either "cat" for
  categorical or "num" for numerical).

## Value

A character vector that will be used under the model tag in the input
text file.

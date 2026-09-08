# Read and format the output file of the program

Takes the file path of the output file and read the results using
parsers from
[`parse_output()`](https://nrode.github.io/Lifelihood/reference/parse_output.md).

## Usage

``` r
read_output_from_file(
  file_path,
  covariates = NULL,
  config = NULL,
  MCMC,
  path_config = NULL
)
```

## Arguments

- file_path:

  Location of the output file of the program.

- covariates:

  Vector containing the names of the covariates.

- config:

  A complete configuration list.

- MCMC:

  Perform MCMC sampling of the parameter after convergence to estimate
  their 95% confidence interval.

- path_config:

  Deprecated alias for `config`.

## Value

An object of class `lifelihoodResults` with all results from the output
file.

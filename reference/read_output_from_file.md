# Read and format the output file of the program

Takes the file path of the output file and read the results using
parsers from
[`parse_output()`](https://nrode.github.io/Lifelihood/reference/parse_output.md).

## Usage

``` r
read_output_from_file(
  file_path,
  group_by_group = FALSE,
  covariates = NULL,
  path_config
)
```

## Arguments

- file_path:

  Location of the output file of the program

- group_by_group:

  Boolean option to fit the full factorial model with all the
  interactions between each of the factors

- path_config:

  A character string specifying the file path to the YAML configuration
  file.

## Value

An object of class `lifelihoodResults` with all results from the output
file

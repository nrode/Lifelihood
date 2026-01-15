# Count total number of parameter to fit

Simple utility function that parses the config yaml file and count the
total number of parameter that will be fit.

## Usage

``` r
count_parameters(x)
```

## Arguments

- x:

  Config file parsed by
  [`yaml::yaml.load_file()`](https://yaml.r-lib.org/reference/yaml.load.html).

## Value

Total number of parameter to fit.

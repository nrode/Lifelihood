# Parsing functions used to read the output file of the program

Find specific result in the output file of the lifelihood program,
according to the `element` argument. This function is an aggregator of
all the `get_*()` functions described below.

## Usage

``` r
parse_output(lines, element)

get_seeds(lines)

get_likelihood(lines)

get_param_ranges(lines)

get_ratio_max(lines)

get_effects(lines)

get_hessian(lines)

get_mcmc(lines)
```

## Arguments

- lines:

  Vector of the output file (`.out`), where each element is a line of
  the file.

- element:

  Name of the result to parse. Must be in one of 'seeds', 'likelihood',
  'effects', 'parameter_ranges', 'ratio_max', 'mcmc'.

## Value

The parsed element

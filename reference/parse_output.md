# Parsing functions used to read the output file of the program

Find specific result in the output file of the lifelihood program,
according to the `element` argument. This function is an aggregator of
all the `get_*()` functions described below.

## Usage

``` r
parse_output(lines, element, group_by_group = FALSE)

get_seeds(lines, group_by_group = FALSE)

get_likelihood(lines, group_by_group = FALSE)

get_param_ranges(lines)

get_ratio_max(lines)

get_effects(lines, group_by_group = FALSE)

get_hessian(lines)
```

## Arguments

- lines:

  Vector of the output file (`.out`), where each element is a line of
  the file.

- element:

  Name of the result to parse. Must be in one of 'seeds', 'likelihood',
  'effects', 'parameter_ranges', 'ratio_max', 'mcmc'.

- group_by_group:

  Boolean indicating whether parsing should be performed group by group
  or not (`FALSE` by default). This argument is necessary because the
  structure of the output file is different depending on whether the
  analysis was carried out "group by group" or not (the analysis method
  used will then be different, for certain parsers).

## Value

The parsed element

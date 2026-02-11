# Merge group-by-group results

Combines the results from individual group fits into a single
`lifelihoodResults` object.

## Usage

``` r
merge_group_results(
  group_results,
  group_names,
  original_config,
  original_lifelihoodData
)
```

## Arguments

- group_results:

  Named list of `lifelihoodResults` objects.

- group_names:

  Character vector of group labels.

- original_config:

  Original config list from YAML.

- original_lifelihoodData:

  Original `lifelihoodData` object.

## Value

A `lifelihoodResults` object with merged results.

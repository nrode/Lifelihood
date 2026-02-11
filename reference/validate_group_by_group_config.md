# Validate group-by-group configuration

Checks that all fitted parameters in the config use the same covariate
formula, as required for group-by-group fitting.

## Usage

``` r
validate_group_by_group_config(config)
```

## Arguments

- config:

  Configuration list loaded from YAML file.

## Value

The single common formula string (e.g., `"par"` or `"par * geno"`).

# Create intercept-only config

Takes the original config and replaces every non-`"not_fitted"` formula
with `"1"` (intercept only). Writes the result to a temporary YAML file.

## Usage

``` r
create_intercept_only_config(original_config, temp_dir)
```

## Arguments

- original_config:

  Configuration list loaded from YAML.

- temp_dir:

  Directory where the temp config should be written.

## Value

The file path to the temporary YAML config.

# safe access

Safely access elements in config file. This function exists because
yaml.load_file() returns NULL when a value is not found instead of
raising an error.

## Usage

``` r
safe_access(config, path)
```

## Arguments

- config:

  configuration object

- path:

  event and parameter to read

## Value

the read value

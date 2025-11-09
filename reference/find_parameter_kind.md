# Deducting the type of parameter from an estimate

This function takes an estimate name as input and returns whether it is
a intercept or a coefficient/slope. If the element is not found in the
possible match, it returns an error. This function is used to add
information about the type of estimate to the output of the
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
function.

## Usage

``` r
find_parameter_kind(name)
```

## Arguments

- name:

  A character string representing the parameter name.

## Value

The kind of parameter: either intercept or coefficient/slope

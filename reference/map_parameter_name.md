# Associate the parameter name with the associated parameter

This function takes a name as input and returns its match from a
predefined list. If the element is not found in the match, an error is
generated. This function is used to determine which parameter a
parameter estimate is based on.

## Usage

``` r
map_parameter_name(name)
```

## Arguments

- name:

  A character string representing the element to be mapped.

## Value

A named list containing the mapped value for the input element.

## Examples

``` r
map_parameter_name("eff_expt_death_geno1")
#> Error in map_parameter_name("eff_expt_death_geno1"): could not find function "map_parameter_name"
map_parameter_name("eff_ratio_expt_death_geno1")
#> Error in map_parameter_name("eff_ratio_expt_death_geno1"): could not find function "map_parameter_name"
map_parameter_name("int_sex_ratio")
#> Error in map_parameter_name("int_sex_ratio"): could not find function "map_parameter_name"
```

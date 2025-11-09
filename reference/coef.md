# Coefficient estimates

`coef()` retrieve all coefficients from the output of
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)

`coeff()` retrieve coefficients of one parameter from the output of
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)

## Usage

``` r
# S3 method for class 'lifelihoodResults'
coef(object, ...)

coeff(object, parameter_name)
```

## Arguments

- object:

  output of
  [`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)

- ...:

  Ignored

- parameter_name:

  Name of the parameters to extract the estimate from to extract all
  parameter estimates). All parameters#' can be found
  [here](https://nrode.github.io/articles/setting-up-the-configuration-file.html#parameters).

## Value

A nested list of coefficient estimates

A list of coefficient estimates

## Examples

``` r
library(lifelihood)
library(tidyverse)

df <- lifelihood::fakesample |>
  mutate(
    type = as.factor(type),
    geno = as.factor(geno)
  )

clutchs <- c(
  "clutch_start1", "clutch_end1", "clutch_size1",
  "clutch_start2", "clutch_end2", "clutch_size2"
)

dataLFH <- lifelihoodData(
  df = df,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "mat_start",
  maturity_end = "mat_end",
  clutchs = clutchs,
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("geno", "type"),
  model_specs = c("gam", "lgn", "wei")
)

results <- lifelihood(
  lifelihoodData = dataLFH,
  path_config = get_config_path("config"),
  seeds = c(1, 2, 3, 4),
  raise_estimation_warning = FALSE
)
#> [1] "/private/var/folders/6c/pzd640_546q6_yfn24r65c_40000gn/T/RtmppLXaV9/temp_libpath17bb228bb881/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1_2_3_4/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1_2_3_4/temp_param_range_path.txt FALSE 0 25 FALSE 0 FALSE 0 1 2 3 4 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
coef(results)
#>               int_expt_death        eff_expt_death_geno_1 
#>                   -2.5253959                    0.7835095 
#>        eff_expt_death_type_1        eff_expt_death_type_2 
#>                   -0.9687812                    1.3991316 
#>          int_survival_param2            int_expt_maturity 
#>                   -6.5822104                   -2.2285974 
#>     eff_expt_maturity_geno_1     eff_expt_maturity_type_1 
#>                   -0.8401484                    0.4112408 
#>     eff_expt_maturity_type_2          int_maturity_param2 
#>                   -0.5456602                   -3.1111023 
#>        int_expt_reproduction eff_expt_reproduction_geno_1 
#>                   -2.3365343                    1.3102563 
#>      int_reproduction_param2 
#>                   -2.5535342 
library(lifelihood)
library(tidyverse)

df <- lifelihood::fakesample |>
  mutate(
    type = as.factor(type),
    geno = as.factor(geno)
  )

clutchs <- c(
  "clutch_start1", "clutch_end1", "clutch_size1",
  "clutch_start2", "clutch_end2", "clutch_size2"
)

dataLFH <- lifelihoodData(
  df = df,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "mat_start",
  maturity_end = "mat_end",
  clutchs = clutchs,
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("geno", "type"),
  model_specs = c("gam", "lgn", "wei")
)

results <- lifelihood(
  lifelihoodData = dataLFH,
  path_config = get_config_path("config"),
  seeds = c(1, 2, 3, 4),
  raise_estimation_warning = FALSE
)
#> [1] "/private/var/folders/6c/pzd640_546q6_yfn24r65c_40000gn/T/RtmppLXaV9/temp_libpath17bb228bb881/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1_2_3_4/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1_2_3_4/temp_param_range_path.txt FALSE 0 25 FALSE 0 FALSE 0 1 2 3 4 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"

coeff(results, "expt_death")
#>        int_expt_death eff_expt_death_geno_1 eff_expt_death_type_1 
#>            -2.5253959             0.7835095            -0.9687812 
#> eff_expt_death_type_2 
#>             1.3991316 
```

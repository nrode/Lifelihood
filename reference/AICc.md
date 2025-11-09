# Akaike Information Criterion for small sample size

S3 method to compute AICc (Akaike Information Criterion corrected for
small sample size, see Hurvich and Tsai 1989).

## Usage

``` r
AICc(object, ..., k = length(coef(object)))
```

## Arguments

- object:

  output of
  [`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)

- ...:

  Ignored

- k:

  Number of estimated parameter of the modèle. Default to
  `length(coef(object))`.

## Value

The AICc

## See also

[`AIC()`](https://rdrr.io/r/stats/AIC.html),
[`BIC()`](https://rdrr.io/r/stats/AIC.html)

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
#> [1] "/private/var/folders/6c/pzd640_546q6_yfn24r65c_40000gn/T/Rtmpa5A0QF/temp_libpath28c961796e68/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1_2_3_4/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1_2_3_4/temp_param_range_path.txt FALSE 0 25 FALSE 0 FALSE 0 1 2 3 4 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
AICc(results)
#> [1] 5304.453
```

# Prediction with lifelihood estimations

S3 method to use to make prediction using fitted results from
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md).

## Usage

``` r
prediction(
  object,
  parameter_name,
  newdata = NULL,
  mcmc.fit = FALSE,
  type = c("link", "response"),
  se.fit = FALSE
)
```

## Arguments

- object:

  output of
  [`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)

- parameter_name:

  A string specifying the name of the parameter for which to make the
  prediction. Must be one of
  `unique(lifelihoodResults$effects$parameter)`.

- newdata:

  Data for prediction. If absent, predictions are for the subjects used
  in the original fit.

- type:

  The type of the predicted value: if "response," it is on the original
  data scale; if "link," it is on the lifelihood scale.

- se.fit:

  Whether or not to include standard errors in the prediction.

## Value

A vector containing the predicted values for the parameter.

## Examples

``` r
df <- fakesample |>
  mutate(
    geno = as.factor(geno),
    type = as.factor(type)
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
  path_config = get_config_path("config2"),
  seeds = c(1, 2, 3, 4),
  raise_estimation_warning = FALSE
)
#> [1] "/private/var/folders/bp/kmfmhnl95kx1c8x321z7twbw0000gn/T/RtmpAOVQyW/temp_libpath225d19102e4b/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1_2_3_4/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1_2_3_4/temp_param_range_path.txt FALSE 0 25 FALSE 0 FALSE 0 1 2 3 4 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
#> Error in (start + 1):length(lines): argument of length 0

prediction(results, "expt_death")
#> Error: object 'results' not found
prediction(results, "expt_death", type = "response")
#> Error: object 'results' not found

# predict on new data
newdata <- data.frame(
  type = c(1, 2, 0, 1, 2, 0),
  geno = c(0, 1, 0, 1, 0, 1)
)
newdata$type <- factor(newdata$type)
newdata$geno <- factor(newdata$geno)
prediction(results, "expt_death", newdata)
#> Error: object 'results' not found
prediction(results, "expt_death", newdata, type = "response")
#> Error: object 'results' not found
```

# Prediction from a lifelihood model at new data values

S3 method used to make predictions using `lifelihoodResults` objects,
i.e. results from
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md).

## Usage

``` r
prediction(
  object,
  parameter_name,
  newdata = NULL,
  mcmc.fit = FALSE,
  keep_mcmc_samples = FALSE,
  type = c("link", "response"),
  se.fit = FALSE,
  .warning_ratio_male = TRUE
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

  Data for prediction. If absent, predictions are for each individual in
  the original dataset provided by the user.

- keep_mcmc_samples:

  Whether or not to also retrieve MCMC samples in output. If `TRUE`,
  output is a list with 2 elements: pred and mcmc_samples.

- type:

  The type of the predicted value: if type="response," predictions are
  on the original data scale; if type="link," predictions are on the
  lifelihood scale.

- se.fit:

  Whether or not to include standard errors in the prediction (computed
  on the response scale using the delta method).

## Value

A vector or list containing the predicted values for the parameter.

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

dataLFH <- as_lifelihoodData(
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
  dist = c("gam", "lgn", "wei")
)
#> Error in as_lifelihoodData(df = df, sex = "sex", sex_start = "sex_start",     sex_end = "sex_end", maturity_start = "mat_start", maturity_end = "mat_end",     clutchs = clutchs, death_start = "death_start", death_end = "death_end",     covariates = c("geno", "type"), dist = c("gam", "lgn", "wei")): argument "matclutch" is missing, with no default

results <- lifelihood(
  lifelihoodData = dataLFH,
  path_config = use_test_config("config2"),
  seeds = c(1, 2, 3, 4),
  raise_estimation_warning = FALSE
)
#> Error: object 'dataLFH' not found

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

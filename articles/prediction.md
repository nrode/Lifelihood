# Prediction

[`prediction()`](https://nrode.github.io/Lifelihood/reference/prediction.md)
converts the coefficients of a fitted `lifelihood` model into parameter
values for individuals or combinations of covariates. It can return
predictions:

- for the data used to fit the model or for a new dataset;
- on the internal lifelihood scale or on the response scale;
- with standard errors or MCMC uncertainty when these were requested
  during model fitting.

This vignette covers the first two points, explains how predictions
differ between females and males, and shows how those predictions are
used in simulations. See the [standard errors and MCMC
vignette](https://nrode.github.io/Lifelihood/articles/prediction-with-standard-errors-and-mcmc.md)
for uncertainty estimates.

## Fit an example model

We first create paired female and male observations from `datadaphnia`.
The observed male death intervals are multiplied by 10, while the female
death intervals are left unchanged. Reproduction data are set to `NA`
for males.

The mortality formula in `config_pierrick` models `expt_death` as a
function of `par` and also fits `ratio_expt_death`, which represents the
male-to-female ratio in expected longevity.

``` r

library(lifelihood)
library(tidyverse)

df_female <- datadaphnia |>
  as_tibble() |>
  mutate(par = as.factor(par), spore = as.factor(spore))

df_male <- df_female |>
  mutate(
    sex = 1,
    across(starts_with("clutch"), ~ NA_real_),
    death_start = death_start * 10,
    death_end = death_end * 10
  )

df <- bind_rows(df_female, df_male) |>
  mutate(
    block = rep(
      c("female", "male"),
      c(nrow(df_female), nrow(df_male))
    )
  )

generate_clutch_vector <- function(n) {
  paste(
    "clutch",
    rep(c("start", "end", "size"), n),
    rep(seq_len(n), each = 3),
    sep = "_"
  )
}

lifelihood_data <- as_lifelihoodData(
  df = df,
  matclutch = FALSE,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "mat_start",
  maturity_end = "mat_end",
  clutchs = generate_clutch_vector(28),
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("par", "spore"),
  block = "block",
  dist = c("wei", "gam", "lgn")
)

results <- lifelihood(
  lifelihoodData = lifelihood_data,
  path_config = use_test_config("config_pierrick"),
  seeds = c(3699, 783, 5401, 6502),
  raise_estimation_warning = FALSE
)
```

The mean midpoint of the observed death intervals is exactly 10 times
larger for males because the male rows were constructed that way:

``` r

df |>
  mutate(observed_death = (death_start + death_end) / 2) |>
  summarise(
    mean_observed_death = mean(observed_death),
    .by = sex
  ) |>
  mutate(
    sex = if_else(sex == 0, "female", "male"),
    relative_to_female = mean_observed_death / first(mean_observed_death)
  )
#> # A tibble: 2 × 3
#>   sex    mean_observed_death relative_to_female
#>   <chr>                <dbl>              <dbl>
#> 1 female                81.6                1  
#> 2 male                 816.                10.0
```

`parameter_name` must identify a parameter fitted by the model. The
available parameters can be read from the results:

``` r

results$effects |> distinct(event, parameter)
#>          event           parameter
#> 1    mortality          expt_death
#> 2    mortality     survival_param2
#> 3    mortality    ratio_expt_death
#> 4     maturity       expt_maturity
#> 5     maturity     maturity_param2
#> 6 reproduction   expt_reproduction
#> 7 reproduction reproduction_param2
#> 8 reproduction         n_offspring
```

## Predict for the fitted data

When `newdata` is omitted,
[`prediction()`](https://nrode.github.io/Lifelihood/reference/prediction.md)
returns one value for each individual in the data used to fit the model.

``` r

expected_longevity <- prediction(
  results,
  parameter_name = "expt_death",
  type = "response"
)

length(expected_longevity)
#> [1] 1100
head(expected_longevity)
#> [1] 91.39348 91.39348 91.39348 91.39348 91.39348 91.39348
```

The result is a numeric vector when neither standard errors nor MCMC
predictions are requested. It can therefore be added directly to the
original data:

``` r

df |>
  select(sex, par, spore) |>
  mutate(expected_longevity = expected_longevity) |>
  head()
#> # A tibble: 6 × 4
#>     sex par   spore expected_longevity
#>   <dbl> <fct> <fct>              <dbl>
#> 1     0 0     0                   91.4
#> 2     0 0     0                   91.4
#> 3     0 0     0                   91.4
#> 4     0 0     0                   91.4
#> 5     0 0     0                   91.4
#> 6     0 0     0                   91.4
```

The same approach applies to any fitted parameter:

``` r

prediction(results, parameter_name = "expt_reproduction", type = "response") |>
  head()
#> [1] 4.705965 4.705965 4.705965 4.705965 4.705965 4.705965

prediction(
  results,
  parameter_name = "reproduction_param2",
  type = "response"
) |>
  head()
#> [1] 0.3542137 0.3542137 0.3542137 0.3542137 0.3542137 0.3542137
```

## Choose the prediction scale

By default, `type = "link"`. This returns the linear predictor on the
internal lifelihood scale:

``` r

prediction(results, parameter_name = "expt_death", type = "link") |>
  head()
#> Lifelihood parameter estimate(s) for males are identical to that of females. Use type='response', to get the right parameter estimate(s) for males on the response scale.
#> [1] -3.028431 -3.028431 -3.028431 -3.028431 -3.028431 -3.028431
```

Use `type = "response"` for values on the parameter’s original scale.
For `expt_death`, these values are expected longevity in the time unit
used by the input data:

``` r

prediction(results, parameter_name = "expt_death", type = "response") |>
  head()
#> [1] 91.39348 91.39348 91.39348 91.39348 91.39348 91.39348
```

When a male ratio parameter is fitted, sex-specific values are applied
only on the response scale. The link-scale prediction therefore emits a
message when the prediction data include males.

## Predict on new data

Pass a data frame to `newdata` to predict for selected combinations of
covariates. It must contain:

- the sex column defined in
  [`as_lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md);
- every covariate in the formula of the requested parameter.

Factor covariates must use levels from the fitted data. Preserving all
original levels is useful even when only a subset is present in
`newdata`.

``` r

newdata <- crossing(par = levels(df$par), sex = c(0, 1)) |>
  mutate(
    par = factor(par, levels = levels(df$par)),
    sex_label = if_else(sex == 0, "female", "male")
  )

sex_predictions <- newdata |>
  mutate(
    expected_longevity = prediction(
      results,
      parameter_name = "expt_death",
      newdata = newdata,
      type = "response"
    )
  )

sex_predictions
#> # A tibble: 6 × 4
#>   par     sex sex_label expected_longevity
#>   <fct> <dbl> <chr>                  <dbl>
#> 1 0         0 female                  91.4
#> 2 0         1 male                   763. 
#> 3 1         0 female                  56.0
#> 4 1         1 male                   468. 
#> 5 2         0 female                  55.0
#> 6 2         1 male                   459.
```

Only `par` is required here because the fitted formula for `expt_death`
is `par`. Other columns from the original dataset are not needed.

## Predictions for females and males

For `expt_death` and `expt_maturity`, lifelihood can fit a corresponding
ratio parameter:

- `ratio_expt_death`;
- `ratio_expt_maturity`.

On the response scale, the prediction for a male is the female
prediction multiplied by the fitted ratio. The previous table therefore
has different values for females and males even when they have the same
`par` level.

We can make this comparison explicit:

``` r

sex_predictions |>
  select(par, sex_label, expected_longevity) |>
  pivot_wider(
    names_from = sex_label,
    values_from = expected_longevity
  ) |>
  mutate(male_to_female = male / female)
#> # A tibble: 3 × 4
#>   par   female  male male_to_female
#>   <fct>  <dbl> <dbl>          <dbl>
#> 1 0       91.4  763.           8.35
#> 2 1       56.0  468.           8.35
#> 3 2       55.0  459.           8.35
```

The ratio itself can also be predicted:

``` r

newdata |>
  mutate(
    longevity_ratio = prediction(
      results,
      parameter_name = "ratio_expt_death",
      newdata = newdata,
      type = "response"
    )
  )
#> Parameter 'ratio_expt_death' set to NA for females.
#> # A tibble: 6 × 4
#>   par     sex sex_label longevity_ratio
#>   <fct> <dbl> <chr>               <dbl>
#> 1 0         0 female              NA   
#> 2 0         1 male                 8.35
#> 3 1         0 female              NA   
#> 4 1         1 male                 8.35
#> 5 2         0 female              NA   
#> 6 2         1 male                 8.35
```

The ratio is `NA` for females because it is only used to modify male
predictions. If the corresponding ratio parameter is not fitted, no
additional male adjustment is applied. Here the estimated ratio is close
to, but not exactly, 10. The model estimates the parameters of a
distribution from interval-censored observations rather than reproducing
the raw multiplier directly.

## Simulate females and males

[`simulate_life_history()`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md)
uses response-scale predictions internally. A mortality-only simulation
therefore uses `ratio_expt_death` for males:

``` r

simulated_mortality <- simulate_life_history(
  results,
  event = "mortality",
  seed = 1
) |>
  mutate(sex = df$sex)

simulated_mortality |>
  summarise(
    mean_simulated_longevity = mean(death_end),
    .by = sex
  ) |>
  mutate(
    sex = if_else(sex == 0, "female", "male"),
    relative_to_female = mean_simulated_longevity /
      first(mean_simulated_longevity)
  )
#> # A tibble: 2 × 3
#>   sex    mean_simulated_longevity relative_to_female
#>   <chr>                     <dbl>              <dbl>
#> 1 female                     82.1               1   
#> 2 male                      692.                8.43
```

The simulated ratio is close to the fitted ratio. It will vary between
simulations unless a fixed `seed` is used.

We can also simulate all fitted events for the same six combinations
used in `newdata`:

``` r

simulated_life_histories <- simulate_life_history(
  results,
  newdata = newdata,
  seed = 1
) |>
  relocate(sex_label, par)

simulated_life_histories |>
  select(
    sex_label,
    par,
    death_end,
    maturity_end,
    clutch_size_1,
    clutch_size_2
  )
#> # A tibble: 6 × 6
#>   sex_label par   death_end maturity_end clutch_size_1 clutch_size_2
#>   <chr>     <fct>     <dbl>        <dbl>         <int>         <int>
#> 1 female    0         121.          9.63             6             4
#> 2 male      0         749.         18.3             NA            NA
#> 3 female    1          68.2        18.0              5             2
#> 4 male      1         362.         13.9             NA            NA
#> 5 female    2          11.2         6.53            NA            NA
#> 6 male      2         486.         14.2             NA            NA
```

Reproduction-related columns are always `NA` for males because
lifelihood does not simulate male reproductive events. For females, a
clutch can also be `NA` when it did not occur before the simulated
death. Mortality is still simulated for both sexes, using the
sex-specific expected longevity.

## Prediction options

The return value changes when uncertainty is requested:

| Arguments | Return value |
|----|----|
| Default | Numeric vector of fitted values |
| `se.fit = TRUE` | Data frame with `fitted` and `se.fitted` |
| `mcmc.fit = TRUE` | Data frame with `fitted`, `mcmc_est`, and `mcmc_se` |
| `mcmc.fit = TRUE, keep_mcmc_samples = TRUE` | List containing `pred` and `mcmc_samples` |

`se.fit = TRUE` requires a model fitted with
`lifelihood(..., se.fit = TRUE)`. Similarly, `mcmc.fit = TRUE` requires
a model fitted with `lifelihood(..., MCMC = n)`, where `n` is greater
than zero.

For example, after fitting with MCMC:

``` r

mcmc_prediction <- prediction(
  results,
  parameter_name = "expt_death",
  newdata = newdata,
  type = "response",
  mcmc.fit = TRUE,
  keep_mcmc_samples = TRUE
)

mcmc_prediction$pred
mcmc_prediction$mcmc_samples
```

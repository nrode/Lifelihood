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

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`lifelihood`](https://nrode.github.io/Lifelihood/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyverse`](https://tidyverse.tidyverse.org)`)`\
\
`df_female`` ``<-`` ``datadaphnia`` ``|>`\
`  `[`as_tibble`](https://tibble.tidyverse.org/reference/as_tibble.html)`(``)`` ``|>`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``par ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``par``)``, spore ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``spore``)``)`\
\
`df_male`` ``<-`` ``df_female`` ``|>`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`    sex ``=`` ``1``,`\
`    `[`across`](https://dplyr.tidyverse.org/reference/across.html)`(`[`starts_with`](https://tidyselect.r-lib.org/reference/starts_with.html)`(``"clutch"``)``, ``~``NA_real_``)``,`\
`    death_start ``=`` ``death_start`` ``*`` ``10``,`\
`    death_end ``=`` ``death_end`` ``*`` ``10`\
`  ``)`\
\
`df`` ``<-`` `[`bind_rows`](https://dplyr.tidyverse.org/reference/bind_rows.html)`(``df_female``, ``df_male``)`` ``|>`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`    block ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(`\
`      `[`c`](https://rdrr.io/r/base/c.html)`(``"female"``, ``"male"``)``,`\
`      `[`c`](https://rdrr.io/r/base/c.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``df_female``)``, `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``df_male``)``)`\
`    ``)`\
`  ``)`\
\
`lifelihood_data`` ``<-`` `[`as_lifelihoodData`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md)`(`\
`  df ``=`` ``df``,`\
`  matclutch ``=`` ``FALSE``,`\
`  sex ``=`` ``"sex"``,`\
`  sex_start ``=`` ``"sex_start"``,`\
`  sex_end ``=`` ``"sex_end"``,`\
`  maturity_start ``=`` ``"mat_start"``,`\
`  maturity_end ``=`` ``"mat_end"``,`\
`  clutchs ``=`` `[`generate_clutch_vector`](https://nrode.github.io/Lifelihood/reference/generate_clutch_vector.md)`(``28``)``,`\
`  death_start ``=`` ``"death_start"``,`\
`  death_end ``=`` ``"death_end"``,`\
`  covariates ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"par"``, ``"spore"``)``,`\
`  block ``=`` ``"block"``,`\
`  dist ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``mortality ``=`` ``"wei"``, maturity ``=`` ``"gam"``, reproduction ``=`` ``"lgn"``)`\
`)`\
\
`results`` ``<-`` `[`lifelihood`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)`(`\
`  lifelihoodData ``=`` ``lifelihood_data``,`\
`  path_config ``=`` `[`use_test_config`](https://nrode.github.io/Lifelihood/reference/use_test_config.md)`(``"config_pierrick"``)``,`\
`  seeds ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``3699``, ``783``, ``5401``, ``6502``)``,`\
`  raise_estimation_warning ``=`` ``FALSE`\
`)`

The mean midpoint of the observed death intervals is exactly 10 times
larger for males because the male rows were constructed that way:

\
`df`` ``|>`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``observed_death ``=`` ``(``death_start`` ``+`` ``death_end``)`` ``/`` ``2``)`` ``|>`\
`  `[`summarise`](https://dplyr.tidyverse.org/reference/summarise.html)`(`\
`    mean_observed_death ``=`` `[`mean`](https://rdrr.io/r/base/mean.html)`(``observed_death``)``,`\
`    .by ``=`` ``sex`\
`  ``)`` ``|>`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`    sex ``=`` `[`if_else`](https://dplyr.tidyverse.org/reference/if_else.html)`(``sex`` ``==`` ``0``, ``"female"``, ``"male"``)``,`\
`    relative_to_female ``=`` ``mean_observed_death`` ``/`` `[`first`](https://dplyr.tidyverse.org/reference/nth.html)`(``mean_observed_death``)`\
`  ``)`\
`#> # A tibble: 2 × 3`\
`#>   sex    mean_observed_death relative_to_female`\
`#>   <chr>                <dbl>              <dbl>`\
`#> 1 female                81.6                1  `\
`#> 2 male                 816.                10.0`

`parameter_name` must identify a parameter fitted by the model. The
available parameters can be read from the results:

\
`results``$``effects`` ``|>`` `[`distinct`](https://dplyr.tidyverse.org/reference/distinct.html)`(``event``, ``parameter``)`\
`#>          event           parameter`\
`#> 1    mortality          expt_death`\
`#> 2    mortality     survival_param2`\
`#> 3    mortality    ratio_expt_death`\
`#> 4     maturity       expt_maturity`\
`#> 5     maturity     maturity_param2`\
`#> 6 reproduction   expt_reproduction`\
`#> 7 reproduction reproduction_param2`\
`#> 8 reproduction         n_offspring`

## Predict for the fitted data

When `newdata` is omitted,
[`prediction()`](https://nrode.github.io/Lifelihood/reference/prediction.md)
returns one value for each individual in the data used to fit the model.

\
`expected_longevity`` ``<-`` `[`prediction`](https://nrode.github.io/Lifelihood/reference/prediction.md)`(`\
`  ``results``,`\
`  parameter_name ``=`` ``"expt_death"``,`\
`  type ``=`` ``"response"`\
`)`\
\
[`length`](https://rdrr.io/r/base/length.html)`(``expected_longevity``)`\
`#> [1] 1100`\
[`head`](https://rdrr.io/r/utils/head.html)`(``expected_longevity``)`\
`#> [1] 91.67703 91.67703 91.67703 91.67703 91.67703 91.67703`

The result is a numeric vector when neither standard errors nor MCMC
predictions are requested. It can therefore be added directly to the
original data:

\
`df`` ``|>`\
`  `[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``sex``, ``par``, ``spore``)`` ``|>`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``expected_longevity ``=`` ``expected_longevity``)`` ``|>`\
`  `[`head`](https://rdrr.io/r/utils/head.html)`(``)`\
`#> # A tibble: 6 × 4`\
`#>     sex par   spore expected_longevity`\
`#>   <dbl> <fct> <fct>              <dbl>`\
`#> 1     0 0     0                   91.7`\
`#> 2     0 0     0                   91.7`\
`#> 3     0 0     0                   91.7`\
`#> 4     0 0     0                   91.7`\
`#> 5     0 0     0                   91.7`\
`#> 6     0 0     0                   91.7`

The same approach applies to any fitted parameter:

\
[`prediction`](https://nrode.github.io/Lifelihood/reference/prediction.md)`(``results``, parameter_name ``=`` ``"expt_reproduction"``, type ``=`` ``"response"``)`` ``|>`\
`  `[`head`](https://rdrr.io/r/utils/head.html)`(``)`\
`#> [1] 4.67445 4.67445 4.67445 4.67445 4.67445 4.67445`\
\
[`prediction`](https://nrode.github.io/Lifelihood/reference/prediction.md)`(`\
`  ``results``,`\
`  parameter_name ``=`` ``"reproduction_param2"``,`\
`  type ``=`` ``"response"`\
`)`` ``|>`\
`  `[`head`](https://rdrr.io/r/utils/head.html)`(``)`\
`#> [1] 0.3561403 0.3561403 0.3561403 0.3561403 0.3561403 0.3561403`

## Choose the prediction scale

By default, `type = "link"`. This returns the linear predictor on the
internal lifelihood scale:

\
[`prediction`](https://nrode.github.io/Lifelihood/reference/prediction.md)`(``results``, parameter_name ``=`` ``"expt_death"``, type ``=`` ``"link"``)`` ``|>`\
`  `[`head`](https://rdrr.io/r/utils/head.html)`(``)`\
`#> Lifelihood parameter estimate(s) for males are identical to that of females. Use type='response', to get the right parameter estimate(s) for males on the response scale.`\
`#> [1] -3.025183 -3.025183 -3.025183 -3.025183 -3.025183 -3.025183`

Use `type = "response"` for values on the parameter’s original scale.
For `expt_death`, these values are expected longevity in the time unit
used by the input data:

\
[`prediction`](https://nrode.github.io/Lifelihood/reference/prediction.md)`(``results``, parameter_name ``=`` ``"expt_death"``, type ``=`` ``"response"``)`` ``|>`\
`  `[`head`](https://rdrr.io/r/utils/head.html)`(``)`\
`#> [1] 91.67703 91.67703 91.67703 91.67703 91.67703 91.67703`

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

\
`newdata`` ``<-`` `[`crossing`](https://tidyr.tidyverse.org/reference/expand.html)`(``par ``=`` `[`levels`](https://rdrr.io/r/base/levels.html)`(``df``$``par``)``, sex ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``1``)``)`` ``|>`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`    par ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``par``, levels ``=`` `[`levels`](https://rdrr.io/r/base/levels.html)`(``df``$``par``)``)``,`\
`    sex_label ``=`` `[`if_else`](https://dplyr.tidyverse.org/reference/if_else.html)`(``sex`` ``==`` ``0``, ``"female"``, ``"male"``)`\
`  ``)`\
\
`sex_predictions`` ``<-`` ``newdata`` ``|>`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`    expected_longevity ``=`` `[`prediction`](https://nrode.github.io/Lifelihood/reference/prediction.md)`(`\
`      ``results``,`\
`      parameter_name ``=`` ``"expt_death"``,`\
`      newdata ``=`` ``newdata``,`\
`      type ``=`` ``"response"`\
`    ``)`\
`  ``)`\
\
`sex_predictions`\
`#> # A tibble: 6 × 4`\
`#>   par     sex sex_label expected_longevity`\
`#>   <fct> <dbl> <chr>                  <dbl>`\
`#> 1 0         0 female                  91.7`\
`#> 2 0         1 male                   759. `\
`#> 3 1         0 female                  57.0`\
`#> 4 1         1 male                   472. `\
`#> 5 2         0 female                  55.4`\
`#> 6 2         1 male                   458.`

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

\
`sex_predictions`` ``|>`\
`  `[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``par``, ``sex_label``, ``expected_longevity``)`` ``|>`\
`  `[`pivot_wider`](https://tidyr.tidyverse.org/reference/pivot_wider.html)`(`\
`    names_from ``=`` ``sex_label``,`\
`    values_from ``=`` ``expected_longevity`\
`  ``)`` ``|>`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``male_to_female ``=`` ``male`` ``/`` ``female``)`\
`#> # A tibble: 3 × 4`\
`#>   par   female  male male_to_female`\
`#>   <fct>  <dbl> <dbl>          <dbl>`\
`#> 1 0       91.7  759.           8.27`\
`#> 2 1       57.0  472.           8.27`\
`#> 3 2       55.4  458.           8.27`

The ratio itself can also be predicted:

\
`newdata`` ``|>`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`    longevity_ratio ``=`` `[`prediction`](https://nrode.github.io/Lifelihood/reference/prediction.md)`(`\
`      ``results``,`\
`      parameter_name ``=`` ``"ratio_expt_death"``,`\
`      newdata ``=`` ``newdata``,`\
`      type ``=`` ``"response"`\
`    ``)`\
`  ``)`\
`#> Parameter 'ratio_expt_death' set to NA for females.`\
`#> # A tibble: 6 × 4`\
`#>   par     sex sex_label longevity_ratio`\
`#>   <fct> <dbl> <chr>               <dbl>`\
`#> 1 0         0 female              NA   `\
`#> 2 0         1 male                 8.27`\
`#> 3 1         0 female              NA   `\
`#> 4 1         1 male                 8.27`\
`#> 5 2         0 female              NA   `\
`#> 6 2         1 male                 8.27`

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

\
`simulated_mortality`` ``<-`` `[`simulate_life_history`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md)`(`\
`  ``results``,`\
`  event ``=`` ``"mortality"``,`\
`  seed ``=`` ``1`\
`)`\
\
`simulated_mortality`` ``|>`\
`  `[`summarise`](https://dplyr.tidyverse.org/reference/summarise.html)`(`\
`    mean_simulated_longevity ``=`` `[`mean`](https://rdrr.io/r/base/mean.html)`(``mortality_end``)``,`\
`    .by ``=`` ``sex`\
`  ``)`` ``|>`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`    sex ``=`` `[`if_else`](https://dplyr.tidyverse.org/reference/if_else.html)`(``sex`` ``==`` ``0``, ``"female"``, ``"male"``)``,`\
`    relative_to_female ``=`` ``mean_simulated_longevity`` ``/`\
`      `[`first`](https://dplyr.tidyverse.org/reference/nth.html)`(``mean_simulated_longevity``)`\
`  ``)`\
`#> # A tibble: 2 × 3`\
`#>   sex    mean_simulated_longevity relative_to_female`\
`#>   <chr>                     <dbl>              <dbl>`\
`#> 1 female                     82.6               1   `\
`#> 2 male                      689.                8.35`

The simulated ratio is close to the fitted ratio. It will vary between
simulations unless a fixed `seed` is used.

We can also simulate all fitted events for the same six combinations
used in `newdata`:

\
`simulated_life_histories`` ``<-`` `[`simulate_life_history`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md)`(`\
`  ``results``,`\
`  newdata ``=`` ``newdata``,`\
`  seed ``=`` ``1`\
`)`` ``|>`\
`  `[`relocate`](https://dplyr.tidyverse.org/reference/relocate.html)`(``sex_label``, ``par``)`\
\
`simulated_life_histories`` ``|>`\
`  `[`select`](https://dplyr.tidyverse.org/reference/select.html)`(`\
`    ``sex_label``,`\
`    ``par``,`\
`    ``mortality_end``,`\
`    ``maturity_end``,`\
`    ``clutch_size_1``,`\
`    ``clutch_size_2`\
`  ``)`\
`#> # A tibble: 6 × 6`\
`#>   sex_label par   mortality_end maturity_end clutch_size_1 clutch_size_2`\
`#>   <chr>     <fct>         <dbl>        <dbl>         <int>         <int>`\
`#> 1 female    0              87.7         9.62             6             4`\
`#> 2 male      0             374.         18.3             NA            NA`\
`#> 3 female    1              24.4        18.0              2            NA`\
`#> 4 male      1             596.         13.9             NA            NA`\
`#> 5 female    2              33.1         6.52             2             4`\
`#> 6 male      2             303.         14.2             NA            NA`

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

\
`mcmc_prediction`` ``<-`` `[`prediction`](https://nrode.github.io/Lifelihood/reference/prediction.md)`(`\
`  ``results``,`\
`  parameter_name ``=`` ``"expt_death"``,`\
`  newdata ``=`` ``newdata``,`\
`  type ``=`` ``"response"``,`\
`  mcmc.fit ``=`` ``TRUE``,`\
`  keep_mcmc_samples ``=`` ``TRUE`\
`)`\
\
`mcmc_prediction``$``pred`\
`mcmc_prediction``$``mcmc_samples`

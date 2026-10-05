# Fitting fitness

\
`devtools``::`[`load_all`](https://devtools.r-lib.org/reference/load_all.html)`(``)`\
`#> ℹ Loading lifelihood`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyverse`](https://tidyverse.tidyverse.org)`)`\
`#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──`\
`#> ✔ dplyr     1.2.1     ✔ readr     2.2.0`\
`#> ✔ forcats   1.0.1     ✔ stringr   1.6.0`\
`#> ✔ ggplot2   4.0.3     ✔ tibble    3.3.1`\
`#> ✔ lubridate 1.9.5     ✔ tidyr     1.3.2`\
`#> ✔ purrr     1.2.2     `\
`#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──`\
`#> ✖ readr::edition_get()   masks testthat::edition_get()`\
`#> ✖ dplyr::filter()        masks lifelihood::filter(), stats::filter()`\
`#> ✖ dplyr::lag()           masks lifelihood::lag(), stats::lag()`\
`#> ✖ readr::local_edition() masks testthat::local_edition()`\
`#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors`\
\
`clutchs`` ``<-`` `[`generate_clutch_vector`](https://nrode.github.io/Lifelihood/reference/generate_clutch_vector.md)`(``6``)`\
\
`lifelihoodData`` ``<-`` `[`as_lifelihoodData`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md)`(`\
`  df ``=`` ``datalenski``,`\
`  matclutch ``=`` ``FALSE``,`\
`  sex ``=`` ``"sex"``,`\
`  sex_start ``=`` ``"sex_start"``,`\
`  sex_end ``=`` ``"sex_end"``,`\
`  maturity_start ``=`` ``"mat_start"``,`\
`  maturity_end ``=`` ``"mat_end"``,`\
`  clutchs ``=`` ``clutchs``,`\
`  death_start ``=`` ``"death_start"``,`\
`  death_end ``=`` ``"death_end"``,`\
`  covariates ``=`` ``"Group"``,`\
`  dist ``=`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``mortality ``=`` ``"wei"``, maturity ``=`` ``"lgn"``, reproduction ``=`` ``"lgn"``)`\
`)`\
\
`config_fitness`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  mortality ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``expt_death ``=`` ``1``, survival_param2 ``=`` ``1``)``,`\
`  maturity ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``expt_maturity ``=`` ``1``, maturity_param2 ``=`` ``1``)``,`\
`  reproduction ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`    expt_reproduction ``=`` ``1``,`\
`    reproduction_param2 ``=`` ``1``,`\
`    fitness ``=`` ``1`\
`  ``)`\
`)`

## Fit lifetime reproductive success

To fit lifetime reproductive success, we need to replace the
`n_offspring` parameter by the `fitness` parameter.

We recommend using MCMC sampling to compute fitness confidence intervals
instead of `se.fit` which is likely to give wrong results.

\
`results`` ``<-`` `[`lifelihood`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)`(`\
`  ``lifelihoodData``,`\
`  config ``=`` ``config_fitness``,`\
`  n_fit ``=`` ``30`\
`)`\
`#> Warning in lifelihood(lifelihoodData, config = config_fitness, n_fit = 30):`\
`#> Best and second-best likelihoods for model row 1 differ by 0.173 (> 0.1).`\
`#> Consider increasing n_fit (currently 30) to be sure of model convergence and`\
`#> find the model with highest log-likelihood.`

## Results

We can then predict fitness and its confidence interval:

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``results``)`\
`#> `\
`#> === LIFELIHOOD RESULTS ===`\
`#> `\
`#> Sample size: 18 `\
`#> `\
`#> --- Model Fit ---`\
`#> Log-likelihood:  -171.173`\
`#> AIC:             356.3`\
`#> BIC:             362.6`\
`#> `\
`#> --- Key Parameters ---`\
`#> `\
`#> Mortality:`\
`#>   expt_death (Intercept)    -0.935 (0.000)`\
`#>   survival_param2 (Intercept) -5.782 (0.000)`\
`#> `\
`#> Maturity:`\
`#>   expt_maturity (Intercept) -0.971 (0.000)`\
`#>   maturity_param2 (Intercept) -3.554 (0.000)`\
`#> `\
`#> Reproduction:`\
`#>   expt_reproduction (Intercept) -3.474 (0.000)`\
`#>   reproduction_param2 (Intercept) -5.094 (0.000)`\
`#>   fitness (Intercept)       -3.878 (0.000)`\
`#> `\
`#> --- Convergence ---`\
`#> All parameters within bounds`\
`#> `\
`#> ======================`\
\
[`prediction`](https://nrode.github.io/Lifelihood/reference/prediction.md)`(``results``, ``"fitness"``, type ``=`` ``"response"``)`\
`#>  [1] 20.27206 20.27206 20.27206 20.27206 20.27206 20.27206 20.27206 20.27206`\
`#>  [9] 20.27206 20.27206 20.27206 20.27206 20.27206 20.27206 20.27206 20.27206`\
`#> [17] 20.27206 20.27206`

## Using simulated data

\
`population`` ``<-`` `[`crossing`](https://tidyr.tidyverse.org/reference/expand.html)`(``group ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``0``)``, sex ``=`` ``0``)`` ``|>`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``n_individuals ``=`` ``100``)`\
\
`effects`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  expt_death ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``intercept ``=`` ``0``)``,`\
`  expt_maturity ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``intercept ``=`` ``0``)``,`\
`  expt_reproduction ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``intercept ``=`` ``0``)``,`\
`  n_offspring ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``intercept ``=`` ``0``)`\
`)`\
\
`config`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  mortality ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``expt_death ``=`` ``1``)``,`\
`  maturity ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``expt_maturity ``=`` ``1``)``,`\
`  reproduction ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`    expt_reproduction ``=`` ``1``,`\
`    n_offspring ``=`` ``1`\
`  ``)`\
`)`\
\
`pseudo_results`` ``<-`` `[`create_simulation_input`](https://nrode.github.io/Lifelihood/reference/create_simulation_input.md)`(`\
`  effects ``=`` ``effects``,`\
`  data ``=`` ``population``,`\
`  covariates ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"group"``)``,`\
`  sex ``=`` ``"sex"``,`\
`  config ``=`` ``config``,`\
`  dist ``=`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``mortality ``=`` ``"exp"``, maturity ``=`` ``"exp"``, reproduction ``=`` ``"exp"``)``,`\
`  n_per_combination ``=`` ``"n_individuals"`\
`)`\
\
`bounds_df`` ``<-`` `[`default_bounds_df`](https://nrode.github.io/Lifelihood/reference/default_bounds_df.md)`(``pseudo_results``$``lifelihoodData``)`\
`bounds_df``$``max``[``bounds_df``$``param`` ``==`` ``"expt_death"``]`` ``<-`` ``110`\
`bounds_df``$``min``[``bounds_df``$``param`` ``==`` ``"expt_death"``]`` ``<-`` ``110`\
`bounds_df``$``max``[``bounds_df``$``param`` ``==`` ``"expt_maturity"``]`` ``<-`` ``10`\
`bounds_df``$``min``[``bounds_df``$``param`` ``==`` ``"expt_maturity"``]`` ``<-`` ``10`\
`bounds_df``$``max``[``bounds_df``$``param`` ``==`` ``"expt_reproduction"``]`` ``<-`` ``5`\
`bounds_df``$``min``[``bounds_df``$``param`` ``==`` ``"expt_reproduction"``]`` ``<-`` ``5`\
`bounds_df``$``max``[``bounds_df``$``param`` ``==`` ``"n_offspring"``]`` ``<-`` ``10`\
`bounds_df``$``min``[``bounds_df``$``param`` ``==`` ``"n_offspring"``]`` ``<-`` ``10`\
\
`pseudo_results``$``param_bounds_df`` ``<-`` ``bounds_df`\
\
`visits`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``block ``=`` ``1``, visit ``=`` `[`seq`](https://rdrr.io/r/base/seq.html)`(``0``, ``1000``, by ``=`` ``0.1``)``)`\
`pseudo_results``$``lifelihoodData``$``block`` ``<-`` ``"block"`\
`pseudo_results``$``lifelihoodData``$``df``$``block`` ``<-`` ``1`\
`simulated_df`` ``<-`` `[`simulate_life_history`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md)`(``pseudo_results``, visits ``=`` ``visits``)`

## Refit

\
`max_n_clutches`` ``<-`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(``simulated_df``$``total_n_clutches``, na.rm ``=`` ``TRUE``)`\
`clutchs`` ``<-`` `[`generate_clutch_vector`](https://nrode.github.io/Lifelihood/reference/generate_clutch_vector.md)`(``max_n_clutches``)`\
\
`lifelihoodData`` ``<-`` `[`as_lifelihoodData`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md)`(`\
`  df ``=`` ``simulated_df``,`\
`  matclutch ``=`` ``FALSE``,`\
`  sex ``=`` ``"sex"``,`\
`  sex_start ``=`` ``"sex_start"``,`\
`  sex_end ``=`` ``"sex_end"``,`\
`  maturity_start ``=`` ``"maturity_start"``,`\
`  maturity_end ``=`` ``"maturity_end"``,`\
`  clutchs ``=`` ``clutchs``,`\
`  death_start ``=`` ``"mortality_start"``,`\
`  death_end ``=`` ``"mortality_end"``,`\
`  covariates ``=`` ``"group"``,`\
`  dist ``=`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``mortality ``=`` ``"exp"``, maturity ``=`` ``"exp"``, reproduction ``=`` ``"exp"``)`\
`)`\
\
`config_fitness`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  mortality ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``expt_death ``=`` ``1``)``,`\
`  maturity ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``expt_maturity ``=`` ``1``)``,`\
`  reproduction ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``expt_reproduction ``=`` ``1``, fitness ``=`` ``1``)`\
`)`\
`config_fitness``$``reproduction``$``fitness`\
`#> [1] 1`\
\
[`default_bounds_df`](https://nrode.github.io/Lifelihood/reference/default_bounds_df.md)`(``lifelihoodData``)`\
`#>                                param   min     max`\
`#> 1                         expt_death 0.001  1438.4`\
`#> 2                    survival_param2  0.05    1000`\
`#> 3                   ratio_expt_death  0.01     100`\
`#> 4                         prob_death 1e-05 0.99999`\
`#> 5                          sex_ratio 1e-05 0.99999`\
`#> 6                      expt_maturity 0.001    72.2`\
`#> 7                    maturity_param2  0.05    1000`\
`#> 8                ratio_expt_maturity  0.01     100`\
`#> 9                  expt_reproduction 0.001  1438.4`\
`#> 10               reproduction_param2  0.05    1000`\
`#> 11                       n_offspring     1      50`\
`#> 12             increase_death_hazard 1e-05      10`\
`#> 13                         tof_decay 1e-07      10`\
`#> 14 increase_death_hazard_n_offspring 1e-07      10`\
`#> 15               lin_decrease_hazard   -20      20`\
`#> 16              quad_decrease_hazard   -10      10`\
`#> 17            lin_change_n_offspring   -10      10`\
`#> 18           quad_change_n_offspring   -10      10`\
`#> 19                   tof_n_offspring   -10      10`\
`#> 20                           fitness 0.001    1000`\
`results`` ``<-`` `[`lifelihood`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)`(`\
`  ``lifelihoodData``,`\
`  config ``=`` ``config_fitness``,`\
`  n_fit ``=`` ``10``,`\
`  delete_temp_files ``=`` ``FALSE`\
`)`\
`#> Warning in lifelihood(lifelihoodData, config = config_fitness, n_fit = 10, :`\
`#> Best and second-best likelihoods for model row 1 differ by 14.351 (> 0.1).`\
`#> Consider increasing n_fit (currently 10) to be sure of model convergence and`\
`#> find the model with highest log-likelihood.`\
\
[`summary`](https://rdrr.io/r/base/summary.html)`(``results``)`\
`#> `\
`#> === LIFELIHOOD RESULTS ===`\
`#> `\
`#> Sample size: 100 `\
`#> `\
`#> --- Model Fit ---`\
`#> Log-likelihood:  -15416.634`\
`#> AIC:             30841.3`\
`#> BIC:             30851.7`\
`#> `\
`#> --- Key Parameters ---`\
`#> `\
`#> Mortality:`\
`#>   expt_death (Intercept)    -2.217 (0.000)`\
`#> `\
`#> Maturity:`\
`#>   expt_maturity (Intercept) -1.777 (0.000)`\
`#> `\
`#> Reproduction:`\
`#>   expt_reproduction (Intercept) -5.648 (0.000)`\
`#>   fitness (Intercept)       -1.057 (0.000)`\
`#> `\
`#> --- Convergence ---`\
`#> All parameters within bounds`\
`#> `\
`#> ======================`\
\
[`prediction`](https://nrode.github.io/Lifelihood/reference/prediction.md)`(`\
`  ``results``,`\
`  `[`c`](https://rdrr.io/r/base/c.html)`(``"expt_death"``, ``"expt_maturity"``, ``"expt_reproduction"``, ``"fitness"``)``,`\
`  type ``=`` ``"response"`\
`)`\
`#> # A tibble: 100 × 4`\
`#>    expt_death expt_maturity expt_reproduction fitness`\
`#>         <dbl>         <dbl>             <dbl>   <dbl>`\
`#>  1       141.          10.4              5.05    258.`\
`#>  2       141.          10.4              5.05    258.`\
`#>  3       141.          10.4              5.05    258.`\
`#>  4       141.          10.4              5.05    258.`\
`#>  5       141.          10.4              5.05    258.`\
`#>  6       141.          10.4              5.05    258.`\
`#>  7       141.          10.4              5.05    258.`\
`#>  8       141.          10.4              5.05    258.`\
`#>  9       141.          10.4              5.05    258.`\
`#> 10       141.          10.4              5.05    258.`\
`#> # ℹ 90 more rows`

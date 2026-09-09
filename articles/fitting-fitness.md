# Fitting fitness

\
`devtools``::`[`load_all`](https://devtools.r-lib.org/reference/load_all.html)`(``)`\
`#> ℹ Loading lifelihood`\
`#> Loading required package: tidyverse`\
`#> `\
`#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──`\
`#> ✔ dplyr     1.2.1     ✔ readr     2.2.0`\
`#> ✔ forcats   1.0.1     ✔ stringr   1.6.0`\
`#> ✔ ggplot2   4.0.3     ✔ tibble    3.3.1`\
`#> ✔ lubridate 1.9.5     ✔ tidyr     1.3.2`\
`#> ✔ purrr     1.2.2     `\
`#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──`\
`#> ✖ dplyr::filter() masks stats::filter()`\
`#> ✖ dplyr::lag()    masks stats::lag()`\
`#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyverse`](https://tidyverse.tidyverse.org)`)`\
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
`  dist ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``mortality ``=`` ``"wei"``, maturity ``=`` ``"lgn"``, reproduction ``=`` ``"lgn"``)`\
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
`)`

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
`#> Log-likelihood:  -171.082`\
`#> AIC:             356.2`\
`#> BIC:             362.4`\
`#> `\
`#> --- Key Parameters ---`\
`#> `\
`#> Mortality:`\
`#>   expt_death (Intercept)    -0.986 (0.000)`\
`#>   survival_param2 (Intercept) -5.781 (0.000)`\
`#> `\
`#> Maturity:`\
`#>   expt_maturity (Intercept) -0.961 (0.000)`\
`#>   maturity_param2 (Intercept) -3.548 (0.000)`\
`#> `\
`#> Reproduction:`\
`#>   expt_reproduction (Intercept) -3.679 (0.000)`\
`#>   reproduction_param2 (Intercept) -5.561 (0.000)`\
`#>   fitness (Intercept)       -3.734 (0.000)`\
`#> `\
`#> --- Convergence ---`\
`#> All parameters within bounds`\
`#> `\
`#> ======================`\
\
[`prediction`](https://nrode.github.io/Lifelihood/reference/prediction.md)`(``results``, ``"fitness"``, type ``=`` ``"response"``)`\
`#>  [1] 23.34324 23.34324 23.34324 23.34324 23.34324 23.34324 23.34324 23.34324`\
`#>  [9] 23.34324 23.34324 23.34324 23.34324 23.34324 23.34324 23.34324 23.34324`\
`#> [17] 23.34324 23.34324`

## Using simulated data

\
`population`` ``<-`` `[`crossing`](https://tidyr.tidyverse.org/reference/expand.html)`(``group ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``0``)``, sex ``=`` ``0``)`` ``|>`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``n_individuals ``=`` ``100``)`\
\
`effects`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  expt_death ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``intercept ``=`` ``0``)``,`\
`  survival_param2 ``=`` ``0``,`\
`  expt_maturity ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``intercept ``=`` ``0``)``,`\
`  maturity_param2 ``=`` ``0``,`\
`  expt_reproduction ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``intercept ``=`` ``0``)``,`\
`  reproduction_param2 ``=`` ``0``,`\
`  n_offspring ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``intercept ``=`` ``0``)`\
`)`\
\
`config`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  mortality ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``expt_death ``=`` ``1``, survival_param2 ``=`` ``1``)``,`\
`  maturity ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``expt_maturity ``=`` ``1``, maturity_param2 ``=`` ``1``)``,`\
`  reproduction ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`    expt_reproduction ``=`` ``1``,`\
`    reproduction_param2 ``=`` ``1``,`\
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
`  dist ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``mortality ``=`` ``"exp"``, maturity ``=`` ``"exp"``, reproduction ``=`` ``"exp"``)``,`\
`  n_per_combination ``=`` ``"n_individuals"`\
`)`\
\
`bounds_df`` ``<-`` `[`default_bounds_df`](https://nrode.github.io/Lifelihood/reference/default_bounds_df.md)`(``pseudo_results``$``lifelihoodData``)`\
`bounds_df``$``max``[``bounds_df``$``param`` ``==`` ``"expt_death"``]`` ``<-`` ``110`\
`bounds_df``$``min``[``bounds_df``$``param`` ``==`` ``"expt_death"``]`` ``<-`` ``110`\
`bounds_df``$``max``[``bounds_df``$``param`` ``==`` ``"survival_param2"``]`` ``<-`` ``2`\
`bounds_df``$``max``[``bounds_df``$``param`` ``==`` ``"expt_maturity"``]`` ``<-`` ``10`\
`bounds_df``$``min``[``bounds_df``$``param`` ``==`` ``"expt_maturity"``]`` ``<-`` ``10`\
`bounds_df``$``max``[``bounds_df``$``param`` ``==`` ``"maturity_param2"``]`` ``<-`` ``2`\
`bounds_df``$``max``[``bounds_df``$``param`` ``==`` ``"expt_reproduction"``]`` ``<-`` ``5`\
`bounds_df``$``min``[``bounds_df``$``param`` ``==`` ``"expt_reproduction"``]`` ``<-`` ``5`\
`bounds_df``$``max``[``bounds_df``$``param`` ``==`` ``"reproduction_param2"``]`` ``<-`` ``2`\
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
`  dist ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``mortality ``=`` ``"exp"``, maturity ``=`` ``"exp"``, reproduction ``=`` ``"exp"``)`\
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
`#> 1                         expt_death 0.001  1281.2`\
`#> 2                    survival_param2  0.05    1000`\
`#> 3                   ratio_expt_death  0.01     100`\
`#> 4                         prob_death 1e-05 0.99999`\
`#> 5                          sex_ratio 1e-05 0.99999`\
`#> 6                      expt_maturity 0.001   101.6`\
`#> 7                    maturity_param2  0.05    1000`\
`#> 8                ratio_expt_maturity  0.01     100`\
`#> 9                  expt_reproduction 0.001  1281.2`\
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
`#> Best and second-best likelihoods differ by 9.06 (> 0.1). Consider increasing`\
`#> n_fit (currently 10) to be sure of model convergence and find the model with`\
`#> highest log-likelihood.`\
\
[`summary`](https://rdrr.io/r/base/summary.html)`(``results``)`\
`#> `\
`#> === LIFELIHOOD RESULTS ===`\
`#> `\
`#> Sample size: 100 `\
`#> `\
`#> --- Model Fit ---`\
`#> Log-likelihood:  -16165.606`\
`#> AIC:             32339.2`\
`#> BIC:             32349.6`\
`#> `\
`#> --- Key Parameters ---`\
`#> `\
`#> Mortality:`\
`#>   expt_death (Intercept)    -2.307 (0.000)`\
`#> `\
`#> Maturity:`\
`#>   expt_maturity (Intercept) -2.180 (0.000)`\
`#> `\
`#> Reproduction:`\
`#>   expt_reproduction (Intercept) -5.474 (0.000)`\
`#>   fitness (Intercept)       -1.379 (0.000)`\
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
`#>  1       116.          10.3              5.35    201.`\
`#>  2       116.          10.3              5.35    201.`\
`#>  3       116.          10.3              5.35    201.`\
`#>  4       116.          10.3              5.35    201.`\
`#>  5       116.          10.3              5.35    201.`\
`#>  6       116.          10.3              5.35    201.`\
`#>  7       116.          10.3              5.35    201.`\
`#>  8       116.          10.3              5.35    201.`\
`#>  9       116.          10.3              5.35    201.`\
`#> 10       116.          10.3              5.35    201.`\
`#> # ℹ 90 more rows`

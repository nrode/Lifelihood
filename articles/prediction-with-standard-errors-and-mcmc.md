# Standard errors and MCMC

## Create a `lifelihoodData` object

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`lifelihood`](https://nrode.github.io/Lifelihood/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyverse`](https://tidyverse.tidyverse.org)`)`\
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
\
`df`` ``<-`` ``datapierrick`` ``|>`\
`  `[`as_tibble`](https://tibble.tidyverse.org/reference/as_tibble.html)`(``)`` ``|>`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`    par ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``par``)``,`\
`    geno ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``geno``)``,`\
`    spore ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``spore``)``,`\
`    block ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``1``:``2``, each ``=`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``datapierrick``)`` ``/`` ``2``)`\
`  ``)`\
\
`clutchs`` ``<-`` `[`generate_clutch_vector`](https://nrode.github.io/Lifelihood/reference/generate_clutch_vector.md)`(``28``)`\
\
`lifelihoodData`` ``<-`` `[`as_lifelihoodData`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md)`(`\
`  df ``=`` ``df``,`\
`  matclutch ``=`` ``FALSE``,`\
`  sex ``=`` ``"sex"``,`\
`  sex_start ``=`` ``"sex_start"``,`\
`  sex_end ``=`` ``"sex_end"``,`\
`  maturity_start ``=`` ``"mat_start"``,`\
`  maturity_end ``=`` ``"mat_end"``,`\
`  clutchs ``=`` ``clutchs``,`\
`  block ``=`` ``"block"``,`\
`  death_start ``=`` ``"death_start"``,`\
`  death_end ``=`` ``"death_end"``,`\
`  covariates ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"par"``, ``"spore"``)``,`\
`  dist ``=`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``mortality ``=`` ``"wei"``, maturity ``=`` ``"gam"``, reproduction ``=`` ``"lgn"``)`\
`)`\
\
`config_se`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  mortality ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``expt_death ``=`` ``"par"``, survival_param2 ``=`` ``1``)`\
`)`\
\
`config_mcmc`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  mortality ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``expt_death ``=`` ``"par"``, survival_param2 ``=`` ``1``)``,`\
`  maturity ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``expt_maturity ``=`` ``"par"``, maturity_param2 ``=`` ``1``)``,`\
`  reproduction ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`    expt_reproduction ``=`` ``"par"``,`\
`    reproduction_param2 ``=`` ``1``,`\
`    fitness ``=`` ``1`\
`  ``)`\
`)`

## Standard errors

By default, lifelihood will not try to fit standard errors. But, you can
use the `se.fit` argument for this purpose:

\
\
`## Fail to compute standard errors due to absence of convergence to the ML optimum`\
`results_wrong`` ``<-`` `[`lifelihood`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)`(`\
`  lifelihoodData ``=`` ``lifelihoodData``,`\
`  config ``=`` ``config_se``,`\
`  se.fit ``=`` ``TRUE``,`\
`  seed ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``103``, ``349``, ``1213``, ``1283``)`\
`)`\
\
`## Change sees to converge to the ML optimum`\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
`results`` ``<-`` `[`lifelihood`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)`(`\
`  lifelihoodData ``=`` ``lifelihoodData``,`\
`  config ``=`` ``config_se``,`\
`  se.fit ``=`` ``TRUE``,`\
`  n_fit ``=`` ``5`\
`)`\
\
`## New model has better convergence`\
[`logLik`](https://rdrr.io/r/stats/logLik.html)`(``results_wrong``)`\
`#> [1] -2497.344`\
[`logLik`](https://rdrr.io/r/stats/logLik.html)`(``results``)`\
`#> [1] -2497.366`\
\
[`summary`](https://rdrr.io/r/base/summary.html)`(``results``)`\
`#> `\
`#> === LIFELIHOOD RESULTS ===`\
`#> `\
`#> Sample size: 550 `\
`#> `\
`#> --- Model Fit ---`\
`#> Log-likelihood:  -2497.366`\
`#> AIC:             5002.7`\
`#> BIC:             5020.0`\
`#> `\
`#> --- Key Parameters ---`\
`#> `\
`#> Mortality:`\
`#>   expt_death (Intercept)    -0.916 (0.020)`\
`#>   expt_death eff_expt_death_par_1 -0.780 (0.037)`\
`#>   expt_death eff_expt_death_par_2 -0.806 (0.052)`\
`#>   survival_param2 (Intercept) -4.884 (0.034)`\
`#> `\
`#> --- Convergence ---`\
`#> All parameters within bounds`\
`#> `\
`#> ======================`

Now if we have a look at the estimations we have standard errors:

\
`results``$``effects`` ``|>`` `[`as_tibble`](https://tibble.tidyverse.org/reference/as_tibble.html)`(``)`\
`#> # A tibble: 4 × 6`\
`#>   name                 estimation stderror parameter       kind            event`\
`#>   <chr>                     <dbl>    <dbl> <chr>           <chr>           <chr>`\
`#> 1 int_expt_death           -0.916   0.0201 expt_death      intercept       mort…`\
`#> 2 eff_expt_death_par_1     -0.780   0.0367 expt_death      coefficient_ca… mort…`\
`#> 3 eff_expt_death_par_2     -0.806   0.0517 expt_death      coefficient_ca… mort…`\
`#> 4 int_survival_param2      -4.88    0.0345 survival_param2 intercept       mort…`

## Prediction

We can predict with standard errors.

- Default scale

\
[`prediction`](https://nrode.github.io/Lifelihood/reference/prediction.md)`(``results``, ``"expt_death"``, se.fit ``=`` ``TRUE``)`` ``|>`\
`  `[`as_tibble`](https://tibble.tidyverse.org/reference/as_tibble.html)`(``)`` ``|>`\
`  `[`sample_n`](https://dplyr.tidyverse.org/reference/sample_n.html)`(``5``)`\
`#> Lifelihood parameter estimate(s) for males are identical to that of females. Use type='response', to get the right parameter estimate(s) for males on the response scale.`\
`#> # A tibble: 5 × 2`\
`#>   fitted se.fitted`\
`#>    <dbl>     <dbl>`\
`#> 1 -0.916    0.0201`\
`#> 2 -0.916    0.0201`\
`#> 3 -0.916    0.0201`\
`#> 4 -0.916    0.0201`\
`#> 5 -0.916    0.0201`

- Response scale

\
[`prediction`](https://nrode.github.io/Lifelihood/reference/prediction.md)`(``results``, ``"expt_death"``, type ``=`` ``"response"``, se.fit ``=`` ``TRUE``)`` ``|>`\
`  `[`as_tibble`](https://tibble.tidyverse.org/reference/as_tibble.html)`(``)`` ``|>`\
`  `[`sample_n`](https://dplyr.tidyverse.org/reference/sample_n.html)`(``5``)`\
`#> # A tibble: 5 × 2`\
`#>   fitted se.fitted`\
`#>    <dbl>     <dbl>`\
`#> 1   92.6      1.33`\
`#> 2   50.2      1.37`\
`#> 3   92.6      1.33`\
`#> 4   92.6      1.33`\
`#> 5   50.2      1.37`

## SE fails with interaction model with MCMC

> MCMC stands for **M**arkov **C**hain **M**onte **C**arlo.

### Fitting with MCMC

\
`results`` ``<-`` `[`lifelihood`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)`(`\
`  lifelihoodData ``=`` ``lifelihoodData``,`\
`  config ``=`` ``config_mcmc``,`\
`  MCMC ``=`` ``30`\
`)`

### Visualization

We can represent the confidence interval computed thanks to the standard
errors:

\
[`plot_fitted_event_rate`](https://nrode.github.io/Lifelihood/reference/plot_event_rate.md)`(`\
`  ``results``,`\
`  interval_width ``=`` ``10``,`\
`  event ``=`` ``"mortality"``,`\
`  use_facet ``=`` ``TRUE``,`\
`  groupby ``=`` ``"par"``,`\
`  xlab ``=`` ``"Age (days)"``,`\
`  ylab ``=`` ``"Fitted Mortality Rate"``,`\
`  se.fit ``=`` ``TRUE`\
`)`\
`#> Warning in value[[3L]](cond): Could not compute MCMC standard errors. Number of`\
`#> MCMC iterations (30) must be higher than number of individuals (48).`\
`#> Warning in value[[3L]](cond): Could not compute MCMC standard errors. Number of`\
`#> MCMC iterations (30) must be higher than number of individuals (48).`\
`#> Warning: Removed 20 rows containing missing values or values outside the scale range`\
`` #> (`geom_point()`). ``

![](prediction-with-standard-errors-and-mcmc_files/figure-html/unnamed-chunk-6-1.png)

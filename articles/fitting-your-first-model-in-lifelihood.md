# Fitting your first model in lifelihood

If you haven’t check it yet, have a look at:

- [what is the required data format to work with
  lifelihood?](https://nrode.github.io/Lifelihood/articles/required-data-format.md)
- [setting up the configuration
  file](https://nrode.github.io/Lifelihood/articles/setting-up-the-configuration-file.md)

## Load libraries

------------------------------------------------------------------------

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
`#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors`

## Data preparation

------------------------------------------------------------------------

Load the dataset from `.csv` file:

\
`# input data`\
`df`` ``<-`` ``datapierrick`` ``|>`\
`  `[`as_tibble`](https://tibble.tidyverse.org/reference/as_tibble.html)`(``)`` ``|>`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``par ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``par``)``, geno ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``geno``)``, spore ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``spore``)``)`\
\
`df`` ``|>`` `[`head`](https://rdrr.io/r/utils/head.html)`(``)`\
`#> # A tibble: 6 × 95`\
`#>   par   geno  spore sex_start sex_end   sex mat_start mat_end   mat`\
`#>   <fct> <fct> <fct>     <int>   <int> <int>     <int>   <int> <int>`\
`#> 1 0     0     0            13    1000     0        12      13     6`\
`#> 2 0     0     0            13    1000     0        12      13     3`\
`#> 3 0     0     0            15    1000     0        14      15     1`\
`#> 4 0     0     0            14    1000     0        13      14     6`\
`#> 5 0     0     0            19    1000     0        18      19     2`\
`#> 6 0     0     0            12    1000     0        11      12     1`\
`#> # ℹ 86 more variables: clutch_start_1 <int>, clutch_end_1 <int>,`\
`#> #   clutch_size_1 <int>, clutch_start_2 <int>, clutch_end_2 <int>,`\
`#> #   clutch_size_2 <int>, clutch_start_3 <int>, clutch_end_3 <int>,`\
`#> #   clutch_size_3 <int>, clutch_start_4 <int>, clutch_end_4 <int>,`\
`#> #   clutch_size_4 <int>, clutch_start_5 <int>, clutch_end_5 <int>,`\
`#> #   clutch_size_5 <int>, clutch_start_6 <int>, clutch_end_6 <int>,`\
`#> #   clutch_size_6 <int>, clutch_start_7 <int>, clutch_end_7 <int>, …`

Prepare arguments for the
[`as_lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md)
function:

\
`# name of the columns of the clutchs into a single vector`\
`clutchs`` ``<-`` `[`generate_clutch_vector`](https://nrode.github.io/Lifelihood/reference/generate_clutch_vector.md)`(``28``)`

*Note: If you have a large number of clutches, it is easier to generate
this vector programmatically. See the [Generate clutch
names](https://nrode.github.io/Lifelihood/articles/generate-clutch-names.md)
vignette.*

## Create the `lifelihoodData` object

[`as_lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md)
creates a `lifelihoodData` object, which is a list containing all the
information needed to run the lifelihood program of a given dataset of
individual life history.

This function mostly takes as input your dataset, your column names.

The `dist` argument is a data frame with exactly three columns:
`mortality`, `maturity`, and `reproduction`. Each row specifies one
model to fit. Each cell must contain `"wei"` (Weibull), `"exp"`
(exponential), `"gam"` (gamma), or `"lgn"` (log-normal). Use a one-row
data frame to fit a single model, as below.

\
`dataLFH`` ``<-`` `[`as_lifelihoodData`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md)`(`\
`  df ``=`` ``df``,`\
`  sex ``=`` ``"sex"``,`\
`  sex_start ``=`` ``"sex_start"``,`\
`  sex_end ``=`` ``"sex_end"``,`\
`  maturity_start ``=`` ``"mat_start"``,`\
`  maturity_end ``=`` ``"mat_end"``,`\
`  clutchs ``=`` ``clutchs``,`\
`  death_start ``=`` ``"death_start"``,`\
`  death_end ``=`` ``"death_end"``,`\
`  matclutch ``=`` ``FALSE``,`\
`  covariates ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"par"``, ``"geno"``)``,`\
`  dist ``=`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``mortality ``=`` ``"wei"``, maturity ``=`` ``"gam"``, reproduction ``=`` ``"lgn"``)`\
`)`

## Get the results

------------------------------------------------------------------------

### All default parameters

Once you have created your `lifelihoodData` object with
[`as_lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md),
you can call the
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
function to run the lifelihood program.

It returns a `lifelihoodResults` object, which is a list containing all
the results of the analysis.

Here it’s a minimalist usage of the function, where we only specify the
`lifelihoodData` object, the configuration and the seeds to use (four
integers used by Mersenne Twister pseudorandom number generator of the
lifelihood program). The `raise_estimation_warning` argument will be the
focus of the [next
vignette](https://nrode.github.io/Lifelihood/articles/4-custom-param-boundaries-and-estimation-warning.md).

\
`config`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  mortality ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``expt_death ``=`` ``"par"``, survival_param2 ``=`` ``1``, ratio_expt_death ``=`` ``1``)``,`\
`  maturity ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``expt_maturity ``=`` ``1``, maturity_param2 ``=`` ``1``)``,`\
`  reproduction ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`    expt_reproduction ``=`` ``1``,`\
`    reproduction_param2 ``=`` ``1``,`\
`    n_offspring ``=`` ``1`\
`  ``)`\
`)`\
\
`results`` ``<-`` `[`lifelihood`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)`(`\
`  lifelihoodData ``=`` ``dataLFH``,`\
`  config ``=`` ``config``,`\
`  seeds ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``2``, ``3``, ``4``)``,`\
`  raise_estimation_warning ``=`` ``FALSE`\
`)`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``results``)`\
`#> `\
`#> === LIFELIHOOD RESULTS ===`\
`#> `\
`#> Sample size: 550 `\
`#> `\
`#> --- Model Fit ---`\
`#> Log-likelihood:  -31598.613`\
`#> AIC:             63217.2`\
`#> BIC:             63260.3`\
`#> `\
`#> --- Key Parameters ---`\
`#> `\
`#> Mortality:`\
`#>   expt_death (Intercept)    -0.895 (0.000)`\
`#>   expt_death eff_expt_death_par_1 -1.821 (0.000)`\
`#>   expt_death eff_expt_death_par_2 -1.840 (0.000)`\
`#>   survival_param2 (Intercept) -4.866 (0.000)`\
`#>   ratio_expt_death (Intercept) -3.668 (0.000)`\
`#> `\
`#> Maturity:`\
`#>   expt_maturity (Intercept) -1.494 (0.000)`\
`#>   maturity_param2 (Intercept) -6.034 (0.000)`\
`#> `\
`#> Reproduction:`\
`#>   expt_reproduction (Intercept) -4.234 (0.000)`\
`#>   reproduction_param2 (Intercept) -3.155 (0.000)`\
`#>   n_offspring (Intercept)   -2.581 (0.000)`\
`#> `\
`#> --- Convergence ---`\
`#> All parameters within bounds`\
`#> `\
`#> ======================`

## Get specific results

------------------------------------------------------------------------

The `lifelihoodResults` object is a list containing all the results of
the analysis. We can get specific results by calling the list element.

\
[`coef`](https://nrode.github.io/Lifelihood/reference/coef.md)`(``results``)`\
`#>          int_expt_death    eff_expt_death_par_1    eff_expt_death_par_2 `\
`#>              -0.8945988              -1.8210310              -1.8399792 `\
`#>     int_survival_param2    int_ratio_expt_death       int_expt_maturity `\
`#>              -4.8656529              -3.6679045              -1.4938977 `\
`#>     int_maturity_param2   int_expt_reproduction int_reproduction_param2 `\
`#>              -6.0343392              -4.2344424              -3.1554742 `\
`#>         int_n_offspring `\
`#>              -2.5814995`\
[`coeff`](https://nrode.github.io/Lifelihood/reference/coef.md)`(``results``, ``"expt_death"``)`\
`#>       int_expt_death eff_expt_death_par_1 eff_expt_death_par_2 `\
`#>           -0.8945988           -1.8210310           -1.8399792`\
[`coeff`](https://nrode.github.io/Lifelihood/reference/coef.md)`(``results``, ``"survival_param2"``)`\
`#> int_survival_param2 `\
`#>           -4.865653`\
\
[`AIC`](https://rdrr.io/r/stats/AIC.html)`(``results``)`\
`#> [1] 63217.23`\
[`BIC`](https://rdrr.io/r/stats/AIC.html)`(``results``)`\
`#> [1] 63260.33`\
\
[`logLik`](https://rdrr.io/r/stats/logLik.html)`(``results``)`\
`#> [1] -31598.61`\
\
[`prediction`](https://nrode.github.io/Lifelihood/reference/prediction.md)`(``results``, parameter_name ``=`` ``"expt_death"``)`` ``|>`` `[`head`](https://rdrr.io/r/utils/head.html)`(``)`\
`#> Lifelihood parameter estimate(s) for males are identical to that of females. Use type='response', to get the right parameter estimate(s) for males on the response scale.`\
`#> [1] -0.8945988 -0.8945988 -0.8945988 -0.8945988 -0.8945988 -0.8945988`\
[`prediction`](https://nrode.github.io/Lifelihood/reference/prediction.md)`(``results``, parameter_name ``=`` ``"expt_death"``, type ``=`` ``"response"``)`` ``|>`` `[`head`](https://rdrr.io/r/utils/head.html)`(``)`\
`#> [1] 94.0131 94.0131 94.0131 94.0131 94.0131 94.0131`

## Fit several models at once

------------------------------------------------------------------------

To compare distribution families, supply one row per model. For example,
the following fits a Weibull/gamma/log-normal model and an exponential
model:

\
`data_multiple`` ``<-`` ``dataLFH`\
`data_multiple``$``dist`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  mortality ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"wei"``, ``"exp"``)``,`\
`  maturity ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"gam"``, ``"exp"``)``,`\
`  reproduction ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"lgn"``, ``"exp"``)`\
`)`\
\
`results_multiple`` ``<-`` `[`lifelihood`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)`(`\
`  lifelihoodData ``=`` ``data_multiple``,`\
`  config ``=`` ``config``,`\
`  n_fit ``=`` ``2``,`\
`  raise_estimation_warning ``=`` ``FALSE`\
`)`\
`#> Warning in lifelihood(lifelihoodData = data_multiple, config = config, n_fit =`\
`#> 2, : Best and second-best likelihoods for model row 1 differ by 16.545 (> 0.1).`\
`#> Consider increasing n_fit (currently 2) to be sure of model convergence and`\
`#> find the model with highest log-likelihood.`\
`#> Warning in lifelihood(lifelihoodData = data_multiple, config = config, n_fit =`\
`#> 2, : Best and second-best likelihoods for model row 2 differ by 0.554 (> 0.1).`\
`#> Consider increasing n_fit (currently 2) to be sure of model convergence and`\
`#> find the model with highest log-likelihood.`\
\
`comparison`` ``<-`` `[`summary`](https://rdrr.io/r/base/summary.html)`(``results_multiple``$``all_models``)`\
`comparison`\
`#> # A tibble: 4 × 20`\
`#>   fit          seeds dist_maturity dist_reproduction dist_mortality n_parameters`\
`#>   <chr>        <chr> <chr>         <chr>             <chr>                 <int>`\
`#> 1 lifelihood_… 5954… gam           lgn               wei                      10`\
`#> 2 lifelihood_… 154_… gam           lgn               wei                      10`\
`#> 3 lifelihood_… 1609… exp           exp               exp                       7`\
`#> 4 lifelihood_… 825_… exp           exp               exp                       7`\
`#> # ℹ 14 more variables: likelihood <dbl>, AIC <dbl>, AICc <dbl>, ΔAICc <dbl>,`\
`#> #   int_expt_death <dbl>, eff_expt_death_par_1 <dbl>,`\
`#> #   eff_expt_death_par_2 <dbl>, int_survival_param2 <dbl>,`\
`#> #   int_ratio_expt_death <dbl>, int_expt_maturity <dbl>,`\
`#> #   int_maturity_param2 <dbl>, int_expt_reproduction <dbl>,`\
`#> #   int_reproduction_param2 <dbl>, int_n_offspring <dbl>`

`n_fit` applies to every row of `dist`, so this example runs four fits.
Each replicate receives fresh random seeds. Leave `seeds = NULL` when
`n_fit > 1`; with `n_fit = 1`, explicit seeds are reused for each model.

All models share the configuration. In a batch with multiple rows,
second parameters (`survival_param2`, `maturity_param2`, and
`reproduction_param2`) are automatically set to `"not_fitted"` for
exponential events, which have no second parameter. Other events keep
their configured formulas. For a single-model fit, an exponential event
with a fitted second parameter raises a warning and that parameter is
disabled before fitting.

`results_multiple` contains the fit with the highest log-likelihood. The
comparison table includes every model and replicate, sorted by AICc,
with the difference from the lowest AICc in its `ΔAICc` column. The
first row of this table can differ from the model with the highest
log-likelihood because AICc accounts for the number of parameters. Use
the `fit` column to retrieve a particular fit:

\
`best_aicc`` ``<-`` ``results_multiple``$``all_models``[[``comparison``$``fit``[``1``]``]``]`\
`best_aicc``$``dist`\
`#>   mortality maturity reproduction`\
`#> 1       wei      gam          lgn`

Each fit stores only its own row of `dist`; prediction, simulation, and
goodness-of-fit therefore use that model’s distributions. Batch fitting
also works with `group_by_group = TRUE`. When default parameter
boundaries are used, they are computed separately for each model. A
supplied `param_bounds_df` applies to every model in the batch.

## Next step

------------------------------------------------------------------------

Now that you have seen how to use the package, you can go further and
[customise your parameter boundaries and deal with estimation
warnings](https://nrode.github.io/Lifelihood/articles/customize-parameter-boundaries-and-estimation-warning.md).

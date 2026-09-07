# Customize parameter boundaries and estimation warning

If you haven’t check it yet, have a look at:

- [What is the required data format to work with
  lifelihood?](https://nrode.github.io/Lifelihood/articles/required-data-format.md)
- [Setting up the configuration
  file](https://nrode.github.io/Lifelihood/articles/setting-up-the-configuration-file.md)
- [How to use the lifelihood
  package](https://nrode.github.io/Lifelihood/articles/fitting-your-first-model-in-lifelihood.md)

## Load libraries

------------------------------------------------------------------------

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`lifelihood`](https://nrode.github.io/Lifelihood/)`)`\
`#> Loading required package: tidyverse`\
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
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyverse`](https://tidyverse.tidyverse.org)`)`

## Data preparation

------------------------------------------------------------------------

Load the dataset from `.csv` file:

\
`# input data`\
`df`` ``<-`` ``fakesample`` ``|>`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`    type ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``type``)``,`\
`    geno ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``geno``)`\
`  ``)`

\
`df`` ``|>`` `[`head`](https://rdrr.io/r/utils/head.html)`(``)`\
`#>   type geno sex_start sex_end sex mat_start mat_end clutch_start1 clutch_end1`\
`#> 1    1    0         0    1000   0         0    1000            NA          NA`\
`#> 2    2    0         0    1000   0         2       3             4           3`\
`#> 3    0    1         0    1000   0         3       4             2           4`\
`#> 4    0    1         0    1000   0         3       4             2           4`\
`#> 5    0    1         0    1000   0         3       4             2           4`\
`#> 6    1    0         0    1000   0         0    1000            NA          NA`\
`#>   clutch_size1 clutch_start2 clutch_end2 clutch_size2 death_start death_end`\
`#> 1           NA            NA          NA           NA         0.1         1`\
`#> 2            4             2           4            5         9.0        10`\
`#> 3            5            NA          NA           NA         5.0         6`\
`#> 4            5            NA          NA           NA         5.0         6`\
`#> 5            5            NA          NA           NA         5.0         6`\
`#> 6           NA            NA          NA           NA         0.1         1`

Prepare input parameters for the
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
function:

\
`# name of the columns of the clutchs into a single vector`\
`clutchs`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`  ``"clutch_start1"``,`\
`  ``"clutch_end1"``,`\
`  ``"clutch_size1"``,`\
`  ``"clutch_start2"``,`\
`  ``"clutch_end2"``,`\
`  ``"clutch_size2"`\
`)`

*Note: If you have a large number of clutches, it is easier to generate
this vector programmatically, particularly if your dataset contains a
large number of clutches. See the [Generate clutch
names](https://nrode.github.io/Lifelihood/articles/generate-clutch-names.md)
vignette.*

## Create the `lifelihoodData` object

\
`dataLFH`` ``<-`` `[`as_lifelihoodData`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md)`(`\
`  df ``=`` ``df``,`\
`  matclutch ``=`` ``FALSE``,`\
`  sex ``=`` ``"sex"``,`\
`  sex_start ``=`` ``"sex_start"``,`\
`  sex_end ``=`` ``"sex_end"``,`\
`  maturity_start ``=`` ``"mat_start"``,`\
`  maturity_end ``=`` ``"mat_end"``,`\
`  clutchs ``=`` ``clutchs``,`\
`  death_start ``=`` ``"death_start"``,`\
`  death_end ``=`` ``"death_end"``,`\
`  covariates ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"geno"``, ``"type"``)``,`\
`  dist ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``mortality ``=`` ``"gam"``, maturity ``=`` ``"lgn"``, reproduction ``=`` ``"wei"``)`\
`)`

## Get the results

------------------------------------------------------------------------

Let’s run the analysis with default parameters.

\
`results`` ``<-`` `[`lifelihood`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)`(`\
`  lifelihoodData ``=`` ``dataLFH``,`\
`  path_config ``=`` `[`use_test_config`](https://nrode.github.io/Lifelihood/reference/use_test_config.md)`(``"config"``)`\
`)`

## Warning

------------------------------------------------------------------------

### What is it?

When runnning
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
function, you might encounter the following warning:

\
`## Warning in check_estimation(results_lifelihood = results): Estimation of`\
`## 'increase_death_hazard' is close to the maximum bound:`\
`## increase_death_hazard≃9.9864992332278. Consider increasing maximum bound.`

This warning indicates that the estimation of the
`increase_death_hazard` parameter is close to the maximum bound.

This is not an error, but a signal that the estimation of the
`increase_death_hazard` parameter is approaching its upper limit. During
the optimization process, the algorithm may have been constrained by the
current parameter boundaries, potentially affecting the accuracy of the
estimation.

To address this warning, you can consider increasing the maximum bound
of the `increase_death_hazard` parameter. This will give the
optimization algorithm more flexibility to find the best estimate.

### Customize parameter boundaries

You can get the **default** parameter boundaries with the
[`default_bounds_df()`](https://nrode.github.io/Lifelihood/reference/default_bounds_df.md)
function and by passing the `lifelihoodData` object:

\
`bounds_df`` ``<-`` `[`default_bounds_df`](https://nrode.github.io/Lifelihood/reference/default_bounds_df.md)`(``dataLFH``)`\
`bounds_df`\
`#>                                param    min     max`\
`#> 1                         expt_death  0.001      40`\
`#> 2                    survival_param2  0.005     600`\
`#> 3                   ratio_expt_death   0.01     100`\
`#> 4                         prob_death  1e-05 0.99999`\
`#> 5                          sex_ratio  1e-05 0.99999`\
`#> 6                      expt_maturity  0.001       8`\
`#> 7                    maturity_param2 0.0025      10`\
`#> 8                ratio_expt_maturity   0.01     100`\
`#> 9                  expt_reproduction  0.001      40`\
`#> 10               reproduction_param2   0.05     500`\
`#> 11                       n_offspring      1      50`\
`#> 12             increase_death_hazard  1e-05      10`\
`#> 13                         tof_decay  1e-07      10`\
`#> 14 increase_death_hazard_n_offspring  1e-07      10`\
`#> 15               lin_decrease_hazard    -20      20`\
`#> 16              quad_decrease_hazard    -10      10`\
`#> 17            lin_change_n_offspring    -10      10`\
`#> 18           quad_change_n_offspring    -10      10`\
`#> 19                   tof_n_offspring    -10      10`\
`#> 20                           fitness  0.001    1000`

Since 10 seems to not be high enough, let’s try with 80:

\
`bounds_df``[``bounds_df``$``name`` ``==`` ``"increase_death_hazard"``, ``"max"``]`` ``<-`` ``80`

Once it’s changed, you just have to call
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
again with the `param_bounds_df` argument:

\
`results`` ``<-`` `[`lifelihood`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)`(`\
`  lifelihoodData ``=`` ``dataLFH``,`\
`  path_config ``=`` `[`use_test_config`](https://nrode.github.io/Lifelihood/reference/use_test_config.md)`(``"config"``)``,`\
`  param_bounds_df ``=`` ``bounds_df`\
`)`

Now we don’t get any warning!

# Customize parameter boundaries and estimation warning

If you haven’t already check it, have a look at:

- [What is the required data format to work with
  lifelihood?](https://nrode.github.io/Lifelihood/articles/required-data-format.md)
- [Setting up the configuration
  file](https://nrode.github.io/Lifelihood/articles/setting-up-the-configuration-file.md)
- [How to use the lifelihood
  package](https://nrode.github.io/Lifelihood/articles/how-to-use-lifelihood.md)

## Load libraries

------------------------------------------------------------------------

``` r

library(lifelihood)
#> Loading required package: tidyverse
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.6
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ ggplot2   4.0.1     ✔ tibble    3.3.0
#> ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
#> ✔ purrr     1.2.0     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
library(tidyverse)
```

## Data preparation

------------------------------------------------------------------------

Load the dataset from `.csv` file:

``` r

# input data
df <- fakesample |>
  mutate(
    type = as.factor(type),
    geno = as.factor(geno)
  )
```

``` r

df |> head()
#>   type geno sex_start sex_end sex mat_start mat_end clutch_start1 clutch_end1
#> 1    1    0         0    1000   0         0    1000            NA          NA
#> 2    2    0         0    1000   0         2       3             4           3
#> 3    0    1         0    1000   0         3       4             2           4
#> 4    0    1         0    1000   0         3       4             2           4
#> 5    0    1         0    1000   0         3       4             2           4
#> 6    1    0         0    1000   0         0    1000            NA          NA
#>   clutch_size1 clutch_start2 clutch_end2 clutch_size2 death_start death_end
#> 1           NA            NA          NA           NA         0.1         1
#> 2            4             2           4            5         9.0        10
#> 3            5            NA          NA           NA         5.0         6
#> 4            5            NA          NA           NA         5.0         6
#> 5            5            NA          NA           NA         5.0         6
#> 6           NA            NA          NA           NA         0.1         1
```

Prepare input parameters for the
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
function:

``` r

# name of the columns of the clutchs into a single vector
clutchs <- c(
  "clutch_start1",
  "clutch_end1",
  "clutch_size1",
  "clutch_start2",
  "clutch_end2",
  "clutch_size2"
)
```

*Note: If you have a large number of clutches, it is easier to generate
this vector programmatically, particularly if your dataset contains a
large number of clutches. See the [Generate clutch
names](https://nrode.github.io/Lifelihood/articles/generate-clutch-names.md)
vignette.*

## Create the `lifelihoodData` object

``` r

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
```

## Get the results

------------------------------------------------------------------------

Let’s run the analysis with default parameters.

``` r

results <- lifelihood(
  lifelihoodData = dataLFH,
  path_config = get_config_path("config")
)
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_/temp_param_range_path.txt FALSE 0 25 FALSE 0 FALSE 0 5496 5792 27 6308 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
```

## Warning

------------------------------------------------------------------------

### What is it?

When runnning
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
function, you might encounter the following warning:

``` r

## Warning in check_estimation(results_lifelihood = results): Estimation of
## 'increase_death_hazard' is close to the maximum bound:
## increase_death_hazard≃9.9864992332278. Consider increasing maximum bound.
```

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

You can get the parameter boundaries with the
[`default_bounds_df()`](https://nrode.github.io/Lifelihood/reference/default_bounds_df.md)
function and by passing the `lifelihoodData` object:

``` r

bounds_df <- default_bounds_df(dataLFH)
bounds_df
#>                       param    min     max
#> 1                expt_death  0.001      40
#> 2           survival_param2   0.05     500
#> 3          ratio_expt_death   0.01     100
#> 4                prob_death  1e-05 0.99999
#> 5                 sex_ratio  1e-05 0.99999
#> 6             expt_maturity  0.001       8
#> 7           maturity_param2  0.005     600
#> 8       ratio_expt_maturity   0.01     100
#> 9         expt_reproduction  0.001      10
#> 10      reproduction_param2 0.0025      10
#> 11              n_offspring      1      50
#> 12    increase_death_hazard  1e-05      10
#> 13       tof_reduction_date  1e-07      10
#> 14 increase_tof_n_offspring  1e-07      10
#> 15      lin_decrease_hazard    -20      20
#> 16          quad_senescence    -20      20
#> 17     quad_decrease_hazard    -10      10
#> 18  quad_change_n_offspring    -10      10
#> 19          tof_n_offspring    -10      10
#> 20                  fitness  0.001    1000
```

Since 10 seems to not be high enough, let’s try with 80:

``` r

bounds_df[bounds_df$name == "increase_death_hazard", "max"] <- 80
```

Once it’s changed, you just have to call
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
again with the `param_bounds_df` argument:

``` r

results <- lifelihood(
  lifelihoodData = dataLFH,
  path_config = get_config_path("config"),
  param_bounds_df = bounds_df
)
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_/temp_param_range_path.txt FALSE 0 25 FALSE 0 FALSE 0 7328 1072 3840 4452 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
```

Now we don’t get any warning!

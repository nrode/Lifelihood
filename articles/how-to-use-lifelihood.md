# How to use lifelihood

If you haven’t already check it, have a look at:

- [what is the required data format to work with
  lifelihood?](https://nrode.github.io/Lifelihood/articles/required-data-format.md)
- [setting up the configuration
  file](https://nrode.github.io/Lifelihood/articles/setting-up-the-configuration-file.md)

## Load libraries

------------------------------------------------------------------------

``` r

library(lifelihood)
#> Loading required package: tidyverse
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.6
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ ggplot2   4.0.1     ✔ tibble    3.3.1
#> ✔ lubridate 1.9.4     ✔ tidyr     1.3.2
#> ✔ purrr     1.2.1     
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
[`lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/lifelihoodData.md)
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
this vector programmatically. See the [Generate clutch
names](https://nrode.github.io/Lifelihood/articles/generate-clutch-names.md)
vignette.*

## Create the `lifelihoodData` object

[`lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/lifelihoodData.md)
creates a `lifelihoodData` object, which is a list containing all the
information needed to run the lifelihood program of a given dataset of
individual life history.

This function mostly takes as input your dataset, your column names.

It also has the `model_specs` argument, which is a vector of characters
with the name of the statistical law to use. Must be of length 3 and
each element must be one of `"wei"` (Weibull law), `"exp"` (Exponential
law), `"gam"` (Gamma law) or `"lgn"` (Log-normal law). The first one is
used for **death**, the second one is used for **maturity** and the
third one for **clutchs**.

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

### All default parameters

Once you have created your `lifelihoodData` object with
[`lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/lifelihoodData.md),
you can call the
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
function to run the lifelihood program.

It returns a `lifelihoodResults` object, which is a list containing all
the results of the analysis.

Here it’s a minimalist usage of the function, where we only specify the
`lifelihoodData` object, the path to the [configuration
file](https://nrode.github.io/Lifelihood/articles/2-setting-up-the-configuration-file.md)
and the seeds to use. The `raise_estimation_warning` argument will be
the focus of the [next
vignette](https://nrode.github.io/Lifelihood/articles/4-custom-param-boundaries-and-estimation-warning.md).

``` r

results <- lifelihood(
  lifelihoodData = dataLFH,
  path_config = get_config_path("config"),
  seeds = c(1, 2, 3, 4),
  raise_estimation_warning = FALSE
)
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1_2_3_4/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1_2_3_4/temp_param_range_path.txt FALSE 0 25 FALSE 0 FALSE 0 1 2 3 4 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
summary(results)
#> 
#> === LIFELIHOOD RESULTS ===
#> 
#> Sample size: 13 
#> 
#> --- Model Fit ---
#> Log-likelihood:  -90000000007.406
#> AIC:             180000000040.8
#> BIC:             180000000048.2
#> 
#> --- Key Parameters ---
#> 
#> Mortality:
#>   expt_death (Intercept)    -2.190 (0.000)
#>   expt_death eff_expt_death_geno_1 -0.884 (0.000)
#>   expt_death eff_expt_death_type_1 0.000 (0.000)
#>   expt_death eff_expt_death_type_2 0.000 (0.000)
#>   survival_param2 (Intercept) -6.264 (0.000)
#> 
#> Maturity:
#>   expt_maturity (Intercept) -1.099 (0.000)
#>   expt_maturity eff_expt_maturity_geno_1 0.000 (0.000)
#>   expt_maturity eff_expt_maturity_type_1 0.000 (0.000)
#>   expt_maturity eff_expt_maturity_type_2 0.000 (0.000)
#>   maturity_param2 (Intercept) -6.400 (0.000)
#> 
#> Reproduction:
#>   expt_reproduction (Intercept) -1.099 (0.000)
#>   expt_reproduction eff_expt_reproduction_geno_1 0.000 (0.000)
#>   reproduction_param2 (Intercept) -2.218 (0.000)
#> 
#> --- Convergence ---
#> All parameters within bounds
#> 
#> ======================
```

## Get specific results

------------------------------------------------------------------------

The `lifelihoodResults` object is a list containing all the results of
the analysis. We can get specific results by calling the list element.

``` r

coef(results)
#>               int_expt_death        eff_expt_death_geno_1 
#>                   -2.1903521                   -0.8837441 
#>        eff_expt_death_type_1        eff_expt_death_type_2 
#>                    0.0000000                    0.0000000 
#>          int_survival_param2            int_expt_maturity 
#>                   -6.2638994                   -1.0986123 
#>     eff_expt_maturity_geno_1     eff_expt_maturity_type_1 
#>                    0.0000000                    0.0000000 
#>     eff_expt_maturity_type_2          int_maturity_param2 
#>                    0.0000000                   -6.4002741 
#>        int_expt_reproduction eff_expt_reproduction_geno_1 
#>                   -1.0986123                    0.0000000 
#>      int_reproduction_param2 
#>                   -2.2180354
coeff(results, "expt_death")
#>        int_expt_death eff_expt_death_geno_1 eff_expt_death_type_1 
#>            -2.1903521            -0.8837441             0.0000000 
#> eff_expt_death_type_2 
#>             0.0000000
coeff(results, "survival_param2")
#> int_survival_param2 
#>           -6.263899

AIC(results)
#> [1] 1.8e+11
BIC(results)
#> [1] 1.8e+11

logLik(results)
#> [1] -9e+10

prediction(results, parameter_name = "expt_death") |> head()
#> [1] -2.190352 -2.190352 -3.074096 -3.074096 -3.074096 -2.190352
prediction(results, parameter_name = "expt_death", type = "response") |> head()
#> [1] 4.025709 4.025709 1.768496 1.768496 1.768496 4.025709
```

## Next step

------------------------------------------------------------------------

Now that you have seen how to use the package, you can go further and
[customise your parameter boundaries and deal with estimation
warnings](https://nrode.github.io/Lifelihood/articles/custom-param-boundaries-and-estimation-warning.md).

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
library(tidyverse)
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
#> LIFELIHOOD RESULTS
#> 
#> Likelihood:
#> [1] -2821.227
#> 
#> Effects:
#>                            name estimation stderror           parameter
#> 1                int_expt_death -2.5253959        0          expt_death
#> 2         eff_expt_death_geno_1  0.7835095        0          expt_death
#> 3         eff_expt_death_type_1 -0.9687812        0          expt_death
#> 4         eff_expt_death_type_2  1.3991316        0          expt_death
#> 5           int_survival_param2 -6.5822104        0     survival_param2
#> 6             int_expt_maturity -2.2285974        0       expt_maturity
#> 7      eff_expt_maturity_geno_1 -0.8401484        0       expt_maturity
#> 8      eff_expt_maturity_type_1  0.4112408        0       expt_maturity
#> 9      eff_expt_maturity_type_2 -0.5456602        0       expt_maturity
#> 10          int_maturity_param2 -3.1111023        0     maturity_param2
#> 11        int_expt_reproduction -2.3365343        0   expt_reproduction
#> 12 eff_expt_reproduction_geno_1  1.3102563        0   expt_reproduction
#> 13      int_reproduction_param2 -2.5535342        0 reproduction_param2
#>                    kind        event
#> 1             intercept    mortality
#> 2  coefficient_category    mortality
#> 3  coefficient_category    mortality
#> 4  coefficient_category    mortality
#> 5             intercept    mortality
#> 6             intercept     maturity
#> 7  coefficient_category     maturity
#> 8  coefficient_category     maturity
#> 9  coefficient_category     maturity
#> 10            intercept     maturity
#> 11            intercept reproduction
#> 12 coefficient_category reproduction
#> 13            intercept reproduction
```

## Get specific results

------------------------------------------------------------------------

The `lifelihoodResults` object is a list containing all the results of
the analysis. We can get specific results by calling the list element.

``` r

coef(results)
#>               int_expt_death        eff_expt_death_geno_1 
#>                   -2.5253959                    0.7835095 
#>        eff_expt_death_type_1        eff_expt_death_type_2 
#>                   -0.9687812                    1.3991316 
#>          int_survival_param2            int_expt_maturity 
#>                   -6.5822104                   -2.2285974 
#>     eff_expt_maturity_geno_1     eff_expt_maturity_type_1 
#>                   -0.8401484                    0.4112408 
#>     eff_expt_maturity_type_2          int_maturity_param2 
#>                   -0.5456602                   -3.1111023 
#>        int_expt_reproduction eff_expt_reproduction_geno_1 
#>                   -2.3365343                    1.3102563 
#>      int_reproduction_param2 
#>                   -2.5535342
coeff(results, "expt_death")
#>        int_expt_death eff_expt_death_geno_1 eff_expt_death_type_1 
#>            -2.5253959             0.7835095            -0.9687812 
#> eff_expt_death_type_2 
#>             1.3991316
coeff(results, "survival_param2")
#> int_survival_param2 
#>            -6.58221

AIC(results)
#> [1] 5668.453
BIC(results)
#> [1] 5675.798

logLik(results)
#> [1] -2821.227

prediction(results, parameter_name = "expt_death") |> head()
#> [1] -3.494177 -1.126264 -1.741886 -1.741886 -1.741886 -3.494177
prediction(results, parameter_name = "expt_death", type = "response") |> head()
#> [1] 1.180105 9.794802 5.963791 5.963791 5.963791 1.180105
```

## Next step

------------------------------------------------------------------------

Now that you have seen how to use the package, you can go further and
[customise your parameter boundaries and deal with estimation
warnings](https://nrode.github.io/Lifelihood/articles/custom-param-boundaries-and-estimation-warning.md).

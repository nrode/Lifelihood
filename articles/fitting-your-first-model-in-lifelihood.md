# Fitting your first model in lifelihood

If you haven’t check it yet, have a look at:

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
#> ✔ dplyr     1.2.1     ✔ readr     2.2.0
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
#> ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
#> ✔ purrr     1.2.2     
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
df <- datapierrick |>
  as_tibble() |>
  mutate(par = as.factor(par), geno = as.factor(geno), spore = as.factor(spore))

df |> head()
#> # A tibble: 6 × 95
#>   par   geno  spore sex_start sex_end   sex mat_start mat_end   mat
#>   <fct> <fct> <fct>     <int>   <int> <int>     <int>   <int> <int>
#> 1 0     0     0            13    1000     0        12      13     6
#> 2 0     0     0            13    1000     0        12      13     3
#> 3 0     0     0            15    1000     0        14      15     1
#> 4 0     0     0            14    1000     0        13      14     6
#> 5 0     0     0            19    1000     0        18      19     2
#> 6 0     0     0            12    1000     0        11      12     1
#> # ℹ 86 more variables: clutch_start_1 <int>, clutch_end_1 <int>,
#> #   clutch_size_1 <int>, clutch_start_2 <int>, clutch_end_2 <int>,
#> #   clutch_size_2 <int>, clutch_start_3 <int>, clutch_end_3 <int>,
#> #   clutch_size_3 <int>, clutch_start_4 <int>, clutch_end_4 <int>,
#> #   clutch_size_4 <int>, clutch_start_5 <int>, clutch_end_5 <int>,
#> #   clutch_size_5 <int>, clutch_start_6 <int>, clutch_end_6 <int>,
#> #   clutch_size_6 <int>, clutch_start_7 <int>, clutch_end_7 <int>, …
```

Prepare arguments for the
[`as_lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md)
function:

``` r

# name of the columns of the clutchs into a single vector
clutchs <- generate_clutch_vector(28)
```

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

It also has the `dist` argument, which is a vector of characters with
the name of the statistical distribution to use. Must be of length 3 and
each element must be one of `"wei"` (Weibull distribution), `"exp"`
(exponential distribution), `"gam"` (gamma distribution) or `"lgn"`
(log-normal distribution). The first one is used for **death**, the
second one is used for **maturity** and the third one for **clutchs**.

``` r

dataLFH <- as_lifelihoodData(
  df = df,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "mat_start",
  maturity_end = "mat_end",
  clutchs = clutchs,
  death_start = "death_start",
  death_end = "death_end",
  matclutch = FALSE,
  covariates = c("par", "geno"),
  dist = c("wei", "gam", "lgn")
)
```

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
`lifelihoodData` object, the path to the [configuration
file](https://nrode.github.io/Lifelihood/articles/2-setting-up-the-configuration-file.md)
and the seeds to use (four integers used by Mersenne Twister
pseudorandom number generator of the lifelihood program). The
`raise_estimation_warning` argument will be the focus of the [next
vignette](https://nrode.github.io/Lifelihood/articles/4-custom-param-boundaries-and-estimation-warning.md).

``` r

results <- lifelihood(
  lifelihoodData = dataLFH,
  path_config = use_test_config("config_pierrick"),
  seeds = c(1, 2, 3, 4),
  raise_estimation_warning = FALSE
)
summary(results)
#> 
#> === LIFELIHOOD RESULTS ===
#> 
#> Sample size: 550 
#> 
#> --- Model Fit ---
#> Log-likelihood:  -31588.744
#> AIC:             63197.5
#> BIC:             63240.6
#> 
#> --- Key Parameters ---
#> 
#> Mortality:
#>   expt_death (Intercept)    -0.913 (0.000)
#>   expt_death eff_expt_death_par_1 -1.946 (0.000)
#>   expt_death eff_expt_death_par_2 -1.976 (0.000)
#>   survival_param2 (Intercept) -0.480 (0.000)
#>   ratio_expt_death (Intercept) -3.525 (0.000)
#> 
#> Maturity:
#>   expt_maturity (Intercept) -1.497 (0.000)
#>   maturity_param2 (Intercept) -5.881 (0.000)
#> 
#> Reproduction:
#>   expt_reproduction (Intercept) -1.769 (0.000)
#>   reproduction_param2 (Intercept) -7.303 (0.000)
#>   n_offspring (Intercept)   -2.596 (0.000)
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
#>          int_expt_death    eff_expt_death_par_1    eff_expt_death_par_2 
#>              -0.9133659              -1.9460199              -1.9763243 
#>     int_survival_param2    int_ratio_expt_death       int_expt_maturity 
#>              -0.4795909              -3.5245392              -1.4966132 
#>     int_maturity_param2   int_expt_reproduction int_reproduction_param2 
#>              -5.8814605              -1.7690031              -7.3034247 
#>         int_n_offspring 
#>              -2.5960814
coeff(results, "expt_death")
#>       int_expt_death eff_expt_death_par_1 eff_expt_death_par_2 
#>           -0.9133659           -1.9460199           -1.9763243
coeff(results, "survival_param2")
#> int_survival_param2 
#>          -0.4795909

AIC(results)
#> [1] 63197.49
BIC(results)
#> [1] 63240.59

logLik(results)
#> [1] -31588.74

prediction(results, parameter_name = "expt_death") |> head()
#> Lifelihood parameter estimate(s) for males are identical to that of females. Use type='response', to get the right parameter estimate(s) for males on the response scale.
#> [1] -0.9133659 -0.9133659 -0.9133659 -0.9133659 -0.9133659 -0.9133659
prediction(results, parameter_name = "expt_death", type = "response") |> head()
#> [1] 92.76566 92.76566 92.76566 92.76566 92.76566 92.76566
```

## Next step

------------------------------------------------------------------------

Now that you have seen how to use the package, you can go further and
[customise your parameter boundaries and deal with estimation
warnings](https://nrode.github.io/Lifelihood/articles/customize-parameter-boundaries-and-estimation-warning.md).

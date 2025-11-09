# Individual life history modelling

Computes the joined likelihood of all the events in an individual
life-history (time of maturity, reproductive events, death) and
estimates the parameters of the model using maximum likelihood.

## Usage

``` r
lifelihood(
  lifelihoodData,
  path_config,
  path_to_Lifelihood = NULL,
  n_fit = 1,
  param_bounds_df = NULL,
  group_by_group = FALSE,
  MCMC = 0,
  interval = 25,
  se = FALSE,
  saveprobevent = 0,
  r = 0,
  seeds = NULL,
  ntr = 2,
  nst = 2,
  To = 50,
  Tf = 1,
  climbrate = 1,
  precision = 0.001,
  ratiomax = 10,
  tc = 20,
  tinf = 1000,
  sub_interval = 0.3,
  raise_estimation_warning = TRUE,
  delete_temp_files = TRUE,
  temp_dir = NULL
)
```

## Arguments

- lifelihoodData:

  `lifelihoodData` object created with
  [`lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/lifelihoodData.md).

- path_config:

  A character string specifying the file path to the YAML configuration
  file.

- path_to_Lifelihood:

  A character string specifying the file path to the compile Lifelihood
  program (default is NULL)

- n_fit:

  Number of times to fit.

- param_bounds_df:

  Dataframe with the parameter ranges/boundaries/boundaries

- group_by_group:

  Boolean option to fit the full factorial model with all the
  interactions between each of the factors

- MCMC:

  Perform MCMC sampling of the parameter after convergence to estimate
  their 95% confidence interval

- interval:

  TBD - Check the actual meaning

- se:

  If TRUE, Lifelihood computes the standard error of each parameter
  using the Hessian matrix (output with value of -1 if standard error
  cannot be computed due to singularity of the Hessian matrix)

- saveprobevent:

  TBD - Check the actual meaning

- r:

  Reparametrize the model with one parameter as the intrinsic rate of
  increase

- seeds:

  Vector of length for with seed numbers used to reproduce results (same
  seeds = same results). This argument should be `NULL` (default) when
  `n_fit` \> 1.

- ntr:

  Number of thread for the paralelisation ?

- nst:

  TBD - Check the actual meaning

- To:

  Initial temperature for the simulated annealing

- Tf:

  Initial temperature for the simulated annealing

- climbrate:

  Rate for the simulated annealing ?

- precision:

  TBD - Check the actual meaning

- ratiomax:

  Maximum multiplicative factor for clutch size in models with
  reproductive senescence (cf CalculRatioEspPoissonTronque function in
  Lifelihood)

- tc:

  critical age (after the juvenile mortality peak) at which the survival
  model starts to be fitted

- tinf:

  maximum censoring time (should be greater than the age of the oldest
  individual observed dead in the dataset)

- sub_interval:

  sub-interval used to integrate the left and right censoring dates of
  each event

- raise_estimation_warning:

  Whether or not to raise a warning when the estimate of a parameter is
  too close to its minimum or maximum bound. Default is TRUE.

- delete_temp_files:

  Indicates whether temporary files should be deleted. TRUE by default
  and recommended.

## Value

A `lifelihoodResults` object

## Examples

``` r
library(lifelihood)
library(tidyverse)

df <- fakesample |>
  mutate(
    geno = as.factor(geno),
    type = as.factor(type)
  )
head(df)
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

clutchs <- c(
  "clutch_start1", "clutch_end1", "clutch_size1",
  "clutch_start2", "clutch_end2", "clutch_size2"
)

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

results <- lifelihood(
  lifelihoodData = dataLFH,
  path_config = get_config_path("config"),
  seeds = c(1, 2, 3, 4),
  raise_estimation_warning = FALSE
)
#> [1] "/private/var/folders/6c/pzd640_546q6_yfn24r65c_40000gn/T/Rtmp9BpFOA/temp_libpath18676669f2c2/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1_2_3_4/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_1_2_3_4/temp_param_range_path.txt FALSE 0 25 FALSE 0 FALSE 0 1 2 3 4 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
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
#> 
```

# Fit lifelihood group by group

Orchestrator function that splits data by covariate groups, fits
intercept-only models for each group, and merges results.

## Usage

``` r
lifelihood_fit_group_by_group(
  lifelihoodData,
  path_config,
  path_to_Lifelihood = NULL,
  param_bounds_df = NULL,
  MCMC = 0,
  interval = 25,
  se.fit = FALSE,
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
  delete_temp_files = TRUE
)
```

## Arguments

- lifelihoodData:

  `lifelihoodData` object created with
  [`as_lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md).

- path_config:

  A character string specifying the file path to the YAML configuration
  file.

- path_to_Lifelihood:

  A character string specifying the file path to the compile Lifelihood
  program (default is NULL)

- param_bounds_df:

  Dataframe with the parameter ranges/boundaries/boundaries

- MCMC:

  Perform MCMC sampling of the parameter after convergence to estimate
  their 95% confidence interval

- interval:

  TBD - Check the actual meaning

- se.fit:

  If TRUE, Lifelihood computes the standard error of each parameter
  using the variance-covariance matrix (approximated by the inverse of
  the negative of the Hessian matrix).

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

- delete_temp_files:

  Indicates whether temporary files should be deleted. TRUE by default
  and recommended.

## Value

A `lifelihoodResults` object with group-by-group results.

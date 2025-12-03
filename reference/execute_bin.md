# Execution of the compiled files

Run lifelihood program in console mode.

## Usage

``` r
execute_bin(
  path_to_Lifelihood,
  path_input_data,
  path_param_bounds,
  group_by_group,
  MCMC,
  interval,
  se.fit,
  saveprobevent,
  fitness,
  r,
  seed1,
  seed2,
  seed3,
  seed4,
  ratiomax,
  tc,
  tinf,
  sub_interval,
  path_continuous_var,
  ntr,
  nst,
  To,
  Tf,
  climbrate,
  precision
)
```

## Arguments

- path_to_Lifelihood:

  A character string specifying the file path to the compile Lifelihood
  program (default is NULL)

- path_input_data:

  Path to the input text file with the model and data to be fitted.

- path_param_bounds:

  Path to the parameter boundaries text file with the min and max
  boudaries for each parameter.

- group_by_group:

  Boolean option to fit the full factorial model with all the
  interactions between each of the factors

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

- seed1:

  First seed number used to reproduce results (same seed = same
  results).

- seed2:

  Second seed number used to reproduce results (same seed = same
  results).

- seed3:

  Third seed number used to reproduce results (same seed = same
  results).

- seed4:

  Fourth seed number used to reproduce results (same seed = same
  results).

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

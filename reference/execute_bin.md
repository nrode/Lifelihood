# Execution of the compiled files

Run lifelihood program in console mode.

## Usage

``` r
execute_bin(
  path_to_Lifelihood,
  path_input_data,
  path_param_bounds,
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
  program (default is NULL).

- path_input_data:

  Path to the input text file with the model and data to be fitted.

- path_param_bounds:

  Path to the parameter boundaries text file with the min and max
  boudaries for each parameter.

- MCMC:

  Perform MCMC sampling of the parameter after convergence to estimate
  their 95% confidence interval.

- interval:

  TBD - Check the actual meaning.

- se.fit:

  If TRUE, Lifelihood computes the standard error of each parameter
  using the variance-covariance matrix.

- saveprobevent:

  TBD - Check the actual meaning.

- fitness:

  Reparametrize the model with one parameter as the intrinsic rate of
  increase.

- r:

  Reparametrize the model with one parameter as the intrinsic rate of
  increase.

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
  reproductive senescence.

- tc:

  Critical age (after the juvenile mortality peak) at which the survival
  model starts to be fitted.

- tinf:

  Maximum censoring time (should be greater than the age of the oldest
  individual observed dead in the dataset).

- sub_interval:

  Sub-interval used to integrate the left and right censoring dates of
  each event.

- path_continuous_var:

  Path to the continuous variables file.

- ntr:

  Number of thread for the paralelisation.

- nst:

  Simulated annealing tuning parameter increasing nst makes the search
  more thorough at each temperature (better chance of finding the global
  maximum, slower fit)

- To:

  Initial temperature for the simulated annealing.

- Tf:

  Final temperature for the simulated annealing.

- climbrate:

  Initial cooling rate of the adaptive annealing schedule: scales how
  fast the temperature is lowered between annealing runs (self-adjusting
  during the fit).

- precision:

  Convergence tolerance of the final local search: fitting stops when an
  additional batch of local-search iterations improves the
  log-likelihood by less than this amount.

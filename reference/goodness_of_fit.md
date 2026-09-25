# Parametric goodness-of-fit from simulated datasets

Simulate datasets from a fitted model, refit the model on each simulated
dataset, and compare simulated log-likelihood values to the original
fit.

## Usage

``` r
goodness_of_fit(object, nsim, seed = NULL, fit_args = list())
```

## Arguments

- object:

  Output of
  [`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md).

- nsim:

  Number of simulated datasets to generate and refit.

- seed:

  Optional integer seed for reproducibility.

- fit_args:

  Named list of additional arguments passed to
  [`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
  when refitting each simulated dataset. By default, refits use
  `n_fit = 1`, `MCMC = 0`, and `se.fit = FALSE` for speed.

## Value

A `lifelihoodGOF` object (list) with:

- `original_loglik`: log-likelihood of the original fit

- `simulated_loglik`: numeric vector of simulated/refitted
  log-likelihoods

- `n_success`: number of successful refits

- `n_failed`: number of failed refits

- `p_lower_or_equal`: proportion of simulated log-likelihoods lower than
  or equal to the original value

- `errors`: per-simulation error messages (if any)

- `fits`: list containing the fitted object for each simulation, or
  `NULL` for failed refits

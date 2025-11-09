# Check if estimation are too close from boundaries.

Since the parameter boundaries are from
[`default_bounds_df()`](https://nrode.github.io/Lifelihood/reference/default_bounds_df.md)
(or customized by user), this function checks is the estimation of each
parameter is "too close" (0.5% tolerance) from one of its boundaries
(either min or max). If so, it raises a warning that tries to be
explicit. This function can be ignored thanks to the
`raise_estimation_warning` argument (boolean) in
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md).

## Usage

``` r
check_valid_estimation(lifelihoodResults)
```

## Arguments

- lifelihoodResults:

  Output object of the
  [`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
  function.

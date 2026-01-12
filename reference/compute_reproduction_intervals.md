# Compute time interval between clutches

This function computes the time interval between consecutive clutches
and add censored unobserved last clutch between the last clutch and time
of death unless the time of death is right censored. It also removes
individuals that never reproduced.

## Usage

``` r
compute_reproduction_intervals(lifelihoodData, verbose = TRUE)
```

## Arguments

- lifelihoodData:

  Ouput of
  [`lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/lifelihoodData.md)

- verbose:

  Boolean whether to print messages during the computation process
  (default=TRUE)

## Value

A dataframe with time interval between consecutive clutches starting
from maturity.

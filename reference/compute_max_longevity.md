# Compute a high-quantile longevity bound

Internal helper used by tradeoff simulations to compute an upper
longevity bound from mortality parameters. The bound corresponds to a
very high quantile (`0.99999999`) of the mortality distribution.

## Usage

``` r
compute_max_longevity(expected, shape, family)
```

## Arguments

- expected:

  Numeric vector of expected mortality times.

- shape:

  Numeric vector of second distribution parameters.

- family:

  Character scalar indicating the mortality family. Must be one of
  `"wei"`, `"lgn"`, `"gam"`, or `"exp"`.

## Value

A numeric vector with high-quantile longevity values.

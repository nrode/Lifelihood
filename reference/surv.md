# Probability of dying between t and t+dt afer living until t.

Probability of dying between t and t+dt afer living until t.

## Usage

``` r
surv(t, param1, param2, family = c("exp", "wei", "gam", "lgn"))
```

## Arguments

- t:

  Numeric. The time to event

- param1:

  Numeric. The expected longevity or time to maturity.

- param2:

  Numeric. The second parameter returned by lifelihood.

## Value

The probability of event (being alive or not mature) at time t.

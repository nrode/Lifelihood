# Probability of dying between t and t+dt afer living until t.

Probability of dying between t and t+dt afer living until t.

## Usage

``` r
prob_event_interval_dt(t, dt, param1, param2, family)
```

## Arguments

- t:

  Numeric. The time to event

- dt:

  Interval

- param1:

  Numeric. The expected longevity or time to maturity.

- param2:

  Numeric. The second parameter returned by lifelihood.

- family:

  One of "exp", "wei", "gam", "lgn"

## Value

The survival probability (numeric)

# lifelihood

## Description

`lifelihood` is an R package and a class of continuous time multi-event models which provide the joined likelihood of all the events in an individual life-history (time of maturity, reproductive events, death).

[Documentation](https://nrode.github.io/Lifelihood/)

It requires:

- R 4.1.0 (or later)
- macOS (ARM64) or Windows

<br>

## Installation

```r
devtools::install_github("nrode/Lifelihood")
```

<br>
<br>

## State

### Works

- ✅ Layer between R and Pascal program
- ✅ Prediction using fitted parameters (training or new data)
- ✅ Extract coefficients, AIC/BIC/AICc, likelihood
- ✅ Configure what models to fit
- ✅ Configure parameters bounds
- ✅ Simulation of individual events

### Still needs work

- ❌ Visualization
- ❌ Simulation with trade-offs

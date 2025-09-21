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
<br>

## State

### Statistics

~5000 lines of R code

~6000 lines of pascal code

9 vignettes (3 for internal development)

2 built-in datasets (Pierrick + fake sample)

### Works

- ✅ Layer between R and Pascal program
- ✅ Prediction using fitted parameters (training or new data)
- ✅ Extract coefficients, AIC/BIC/AICc, likelihood
- ✅ Configure what models to fit
- ✅ Configure parameters bounds
- ✅ Simulation of individual events
- ✅ Documentation website updates automatically

### Still needs work

- ❌ Visualization
- ❌ Simulation with trade-offs
- ❌ Group by group
- ❌ Unit tests

### Nice to have

- Better workflow for compilation
- Better error messages

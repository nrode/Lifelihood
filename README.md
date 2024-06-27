# lifelihood

<br>

## Description

`lifelihood` is a class of continuous time multi-event models which provide the joined likelihood of all the events in an individual life-history (time of maturity, reproductive events, death).

<br>

## Project progress

- ✅ Transforming the dataframe into a `.txt` input file
- ✅ Reading the output file to return estimations and other values
- ✅ Simple way to provides
   - parameter ranges
   - seeds
   - which statistical law to use
   - extra variables (geno, spore etc)
- ❌ Compile for Windows with Lazarus
- ❌ Specify which parameters to estimate and interaction effect (`****modele******` section). It currently uses arbitrary default values from `R/write_input.R`.
- ❌ Visualization (what kind?)
- ❌ Simulation using estimation
- ❌ Goodness of fit

There currently are 2 functions for users:
- `lifelihood()`: main function that does most of the job that returns an object of class `LifelihoodResults`
- `summary()`: to use on the output object of `lifelihood()` to display main results such as estimations, seeds used etc.

<br>

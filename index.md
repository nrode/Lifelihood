<br><br>

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
- ✅ Read a config file with all effects to fit
- ✅ Make the input data file and output file temp file
- ✅ Compile for Windows with Lazarus
- ❌ Visualization
- ❌ Simulation using estimation
- ❌ Goodness of fit

There currently are 2 functions for users:

- `lifelihood()`: main function that does most of the job that returns an object of class `LifelihoodResults`
- `summary()`: to use on the output object of `lifelihood()` to display main results such as estimations, seeds used etc.

<br>

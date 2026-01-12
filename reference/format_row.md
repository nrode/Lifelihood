# Format a dataframe row for the input data file

Takes a row from a dataframe with input data (sex, maturity, clutch date
and death) in interval format and transform it into a large string
(required format for the data input file).

## Usage

``` r
format_row(
  row,
  sex,
  sex_start,
  sex_end,
  maturity_start,
  maturity_end,
  clutchs,
  death_start,
  death_end,
  covariates
)
```

## Arguments

- row:

  A row of the dataframe object provided by the user (`df` argument in
  [`lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/lifelihoodData.md)
  function).

- sex:

  Column name containing the sex of the observations.

- sex_start:

  Column name containing the first date of the interval in which the sex
  was determined.

- sex_end:

  Column name containing the second date of the interval in which the
  sex was determined.

- maturity_start:

  Column name containing the first date of the interval in which the
  maturity was determined.

- maturity_end:

  Column name containing the second date of the interval in which the
  maturity was determined.

- clutchs:

  Vector containing the names of the clutch columns.

- death_start:

  Column name containing the first date of the interval in which the
  death was determined.

- death_end:

  Column name containing the second date of the interval in which the
  death was determined.

- covariates:

  Vector containing the names of the covariates.

## Value

A string of the well formated row.

# Data object for lifelihood

Creates a `lifelihoodData` object, which is a list containing all the
information needed to run the lifelihood program for a given dataset of
individual life histories. This function is mainly used to pass data to
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
or to customize parameter boundaries with
[`default_bounds_df()`](https://nrode.github.io/Lifelihood/reference/default_bounds_df.md).

## Usage

``` r
as_lifelihoodData(
  df,
  sex,
  sex_start,
  sex_end,
  maturity_start = NULL,
  maturity_end = NULL,
  clutchs = NULL,
  death_start = NULL,
  death_end = NULL,
  dist,
  covariates,
  block = NULL,
  matclutch = FALSE,
  matclutch_size = NULL,
  right_censoring_date = 1000,
  critical_age = 20,
  ratiomax = 10
)
```

## Arguments

- df:

  Data frame containing life-history data. It should have one row per
  life history / observation.

- sex:

  Column name containing the sex of the observations.

- sex_start:

  Column name containing the first date of the interval in which the sex
  was determined.

- sex_end:

  Column name containing the second date of the interval in which the
  sex was determined.

- maturity_start:

  Optional column name containing the first date of the interval in
  which the maturity was determined. Required when fitting maturity or
  reproduction.

- maturity_end:

  Optional column name containing the second date of the interval in
  which the maturity was determined. Required when fitting maturity or
  reproduction.

- clutchs:

  Optional vector of clutch column names. For each clutch, list the
  first date, second date, and clutch size, in that order. For example,
  if the observation with the most clutches has 10, the vector must
  contain 10 x 3 = 30 elements.

- death_start:

  Optional column name containing the first date of the interval in
  which the death was determined. Required when fitting mortality or
  reproduction.

- death_end:

  Optional column name containing the second date of the interval in
  which the death was determined. Required when fitting mortality or
  reproduction.

- dist:

  Data frame with one or more columns selected from `mortality`,
  `maturity`, and `reproduction`. Each row specifies one model to fit
  with
  [`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md).
  Reproduction requires mortality and maturity data. Each cell must
  contain "wei" (Weibull), "exp" (exponential), "gam" (gamma), or "lgn"
  (log-normal). Use a one-row data frame to fit a single model.

- covariates:

  Vector containing the names of the covariates.

- block:

  Column name containing the block to which each individual belongs.

- matclutch:

  Whether the maturity event (designated by `maturity_start` and
  `maturity_end`) is a clutch event or not. If `TRUE`, must specify the
  `matclutch_size` argument. Default is `FALSE`.

- matclutch_size:

  Column name containing the size of the clutch for the maturity event.
  Required when `matclutch` is `TRUE` and maturity data are used,
  including as a dependency of reproduction.

- right_censoring_date:

  (CURRENTLY IGNORED) Time (integer) point at which a subject's data are
  censored. This means that for subjects who do not experience the event
  of interest (e.g., death or failure) by this date, their data are
  considered censored. In practice, choose a value much larger than the
  maximum longevity seen in the data.

- critical_age:

  (CURRENTLY IGNORED) Critical age (integer) below which life histories
  are not followed individually.

- ratiomax:

  (CURRENTLY IGNORED) Maximum ratio (integer) between the numbers of
  offspring at the last and first reproduction events. It cannot be
  greater than `ratiomax`.

## Value

`lifelihoodData` object

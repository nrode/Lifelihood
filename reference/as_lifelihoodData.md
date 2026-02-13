# Data object for lifelihood

Creates a `lifelihoodData` object, which is a list containing all the
information needed to run the lifelihood program of a given dataset of
individual life history. This function will mainly be used to pass to
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
or for customizing parameter boundaries with
[`default_bounds_df()`](https://nrode.github.io/Lifelihood/reference/default_bounds_df.md).

## Usage

``` r
as_lifelihoodData(
  df,
  sex,
  sex_start,
  sex_end,
  maturity_start,
  maturity_end,
  clutchs,
  death_start,
  death_end,
  model_specs,
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

  Dataframe with the data of life history. It should have one row per
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

  Column name containing the first date of the interval in which the
  maturity was determined.

- maturity_end:

  Column name containing the second date of the interval in which the
  maturity was determined.

- clutchs:

  Vector containing the names of the clutch columns. The order should
  be: first clutch first date, first clutch second date, first clutch
  clutch size, second clutch first date, first clutch second date,
  second clutch clutch size, and so on. If the observation with the most
  clutches is, for example, 10, then the vector must be of size 10 x 3 =
  30 (3 elements per clutch: first date, second date and size).

- death_start:

  Column name containing the first date of the interval in which the
  death was determined.

- death_end:

  Column name containing the second date of the interval in which the
  death was determined.

- model_specs:

  Vector of characters with the name of the statistical law to use. Must
  be of length 3 and each element must be one of "wei" (Weibull law),
  "exp" (Exponential law), "gam" (Gamma law) or "lgn" (Log-normal law).
  The first one is used for mortality, the second one is used for
  maturity and the third is used for reproduction.

- covariates:

  Vector containing the names of the covariates.

- block:

  Name of the block to which each individual belong to.

- matclutch:

  Whether the maturity event (designated by `maturity_start` and
  `maturity_end`) is a clutch event or not. If `TRUE`, must specify the
  `matclutch_size` argument. Default is `FALSE`.

- matclutch_size:

  Column name containing the size of the clutch for the maturity event.
  Only used (and required) if `matclutch` is `TRUE`.

- right_censoring_date:

  (CURRENTLY IGNORED) Time (integer) point at which a subject’s data is
  censored. This means that for subjects who do not experience the event
  of interest (e.g., death, failure) by this date, their data is
  considered censored. In practice, choose a value much larger than the
  maximum longevity seen in the data.

- critical_age:

  (CURRENTLY IGNORED) Critical age (integer) below which life histories
  are not followed individually.

- ratiomax:

  (CURRENTLY IGNORED) Maximum ratio (integer) between number of
  offspring of last and first reproduction events. Cannot be greater
  than ratiomax.

## Value

`lifelihoodData` object

# Create the input data file from a dataframe

Takes a dataframe (`df` argument in
[`lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/lifelihoodData.md)
function) and apply to each row the
[`format_row()`](https://nrode.github.io/Lifelihood/reference/format_row.md)
function to create the input data file.

## Usage

``` r
format_dataframe_to_txt(
  df,
  sex,
  sex_start,
  sex_end,
  maturity_start,
  maturity_end,
  matclutch,
  matclutch_size,
  clutchs,
  death_start,
  death_end,
  covariates,
  model_specs,
  path_config,
  temp_dir
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

- matclutch:

  Whether the maturity event (designated by `maturity_start` and
  `maturity_end`) is a clutch event or not. If `TRUE`, must specify the
  `matclutch_size` argument. Default is `FALSE`.

- matclutch_size:

  Column name containing the size of the clutch for the maturity event.
  Only used (and required) if `matclutch` is `TRUE`.

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

- covariates:

  Vector containing the names of the covariates.

- model_specs:

  Vector of characters with the name of the statistical law to use. Must
  be of length 3 and each element must be one of "wei" (Weibull law),
  "exp" (Exponential law), "gam" (Gamma law) or "lgn" (Log-normal law).
  The first one is used for mortality, the second one is used for
  maturity and the third is used for reproduction.

- path_config:

  A character string specifying the file path to the YAML configuration
  file.

- temp_dir:

  Name of the temporary directory with temporary files

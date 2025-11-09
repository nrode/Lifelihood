# Required data format

``` r

library(lifelihood)
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
#> ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
#> ✔ purrr     1.2.0     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

## Introduction

------------------------------------------------------------------------

As life history data is by nature heterogeneous between observations
from the same sample, the format of the data required can be somewhat
tricky. Under the hood, `lifelihood` creates a text file containing the
data (and other parameters), and then use this file for the (core)
source code. Fortunately, `lifelihood` automatically transform the
dataframe into the text file with the right format.

In this vignette, we’ll look at how the dataframe containing your data
needs to be formatted for the Lifelihood programme to work properly.

## Data preparation

------------------------------------------------------------------------

Let’s create a simple dataset with just 7 observations and the following
columns:

- `sex` Column name containing the sex of the observations.
- `sex_start` Column name containing the first date of the interval in
  which the sex was determined.
- `sex_end` Column name containing the second date of the interval in
  which the sex was determined.
- `maturity_start` Column name containing the first date of the interval
  in which the maturity was determined.
- `maturity_end` Column name containing the second date of the interval
  in which the maturity was determined.
- `clutchs` Vector containing the names of the clutch columns. The order
  should be: first clutch first date, first clutch second date, first
  clutch clutch size, second clutch first date, first clutch second
  date, second clutch clutch size, and so on. If the observation with
  the most clutches is, for example, 10, then the vector must be of size
  10 x 3 = 30 (3 elements per clutch: first date, second date and size).
- `death_start` Column name containing the first date of the interval in
  which the death was determined.
- `death_end` Column name containing the second date of the interval in
  which the death was determined.
- `geno` Column name of the first column to add in the input data file

``` r

df <- data.frame(
  sex = c(0, 0, 0, 0, 0, 0, 0),
  sex_start = c(1, 3, 2, 10, 3, 4, 5),
  sex_end = c(2, 4, 3, 11, 4, 5, 6),
  maturity_start = c(2, 1, 0, 1, 0, 2, 1),
  maturity_end = c(4, 2, 1000, 2, 1000, 3, 2),
  clutch_start1 = c(3, 2, NA, 2, NA, 3, 2),
  clutch_end1 = c(4, 3, NA, 3, NA, 4, 3),
  clutch_size1 = c(4, 6, NA, 5, NA, 2, 30),
  clutch_start2 = c(5, NA, NA, 5, NA, 4, 3),
  clutch_end2 = c(6, NA, NA, 6, NA, 5, 4),
  clutch_size2 = c(5, NA, NA, 7, NA, 10, 5),
  clutch_start3 = c(7, NA, NA, 6, NA, NA, 5),
  clutch_end3 = c(8, NA, NA, 7, NA, NA, 6),
  clutch_size3 = c(1, NA, NA, 1, NA, NA, 2),
  death_start = c(8, 11, 0, 11, 0, 7, 9),
  death_end = c(12, 11, 1, 12, 1, 8, 10),
  geno = c(1, 3, 1, 0, 2, 0, 1)
)
```

``` r

df |> head()
#>   sex sex_start sex_end maturity_start maturity_end clutch_start1 clutch_end1
#> 1   0         1       2              2            4             3           4
#> 2   0         3       4              1            2             2           3
#> 3   0         2       3              0         1000            NA          NA
#> 4   0        10      11              1            2             2           3
#> 5   0         3       4              0         1000            NA          NA
#> 6   0         4       5              2            3             3           4
#>   clutch_size1 clutch_start2 clutch_end2 clutch_size2 clutch_start3 clutch_end3
#> 1            4             5           6            5             7           8
#> 2            6            NA          NA           NA            NA          NA
#> 3           NA            NA          NA           NA            NA          NA
#> 4            5             5           6            7             6           7
#> 5           NA            NA          NA           NA            NA          NA
#> 6            2             4           5           10            NA          NA
#>   clutch_size3 death_start death_end geno
#> 1            1           8        12    1
#> 2           NA          11        11    3
#> 3           NA           0         1    1
#> 4            1          11        12    0
#> 5           NA           0         1    2
#> 6           NA           7         8    0
```

As you can see, some observations made more ponts, leading to the
presence of NULL values.

*One* row of the dataset should represent the **life history** of *one*
observation.

## Next step

------------------------------------------------------------------------

Once our dataframe has the right format, we now have to [create a
configuration
file](https://nrode.github.io/Lifelihood/articles/setting-up-the-configuration-file.md).

# Generate clutch names

The
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
function requires a `clutch` argument. It’s a vector containing the
names of the clutch columns. The order should be: first clutch first
date, first clutch second date, first clutch clutch size, second clutch
first date, first clutch second date, second clutch clutch size, and so
on. If the observation with the most clutches is, for example, 10, then
the vector must be of size 10 x 3 = 30 (3 elements per clutch: first
date, second date and size).

For practical reasons, it is easier to generate this vector
programmatically, particularly if your dataset contains a large number
of clutches.

## Load libraries & data

------------------------------------------------------------------------

``` r

library(lifelihood)
#> Loading required package: tidyverse
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.6
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ ggplot2   4.0.1     ✔ tibble    3.3.0
#> ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
#> ✔ purrr     1.2.0     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
library(tidyverse)

df <- fakesample |>
  mutate(
    type = as.factor(type),
    geno = as.factor(geno)
  )

df |> head()
#>   type geno sex_start sex_end sex mat_start mat_end clutch_start1 clutch_end1
#> 1    1    0         0    1000   0         0    1000            NA          NA
#> 2    2    0         0    1000   0         2       3             4           3
#> 3    0    1         0    1000   0         3       4             2           4
#> 4    0    1         0    1000   0         3       4             2           4
#> 5    0    1         0    1000   0         3       4             2           4
#> 6    1    0         0    1000   0         0    1000            NA          NA
#>   clutch_size1 clutch_start2 clutch_end2 clutch_size2 death_start death_end
#> 1           NA            NA          NA           NA         0.1         1
#> 2            4             2           4            5         9.0        10
#> 3            5            NA          NA           NA         5.0         6
#> 4            5            NA          NA           NA         5.0         6
#> 5            5            NA          NA           NA         5.0         6
#> 6           NA            NA          NA           NA         0.1         1
```

## Generate clutch names

Let’s say my clutch names looks like this: `clutch_start_1`,
`clutch_end_1`, `clutch_size_1`, `clutch_start_2`, `clutch_end_2`,
`clutch_size_2`, and so on. This goes up to `clutch_start_28`,
`clutch_end_28`, `clutch_size_28`.

We can generate this vector with the following function:

``` r

generate_clutch_vector <- function(N) {
  return(paste(
    "clutch",
    rep(c("start", "end", "size"), N),
    rep(1:N, each = 3),
    sep = "_"
  ))
}
clutchs <- generate_clutch_vector(28)
clutchs
#>  [1] "clutch_start_1"  "clutch_end_1"    "clutch_size_1"   "clutch_start_2" 
#>  [5] "clutch_end_2"    "clutch_size_2"   "clutch_start_3"  "clutch_end_3"   
#>  [9] "clutch_size_3"   "clutch_start_4"  "clutch_end_4"    "clutch_size_4"  
#> [13] "clutch_start_5"  "clutch_end_5"    "clutch_size_5"   "clutch_start_6" 
#> [17] "clutch_end_6"    "clutch_size_6"   "clutch_start_7"  "clutch_end_7"   
#> [21] "clutch_size_7"   "clutch_start_8"  "clutch_end_8"    "clutch_size_8"  
#> [25] "clutch_start_9"  "clutch_end_9"    "clutch_size_9"   "clutch_start_10"
#> [29] "clutch_end_10"   "clutch_size_10"  "clutch_start_11" "clutch_end_11"  
#> [33] "clutch_size_11"  "clutch_start_12" "clutch_end_12"   "clutch_size_12" 
#> [37] "clutch_start_13" "clutch_end_13"   "clutch_size_13"  "clutch_start_14"
#> [41] "clutch_end_14"   "clutch_size_14"  "clutch_start_15" "clutch_end_15"  
#> [45] "clutch_size_15"  "clutch_start_16" "clutch_end_16"   "clutch_size_16" 
#> [49] "clutch_start_17" "clutch_end_17"   "clutch_size_17"  "clutch_start_18"
#> [53] "clutch_end_18"   "clutch_size_18"  "clutch_start_19" "clutch_end_19"  
#> [57] "clutch_size_19"  "clutch_start_20" "clutch_end_20"   "clutch_size_20" 
#> [61] "clutch_start_21" "clutch_end_21"   "clutch_size_21"  "clutch_start_22"
#> [65] "clutch_end_22"   "clutch_size_22"  "clutch_start_23" "clutch_end_23"  
#> [69] "clutch_size_23"  "clutch_start_24" "clutch_end_24"   "clutch_size_24" 
#> [73] "clutch_start_25" "clutch_end_25"   "clutch_size_25"  "clutch_start_26"
#> [77] "clutch_end_26"   "clutch_size_26"  "clutch_start_27" "clutch_end_27"  
#> [81] "clutch_size_27"  "clutch_start_28" "clutch_end_28"   "clutch_size_28"
```

The only disadvantage of this technique is that the clutch names have to
have a specific pattern.

# Overview of all features in `lifelihood`

### Load lifelihood

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
```

### Data prepration

``` r

df <- datapierrick |>
  mutate(
    geno = as.factor(geno),
    par = as.factor(par),
    spore = as.factor(spore)
  )

generate_clutch_vector <- function(N) {
  return(paste(
    "pon",
    rep(c("start", "end", "size"), N),
    rep(1:N, each = 3),
    sep = "_"
  ))
}
clutchs <- generate_clutch_vector(28)
df |> head()
#>   par geno spore sex_start sex_end sex mat_start mat_end mat pon_start_1
#> 1   0    0     0        13    1000   0        12      13   6          20
#> 2   0    0     0        13    1000   0        12      13   3          19
#> 3   0    0     0        15    1000   0        14      15   1          18
#> 4   0    0     0        14    1000   0        13      14   6          20
#> 5   0    0     0        19    1000   0        18      19   2          21
#> 6   0    0     0        12    1000   0        11      12   1          18
#>   pon_end_1 pon_size_1 pon_start_2 pon_end_2 pon_size_2 pon_start_3 pon_end_3
#> 1        21          3          23        24          9          26        27
#> 2        20          5          23        24          6          27        28
#> 3        19          5          22        23         10          25        26
#> 4        21          7          24        25          6          27        28
#> 5        22          9          25        26          6          29        30
#> 6        19          6          23        24          6          26        27
#>   pon_size_3 pon_start_4 pon_end_4 pon_size_4 pon_start_5 pon_end_5 pon_size_5
#> 1          3          30        31          2          33        34          8
#> 2          3          31        32          6          35        36          9
#> 3          5          29        30          5          33        34          6
#> 4          5          31        32          8          35        36          8
#> 5          6          33        34          6          38        39          3
#> 6          4          30        31          4          34        35          5
#>   pon_start_6 pon_end_6 pon_size_6 pon_start_7 pon_end_7 pon_size_7 pon_start_8
#> 1          37        38          5          41        42          5          45
#> 2          39        40          9          43        44          4          47
#> 3          37        38          4          41        42          8          45
#> 4          39        40          7          43        44          7          47
#> 5          41        42          9          45        46          6          49
#> 6          38        39          8          42        43          6          46
#>   pon_end_8 pon_size_8 pon_start_9 pon_end_9 pon_size_9 pon_start_10 pon_end_10
#> 1        46          7          48        49          3           53         54
#> 2        48          7          51        52          2           56         57
#> 3        46          6          49        50          3           54         55
#> 4        48          8          51        52          5           57         58
#> 5        50          2          54        55          3           58         59
#> 6        47          7          51        52          5           56         57
#>   pon_size_10 pon_start_11 pon_end_11 pon_size_11 pon_start_12 pon_end_12
#> 1           4           58         59           3           62         63
#> 2           3           60         61           4           65         66
#> 3           2           57         58           4           62         63
#> 4           3           61         62           2           65         66
#> 5           6           65         66           1           68         69
#> 6           4           60         61           4           65         66
#>   pon_size_12 pon_start_13 pon_end_13 pon_size_13 pon_start_14 pon_end_14
#> 1           4           67         68           3           72         73
#> 2           3           70         71           2           74         75
#> 3           5           66         67           3           72         73
#> 4           3           NA         NA          NA           NA         NA
#> 5           4           73         74           2           77         78
#> 6           2           NA         NA          NA           NA         NA
#>   pon_size_14 pon_start_15 pon_end_15 pon_size_15 pon_start_16 pon_end_16
#> 1           2           77         78           2           82         83
#> 2           7           80         81           3           84         85
#> 3           2           76         77           5           NA         NA
#> 4          NA           NA         NA          NA           NA         NA
#> 5           2           82         83           4           87         88
#> 6          NA           NA         NA          NA           NA         NA
#>   pon_size_16 pon_start_17 pon_end_17 pon_size_17 pon_start_18 pon_end_18
#> 1           2           87         88           3           92         93
#> 2           2           90         91           1           NA         NA
#> 3          NA           NA         NA          NA           NA         NA
#> 4          NA           NA         NA          NA           NA         NA
#> 5           2           93         94           2           97         98
#> 6          NA           NA         NA          NA           NA         NA
#>   pon_size_18 pon_start_19 pon_end_19 pon_size_19 pon_start_20 pon_end_20
#> 1           3           97         98           3           NA         NA
#> 2          NA           NA         NA          NA           NA         NA
#> 3          NA           NA         NA          NA           NA         NA
#> 4          NA           NA         NA          NA           NA         NA
#> 5           6          103        104           3          124        125
#> 6          NA           NA         NA          NA           NA         NA
#>   pon_size_20 pon_start_21 pon_end_21 pon_size_21 pon_start_22 pon_end_22
#> 1          NA           NA         NA          NA           NA         NA
#> 2          NA           NA         NA          NA           NA         NA
#> 3          NA           NA         NA          NA           NA         NA
#> 4          NA           NA         NA          NA           NA         NA
#> 5           4          128        129           4           NA         NA
#> 6          NA           NA         NA          NA           NA         NA
#>   pon_size_22 pon_start_23 pon_end_23 pon_size_23 pon_start_24 pon_end_24
#> 1          NA           NA         NA          NA           NA         NA
#> 2          NA           NA         NA          NA           NA         NA
#> 3          NA           NA         NA          NA           NA         NA
#> 4          NA           NA         NA          NA           NA         NA
#> 5          NA           NA         NA          NA           NA         NA
#> 6          NA           NA         NA          NA           NA         NA
#>   pon_size_24 pon_start_25 pon_end_25 pon_size_25 pon_start_26 pon_end_26
#> 1          NA           NA         NA          NA           NA         NA
#> 2          NA           NA         NA          NA           NA         NA
#> 3          NA           NA         NA          NA           NA         NA
#> 4          NA           NA         NA          NA           NA         NA
#> 5          NA           NA         NA          NA           NA         NA
#> 6          NA           NA         NA          NA           NA         NA
#>   pon_size_26 pon_start_27 pon_end_27 pon_size_27 pon_start_28 pon_end_28
#> 1          NA           NA         NA          NA           NA         NA
#> 2          NA           NA         NA          NA           NA         NA
#> 3          NA           NA         NA          NA           NA         NA
#> 4          NA           NA         NA          NA           NA         NA
#> 5          NA           NA         NA          NA           NA         NA
#> 6          NA           NA         NA          NA           NA         NA
#>   pon_size_28 death_start death_end
#> 1          NA         102       103
#> 2          NA          95        96
#> 3          NA          78        79
#> 4          NA          66        67
#> 5          NA         135       136
#> 6          NA          74        75
```

### Creata a data lifelihood object

``` r

dataLFH <- lifelihoodData(
  df = df,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "mat_start",
  maturity_end = "mat_end",
  clutchs = clutchs,
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("par", "spore"),
  model_specs = c("wei", "lgn", "wei")
)
```

### Estimation

``` r

results <- dataLFH |>
  lifelihood(path_config = get_config_path("config_pierrick"))
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos /Users/runner/work/Lifelihood/Lifelihood/lifelihood_/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_/temp_param_range_path.txt FALSE 0 25 FALSE 0 FALSE 0 1343 3448 2283 7709 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
```

### AIC & BIC

``` r

AIC(results)
#> [1] 150338.9
BIC(results)
#> [1] 150407.9
```

### Summary results

``` r

coef(results)
#>              int_expt_death        eff_expt_death_par_1 
#>                  -0.9107523                  -0.4336383 
#>        eff_expt_death_par_2      eff_expt_death_spore_1 
#>                  -0.5267381                  -0.3901389 
#>      eff_expt_death_spore_2      eff_expt_death_spore_3 
#>                  -0.3080676                  -0.2888894 
#>         int_survival_param2           int_expt_maturity 
#>                  -4.8737473                   1.8304244 
#>     eff_expt_maturity_par_1     eff_expt_maturity_par_2 
#>                   0.7223636                   2.4607189 
#>         int_maturity_param2       int_expt_reproduction 
#>                  -1.7704551                  -1.7842157 
#> eff_expt_reproduction_par_1 eff_expt_reproduction_par_2 
#>                  -1.5654294                  -0.8478839 
#>     int_reproduction_param2             int_n_offspring 
#>                  -1.1489510                  -2.5421892
coeff(results, "expt_death")
#>         int_expt_death   eff_expt_death_par_1   eff_expt_death_par_2 
#>             -0.9107523             -0.4336383             -0.5267381 
#> eff_expt_death_spore_1 eff_expt_death_spore_2 eff_expt_death_spore_3 
#>             -0.3901389             -0.3080676             -0.2888894
coeff(results, "survival_param2")
#> int_survival_param2 
#>           -4.873747

AIC(results)
#> [1] 150338.9
BIC(results)
#> [1] 150407.9

logLik(results)
#> [1] -75153.47
```

### Prediction on new data

``` r

newdata <- tibble(
  par = c(0, 1, 2, 0, 1, 2, 1),
  spore = c(0, 1, 2, 1, 0, 1, 1)
) |>
  mutate(
    par = as.factor(par),
    spore = as.factor(spore)
  )

prediction(results, "expt_death", newdata = newdata)
#> [1] -0.9107523 -1.7345294 -1.7455580 -1.3008912 -1.3443906 -1.8276293 -1.7345294
prediction(results, "expt_death", newdata = newdata, type = "response")
#> [1] 92.93879 48.60381 48.14996 69.34167 67.00042 44.88164 48.60381
```

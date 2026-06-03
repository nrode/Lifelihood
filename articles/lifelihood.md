# Overview of all features in `lifelihood`

### Load lifelihood

``` r

library(lifelihood)
#> Loading required package: tidyverse
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.2.1     ✔ readr     2.2.0
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
#> ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
#> ✔ purrr     1.2.2     
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
    "clutch",
    rep(c("start", "end", "size"), N),
    rep(1:N, each = 3),
    sep = "_"
  ))
}
clutchs <- generate_clutch_vector(28)
df |> head()
#>   par geno spore sex_start sex_end sex mat_start mat_end mat clutch_start_1
#> 1   0    0     0        13    1000   0        12      13   6             20
#> 2   0    0     0        13    1000   0        12      13   3             19
#> 3   0    0     0        15    1000   0        14      15   1             18
#> 4   0    0     0        14    1000   0        13      14   6             20
#> 5   0    0     0        19    1000   0        18      19   2             21
#> 6   0    0     0        12    1000   0        11      12   1             18
#>   clutch_end_1 clutch_size_1 clutch_start_2 clutch_end_2 clutch_size_2
#> 1           21             3             23           24             9
#> 2           20             5             23           24             6
#> 3           19             5             22           23            10
#> 4           21             7             24           25             6
#> 5           22             9             25           26             6
#> 6           19             6             23           24             6
#>   clutch_start_3 clutch_end_3 clutch_size_3 clutch_start_4 clutch_end_4
#> 1             26           27             3             30           31
#> 2             27           28             3             31           32
#> 3             25           26             5             29           30
#> 4             27           28             5             31           32
#> 5             29           30             6             33           34
#> 6             26           27             4             30           31
#>   clutch_size_4 clutch_start_5 clutch_end_5 clutch_size_5 clutch_start_6
#> 1             2             33           34             8             37
#> 2             6             35           36             9             39
#> 3             5             33           34             6             37
#> 4             8             35           36             8             39
#> 5             6             38           39             3             41
#> 6             4             34           35             5             38
#>   clutch_end_6 clutch_size_6 clutch_start_7 clutch_end_7 clutch_size_7
#> 1           38             5             41           42             5
#> 2           40             9             43           44             4
#> 3           38             4             41           42             8
#> 4           40             7             43           44             7
#> 5           42             9             45           46             6
#> 6           39             8             42           43             6
#>   clutch_start_8 clutch_end_8 clutch_size_8 clutch_start_9 clutch_end_9
#> 1             45           46             7             48           49
#> 2             47           48             7             51           52
#> 3             45           46             6             49           50
#> 4             47           48             8             51           52
#> 5             49           50             2             54           55
#> 6             46           47             7             51           52
#>   clutch_size_9 clutch_start_10 clutch_end_10 clutch_size_10 clutch_start_11
#> 1             3              53            54              4              58
#> 2             2              56            57              3              60
#> 3             3              54            55              2              57
#> 4             5              57            58              3              61
#> 5             3              58            59              6              65
#> 6             5              56            57              4              60
#>   clutch_end_11 clutch_size_11 clutch_start_12 clutch_end_12 clutch_size_12
#> 1            59              3              62            63              4
#> 2            61              4              65            66              3
#> 3            58              4              62            63              5
#> 4            62              2              65            66              3
#> 5            66              1              68            69              4
#> 6            61              4              65            66              2
#>   clutch_start_13 clutch_end_13 clutch_size_13 clutch_start_14 clutch_end_14
#> 1              67            68              3              72            73
#> 2              70            71              2              74            75
#> 3              66            67              3              72            73
#> 4              NA            NA             NA              NA            NA
#> 5              73            74              2              77            78
#> 6              NA            NA             NA              NA            NA
#>   clutch_size_14 clutch_start_15 clutch_end_15 clutch_size_15 clutch_start_16
#> 1              2              77            78              2              82
#> 2              7              80            81              3              84
#> 3              2              76            77              5              NA
#> 4             NA              NA            NA             NA              NA
#> 5              2              82            83              4              87
#> 6             NA              NA            NA             NA              NA
#>   clutch_end_16 clutch_size_16 clutch_start_17 clutch_end_17 clutch_size_17
#> 1            83              2              87            88              3
#> 2            85              2              90            91              1
#> 3            NA             NA              NA            NA             NA
#> 4            NA             NA              NA            NA             NA
#> 5            88              2              93            94              2
#> 6            NA             NA              NA            NA             NA
#>   clutch_start_18 clutch_end_18 clutch_size_18 clutch_start_19 clutch_end_19
#> 1              92            93              3              97            98
#> 2              NA            NA             NA              NA            NA
#> 3              NA            NA             NA              NA            NA
#> 4              NA            NA             NA              NA            NA
#> 5              97            98              6             103           104
#> 6              NA            NA             NA              NA            NA
#>   clutch_size_19 clutch_start_20 clutch_end_20 clutch_size_20 clutch_start_21
#> 1              3              NA            NA             NA              NA
#> 2             NA              NA            NA             NA              NA
#> 3             NA              NA            NA             NA              NA
#> 4             NA              NA            NA             NA              NA
#> 5              3             124           125              4             128
#> 6             NA              NA            NA             NA              NA
#>   clutch_end_21 clutch_size_21 clutch_start_22 clutch_end_22 clutch_size_22
#> 1            NA             NA              NA            NA             NA
#> 2            NA             NA              NA            NA             NA
#> 3            NA             NA              NA            NA             NA
#> 4            NA             NA              NA            NA             NA
#> 5           129              4              NA            NA             NA
#> 6            NA             NA              NA            NA             NA
#>   clutch_start_23 clutch_end_23 clutch_size_23 clutch_start_24 clutch_end_24
#> 1              NA            NA             NA              NA            NA
#> 2              NA            NA             NA              NA            NA
#> 3              NA            NA             NA              NA            NA
#> 4              NA            NA             NA              NA            NA
#> 5              NA            NA             NA              NA            NA
#> 6              NA            NA             NA              NA            NA
#>   clutch_size_24 clutch_start_25 clutch_end_25 clutch_size_25 clutch_start_26
#> 1             NA              NA            NA             NA              NA
#> 2             NA              NA            NA             NA              NA
#> 3             NA              NA            NA             NA              NA
#> 4             NA              NA            NA             NA              NA
#> 5             NA              NA            NA             NA              NA
#> 6             NA              NA            NA             NA              NA
#>   clutch_end_26 clutch_size_26 clutch_start_27 clutch_end_27 clutch_size_27
#> 1            NA             NA              NA            NA             NA
#> 2            NA             NA              NA            NA             NA
#> 3            NA             NA              NA            NA             NA
#> 4            NA             NA              NA            NA             NA
#> 5            NA             NA              NA            NA             NA
#> 6            NA             NA              NA            NA             NA
#>   clutch_start_28 clutch_end_28 clutch_size_28 death_start death_end
#> 1              NA            NA             NA         102       103
#> 2              NA            NA             NA          95        96
#> 3              NA            NA             NA          78        79
#> 4              NA            NA             NA          66        67
#> 5              NA            NA             NA         135       136
#> 6              NA            NA             NA          74        75
```

### Creata a data lifelihood object

``` r

dataLFH <- as_lifelihoodData(
  df = df,
  matclutch = FALSE,
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
  lifelihood(path_config = use_test_config("config_pierrick"))
#> [1] "/Users/runner/work/_temp/Library/lifelihood/bin/lifelihood-macos-aarch64 /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9245_9771_5478_2984/temp_file_data_lifelihood.txt /Users/runner/work/Lifelihood/Lifelihood/lifelihood_9245_9771_5478_2984/temp_param_range_path.txt 0 25 FALSE 0 FALSE 0 9245 9771 5478 2984 10 20 1000 0.3 NULL 2 2 50 1 1 0.001"
```

### AIC & BIC

``` r

AIC(results)
#> [1] 64960.43
BIC(results)
#> [1] 65003.53
```

### Summary results

``` r

coef(results)
#>          int_expt_death    eff_expt_death_par_1    eff_expt_death_par_2 
#>               -0.911175               -1.560680               -1.579087 
#>     int_survival_param2    int_ratio_expt_death       int_expt_maturity 
#>               -4.878733               -3.903737               -1.469884 
#>     int_maturity_param2   int_expt_reproduction int_reproduction_param2 
#>               -7.354207               -1.803183               -1.152709 
#>         int_n_offspring 
#>               -2.552755
coeff(results, "expt_death")
#>       int_expt_death eff_expt_death_par_1 eff_expt_death_par_2 
#>            -0.911175            -1.560680            -1.579087
coeff(results, "survival_param2")
#> int_survival_param2 
#>           -4.878733

AIC(results)
#> [1] 64960.43
BIC(results)
#> [1] 65003.53

logLik(results)
#> [1] -32470.21
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
#> [1] -0.911175 -2.471855 -2.490262 -0.911175 -2.471855 -2.490262 -2.471855
prediction(results, "expt_death", newdata = newdata, type = "response")
#> numeric(0)
```

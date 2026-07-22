# Simulation and visit masks

`lifelihood` offers a
[`lifelihood::simulate_life_history()`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md)
function that lets you simulate new observations based on estimates you
made.

## Fitting

First, we need to fit the model with
[`lifelihood::lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md):

``` r

devtools::load_all()
#> ℹ Loading lifelihood
#> Loading required package: tidyverse
#> 
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
```

``` r

library(tidyverse)

df <- datapierrick |>
  mutate(
    par = as.factor(par),
    spore = as.factor(spore),
    block = rep(1:2, each = nrow(datapierrick) / 2)
  )

clutchs <- generate_clutch_vector(28)

lifelihoodData <- as_lifelihoodData(
  df = df,
  matclutch = FALSE,
  sex = "sex",
  sex_start = "sex_start",
  sex_end = "sex_end",
  maturity_start = "mat_start",
  maturity_end = "mat_end",
  clutchs = clutchs,
  block = "block",
  death_start = "death_start",
  death_end = "death_end",
  covariates = c("par", "spore"),
  dist = c("wei", "lgn", "wei")
)

results <- lifelihood(
  lifelihoodData,
  path_config = use_test_config("config_pierrick")
)

summary(results)
#> 
#> === LIFELIHOOD RESULTS ===
#> 
#> Sample size: 550 
#> 
#> --- Model Fit ---
#> Log-likelihood:  -32472.896
#> AIC:             64965.8
#> BIC:             65008.9
#> 
#> --- Key Parameters ---
#> 
#> Mortality:
#>   expt_death (Intercept)    -0.919 (0.000)
#>   expt_death eff_expt_death_par_1 -2.463 (0.000)
#>   expt_death eff_expt_death_par_2 -2.484 (0.000)
#>   survival_param2 (Intercept) -4.882 (0.000)
#>   ratio_expt_death (Intercept) -3.002 (0.000)
#> 
#> Maturity:
#>   expt_maturity (Intercept) -1.479 (0.000)
#>   maturity_param2 (Intercept) -3.260 (0.000)
#> 
#> Reproduction:
#>   expt_reproduction (Intercept) -4.253 (0.000)
#>   reproduction_param2 (Intercept) -5.357 (0.000)
#>   n_offspring (Intercept)   -2.563 (0.000)
#> 
#> --- Convergence ---
#> All parameters within bounds
#> 
#> ======================
```

## Default simulations

By default, `lifelihood` will simulate all life history events
(maturity, reproduction, and death):

``` r

simulate_life_history(results) |> head()
#> # A tibble: 6 × 108
#>   par   spore block   sex sex_start sex_end total_n_offspring total_n_clutches
#>   <fct> <fct> <int> <int>     <int>   <int>             <dbl>            <dbl>
#> 1 0     0         1     0        13    1000                25                6
#> 2 0     0         1     0        13    1000                64               14
#> 3 0     0         1     0        15    1000               110               25
#> 4 0     0         1     0        14    1000                30                8
#> 5 0     0         1     0        19    1000                53               16
#> 6 0     0         1     0        12    1000                58               11
#> # ℹ 100 more variables: maturity_start <dbl>, maturity_end <dbl>,
#> #   clutch_start_1 <dbl>, clutch_end_1 <dbl>, clutch_size_1 <int>,
#> #   clutch_start_2 <dbl>, clutch_end_2 <dbl>, clutch_size_2 <int>,
#> #   clutch_start_3 <dbl>, clutch_end_3 <dbl>, clutch_size_3 <int>,
#> #   clutch_start_4 <dbl>, clutch_end_4 <dbl>, clutch_size_4 <int>,
#> #   clutch_start_5 <dbl>, clutch_end_5 <dbl>, clutch_size_5 <int>,
#> #   clutch_start_6 <dbl>, clutch_end_6 <dbl>, clutch_size_6 <int>, …
```

But you can specify which event you want:

``` r

simulate_life_history(results, event = "maturity") |> head()
#> # A tibble: 6 × 10
#>   par   spore block   sex sex_start sex_end maturity_start maturity_end
#>   <fct> <fct> <int> <int>     <int>   <int>          <dbl>        <dbl>
#> 1 0     0         1     0        13    1000           13.0         13.0
#> 2 0     0         1     0        13    1000           13.8         13.8
#> 3 0     0         1     0        15    1000           13.1         13.1
#> 4 0     0         1     0        14    1000           11.7         11.7
#> 5 0     0         1     0        19    1000           12.6         12.6
#> 6 0     0         1     0        12    1000           13.5         13.5
#> # ℹ 2 more variables: total_n_offspring <dbl>, total_n_clutches <dbl>
```

## Simulations with visit masks

`lifelihood` lets you specify visit masks that are used to simulate data
that more closely reflects how the original data was measured by adding
constraints to the interval dates.

If you scroll to the top, you’ll see that we passed a column name from
our dataframe to the `block` argument. This column represents the block
to which each individual belongs. Use
[`get_visits()`](https://nrode.github.io/Lifelihood/reference/get_visits.md)
to retrieve the visit masks inferred from the original dataset:

``` r

visits <- get_visits(lifelihoodData)
visits |> head()
#> # A tibble: 6 × 2
#>   block visit
#>   <int> <int>
#> 1     1     0
#> 2     1     7
#> 3     1     8
#> 4     1     9
#> 5     1    10
#> 6     1    11
```

To use censoring intervals in the simulation, you can pass visits
explicitly along with `use_censoring = TRUE`:

``` r

results |>
  simulate_life_history(
    event = "maturity",
    use_censoring = TRUE,
    visits = visits
  ) |>
  head()
#> # A tibble: 6 × 11
#>   par   spore block   sex sex_start sex_end maturity maturity_start maturity_end
#>   <fct> <fct> <int> <int>     <int>   <int>    <dbl>          <dbl>        <dbl>
#> 1 0     0         1     0        13    1000     12.1             12           13
#> 2 0     0         1     0        13    1000     13.9             13           14
#> 3 0     0         1     0        15    1000     14.3             14           15
#> 4 0     0         1     0        14    1000     13.9             13           14
#> 5 0     0         1     0        19    1000     12.3             12           13
#> 6 0     0         1     0        12    1000     13.0             13           14
#> # ℹ 2 more variables: total_n_offspring <dbl>, total_n_clutches <dbl>
```

Ideally, we recommend to provide explicitly the date where each visit
occured for each block in the simulation. Indeed, that we see that, by
default, with the function
[`get_visits()`](https://nrode.github.io/Lifelihood/reference/get_visits.md)
above, Lifelihood considers that there was no visits for block 1 between
t=0 and t=7 (as no events occured in this time interval), also visits
occured every day.

You can also provide your own visit masks with the `visits` argument. It
must be a dataframe with 2 columns: `block` (the same name as passed to
[`lifelihood::as_lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md)
in the `block` argument) and exactly `visit`. For each block, `visit`
corresponds to the ages at which the events of individuals were
recorded.

Let’s define a custom visits dataframe:

``` r

custom_visits <- tibble(
  block = rep(1:2, nrow(datapierrick) / 2),
  visit = seq_len(nrow(datapierrick))
)
custom_visits |> head()
#> # A tibble: 6 × 2
#>   block visit
#>   <int> <int>
#> 1     1     1
#> 2     2     2
#> 3     1     3
#> 4     2     4
#> 5     1     5
#> 6     2     6
```

Now we can pass this to the
[`simulate_life_history()`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md)
function:

``` r

results |>
  simulate_life_history(
    event = "maturity",
    use_censoring = TRUE,
    visits = custom_visits
  ) |>
  head()
#> # A tibble: 6 × 11
#>   par   spore block   sex sex_start sex_end maturity maturity_start maturity_end
#>   <fct> <fct> <int> <int>     <int>   <int>    <dbl>          <dbl>        <dbl>
#> 1 0     0         1     0        13    1000     13.1             13           15
#> 2 0     0         1     0        13    1000     12.7             11           13
#> 3 0     0         1     0        15    1000     14.0             13           15
#> 4 0     0         1     0        14    1000     12.4             11           13
#> 5 0     0         1     0        19    1000     11.9             11           13
#> 6 0     0         1     0        12    1000     13.4             13           15
#> # ℹ 2 more variables: total_n_offspring <dbl>, total_n_clutches <dbl>
```

## Details

Requesting `event = "reproduction"` also simulates maturity and
mortality, because both events are required to determine when
reproduction can occur (e.g., after the maturity and before the death).
Visit masks apply to reproduction in both the standard simulation and
the reproduction-survival trade-off simulation.

``` r

results |>
  simulate_life_history(
    event = "reproduction",
    use_censoring = TRUE,
    visits = visits,
    seed = 1
  ) |>
  head()
#> # A tibble: 6 × 142
#>   par   spore block   sex sex_start sex_end mortality mortality_start
#>   <fct> <fct> <int> <int>     <int>   <int>     <dbl>           <dbl>
#> 1 0     0         1     0        13    1000      79.2              79
#> 2 0     0         1     0        13    1000      80.3              80
#> 3 0     0         1     0        15    1000      82.6              82
#> 4 0     0         1     0        14    1000      93.2              93
#> 5 0     0         1     0        19    1000      91.5              91
#> 6 0     0         1     0        12    1000      70.4              70
#> # ℹ 134 more variables: mortality_end <dbl>, maturity <dbl>,
#> #   maturity_start <dbl>, maturity_end <dbl>, clutch_1 <dbl>,
#> #   clutch_start_1 <dbl>, clutch_end_1 <dbl>, clutch_size_1 <int>,
#> #   clutch_2 <dbl>, clutch_start_2 <dbl>, clutch_end_2 <dbl>,
#> #   clutch_size_2 <int>, clutch_3 <dbl>, clutch_start_3 <dbl>,
#> #   clutch_end_3 <dbl>, clutch_size_3 <int>, clutch_4 <dbl>,
#> #   clutch_start_4 <dbl>, clutch_end_4 <dbl>, clutch_size_4 <int>, …
```

The following rules define how visit masks are constructed and applied:

- Visit masks are block-specific. The `visits` data frame must contain
  the configured block column and a `visit` column, with visit times for
  every block represented in the simulation. When `newdata` is supplied,
  it must also contain the configured block column.
- [`get_visits()`](https://nrode.github.io/Lifelihood/reference/get_visits.md)
  collects the observed sex, maturity, and clutch interval bounds. It
  removes missing values and the right-censoring date, then deduplicates
  and sorts visit times within each block.
- An event is censored by the last visit before it occured and the first
  visit after it occured. The start date is set to `NA` when the event
  precedes the first visit, and the end date is set to `NA` when it
  follows the last visit.
- Reproduction masks are computed from absolute clutch ages. Maturity is
  added to the first inter-clutch duration, later durations are
  accumulated, and clutches after mortality are removed before visit
  intervals are assigned.
- If several clutches from one individual occur between the same visits,
  they are represented by a single clutch slot and the values of clutch
  sizes are summed. The exact simulated age is kept and named
  `clutch_{i}`.
- The clutches are numbered chronologically.
- Reproduction values are `NA` for males. `total_n_offspring` is
  calculated after same-interval clutches have been merged, so
  aggregation does not change the lifetime total.

# Simulation and visit masks

`lifelihood` provides the
[`lifelihood::simulate_life_history()`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md)
function, which lets you simulate new observations based on your
estimates.

## Fitting

First, we need to fit the model with
[`lifelihood::lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md):

\
`devtools``::`[`load_all`](https://devtools.r-lib.org/reference/load_all.html)`(``)`\
`#> ℹ Loading lifelihood`

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyverse`](https://tidyverse.tidyverse.org)`)`\
`#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──`\
`#> ✔ dplyr     1.2.1     ✔ readr     2.2.0`\
`#> ✔ forcats   1.0.1     ✔ stringr   1.6.0`\
`#> ✔ ggplot2   4.0.3     ✔ tibble    3.3.1`\
`#> ✔ lubridate 1.9.5     ✔ tidyr     1.3.2`\
`#> ✔ purrr     1.2.2     `\
`#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──`\
`#> ✖ readr::edition_get()   masks testthat::edition_get()`\
`#> ✖ dplyr::filter()        masks lifelihood::filter(), stats::filter()`\
`#> ✖ dplyr::lag()           masks lifelihood::lag(), stats::lag()`\
`#> ✖ readr::local_edition() masks testthat::local_edition()`\
`#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors`\
\
`df`` ``<-`` ``datapierrick`` ``|>`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`    par ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``par``)``,`\
`    spore ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``spore``)``,`\
`    block ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``1``:``2``, each ``=`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``datapierrick``)`` ``/`` ``2``)`\
`  ``)`\
\
`clutchs`` ``<-`` `[`generate_clutch_vector`](https://nrode.github.io/Lifelihood/reference/generate_clutch_vector.md)`(``28``)`\
\
`lifelihoodData`` ``<-`` `[`as_lifelihoodData`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md)`(`\
`  df ``=`` ``df``,`\
`  matclutch ``=`` ``FALSE``,`\
`  sex ``=`` ``"sex"``,`\
`  sex_start ``=`` ``"sex_start"``,`\
`  sex_end ``=`` ``"sex_end"``,`\
`  maturity_start ``=`` ``"mat_start"``,`\
`  maturity_end ``=`` ``"mat_end"``,`\
`  clutchs ``=`` ``clutchs``,`\
`  block ``=`` ``"block"``,`\
`  death_start ``=`` ``"death_start"``,`\
`  death_end ``=`` ``"death_end"``,`\
`  covariates ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"par"``, ``"spore"``)``,`\
`  dist ``=`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``mortality ``=`` ``"wei"``, maturity ``=`` ``"lgn"``, reproduction ``=`` ``"wei"``)`\
`)`\
\
`config`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  mortality ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``expt_death ``=`` ``"par"``, survival_param2 ``=`` ``1``, ratio_expt_death ``=`` ``1``)``,`\
`  maturity ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``expt_maturity ``=`` ``1``, maturity_param2 ``=`` ``1``)``,`\
`  reproduction ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`    expt_reproduction ``=`` ``1``,`\
`    reproduction_param2 ``=`` ``1``,`\
`    n_offspring ``=`` ``1`\
`  ``)`\
`)`\
\
`results`` ``<-`` `[`lifelihood`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)`(`\
`  ``lifelihoodData``,`\
`  config ``=`` ``config`\
`)`\
\
[`summary`](https://rdrr.io/r/base/summary.html)`(``results``)`\
`#> `\
`#> === LIFELIHOOD RESULTS ===`\
`#> `\
`#> Sample size: 550 `\
`#> `\
`#> --- Model Fit ---`\
`#> Log-likelihood:  -32472.850`\
`#> AIC:             64965.7`\
`#> BIC:             65008.8`\
`#> `\
`#> --- Key Parameters ---`\
`#> `\
`#> Mortality:`\
`#>   expt_death (Intercept)    -0.913 (0.000)`\
`#>   expt_death eff_expt_death_par_1 -2.181 (0.000)`\
`#>   expt_death eff_expt_death_par_2 -2.200 (0.000)`\
`#>   survival_param2 (Intercept) -4.879 (0.000)`\
`#>   ratio_expt_death (Intercept) -3.299 (0.000)`\
`#> `\
`#> Maturity:`\
`#>   expt_maturity (Intercept) -1.410 (0.000)`\
`#>   maturity_param2 (Intercept) -3.299 (0.000)`\
`#> `\
`#> Reproduction:`\
`#>   expt_reproduction (Intercept) -4.258 (0.000)`\
`#>   reproduction_param2 (Intercept) -5.353 (0.000)`\
`#>   n_offspring (Intercept)   -2.553 (0.000)`\
`#> `\
`#> --- Convergence ---`\
`#> All parameters within bounds`\
`#> `\
`#> ======================`

## Default simulations

By default, `lifelihood` will simulate all life history events
(maturity, reproduction, and death):

\
[`simulate_life_history`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md)`(``results``)`` ``|>`` `[`head`](https://rdrr.io/r/utils/head.html)`(``)`\
`#> # A tibble: 6 × 117`\
`#>   par   spore block   sex sex_start sex_end total_n_offspring total_n_clutches`\
`#>   <fct> <fct> <int> <int>     <int>   <int>             <dbl>            <dbl>`\
`#> 1 0     0         1     0        13    1000                70               17`\
`#> 2 0     0         1     0        13    1000                76               16`\
`#> 3 0     0         1     0        15    1000                69               16`\
`#> 4 0     0         1     0        14    1000               128               33`\
`#> 5 0     0         1     0        19    1000               142               29`\
`#> 6 0     0         1     0        12    1000                16                4`\
`#> # ℹ 109 more variables: maturity_start <dbl>, maturity_end <dbl>,`\
`#> #   clutch_start_1 <dbl>, clutch_end_1 <dbl>, clutch_size_1 <int>,`\
`#> #   clutch_start_2 <dbl>, clutch_end_2 <dbl>, clutch_size_2 <int>,`\
`#> #   clutch_start_3 <dbl>, clutch_end_3 <dbl>, clutch_size_3 <int>,`\
`#> #   clutch_start_4 <dbl>, clutch_end_4 <dbl>, clutch_size_4 <int>,`\
`#> #   clutch_start_5 <dbl>, clutch_end_5 <dbl>, clutch_size_5 <int>,`\
`#> #   clutch_start_6 <dbl>, clutch_end_6 <dbl>, clutch_size_6 <int>, …`

But you can specify which event you want:

\
[`simulate_life_history`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md)`(``results``, event ``=`` ``"maturity"``)`` ``|>`` `[`head`](https://rdrr.io/r/utils/head.html)`(``)`\
`#> # A tibble: 6 × 10`\
`#>   par   spore block   sex sex_start sex_end maturity_start maturity_end`\
`#>   <fct> <fct> <int> <int>     <int>   <int>          <dbl>        <dbl>`\
`#> 1 0     0         1     0        13    1000           14.0         14.0`\
`#> 2 0     0         1     0        13    1000           14.0         14.0`\
`#> 3 0     0         1     0        15    1000           12.8         12.8`\
`#> 4 0     0         1     0        14    1000           13.5         13.5`\
`#> 5 0     0         1     0        19    1000           13.9         13.9`\
`#> 6 0     0         1     0        12    1000           14.0         14.0`\
`#> # ℹ 2 more variables: total_n_offspring <dbl>, total_n_clutches <dbl>`

## Simulations with visit masks

`lifelihood` lets you specify visit masks that are used to simulate data
that more closely reflects how the original data was measured by adding
constraints to the interval dates.

As shown near the top, we passed a column name from our data frame to
the `block` argument. This column identifies the block to which each
individual belongs. Use
[`get_visits()`](https://nrode.github.io/Lifelihood/reference/get_visits.md)
to retrieve the visit masks inferred from the original dataset:

\
`visits`` ``<-`` `[`get_visits`](https://nrode.github.io/Lifelihood/reference/get_visits.md)`(``lifelihoodData``)`\
`visits`` ``|>`` `[`head`](https://rdrr.io/r/utils/head.html)`(``)`\
`#> # A tibble: 6 × 2`\
`#>   block visit`\
`#>   <int> <int>`\
`#> 1     1     0`\
`#> 2     1     7`\
`#> 3     1     8`\
`#> 4     1     9`\
`#> 5     1    10`\
`#> 6     1    11`\
\
`custom_visits`` ``<-`` ``tidyr``::`[`expand_grid`](https://tidyr.tidyverse.org/reference/expand_grid.html)`(`\
`  block ``=`` `[`unique`](https://rdrr.io/r/base/unique.html)`(``df``$``block``)``,`\
`  visit ``=`` `[`seq`](https://rdrr.io/r/base/seq.html)`(``0``, ``lifelihoodData``$``right_censoring_date``)`\
`)`

To use censoring intervals in the simulation, pass visits explicitly:

\
`results`` ``|>`\
`  `[`simulate_life_history`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md)`(`\
`    event ``=`` ``"maturity"``,`\
`    visits ``=`` ``custom_visits`\
`  ``)`` ``|>`\
`  `[`head`](https://rdrr.io/r/utils/head.html)`(``)`\
`#> # A tibble: 6 × 11`\
`#>   par   spore block   sex sex_start sex_end maturity maturity_start maturity_end`\
`#>   <fct> <fct> <int> <int>     <int>   <int>    <dbl>          <dbl>        <dbl>`\
`#> 1 0     0         1     0        13    1000     14.3             14           15`\
`#> 2 0     0         1     0        13    1000     14.6             14           15`\
`#> 3 0     0         1     0        15    1000     13.2             13           14`\
`#> 4 0     0         1     0        14    1000     13.3             13           14`\
`#> 5 0     0         1     0        19    1000     14.0             14           15`\
`#> 6 0     0         1     0        12    1000     13.6             13           14`\
`#> # ℹ 2 more variables: total_n_offspring <dbl>, total_n_clutches <dbl>`

Ideally, provide the date of each visit explicitly for every block in
the simulation. By default, as shown by the
[`get_visits()`](https://nrode.github.io/Lifelihood/reference/get_visits.md)
output above, Lifelihood assumes there were no visits for block 1
between t = 0 and t = 7 because no events occurred during that interval.
It then assumes that visits occurred every day.

You can also provide your own visit masks with the `visits` argument. It
must be a data frame with two columns: `block` (using the same name as
the `block` argument passed to
[`lifelihood::as_lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md))
and `visit`. For each block, `visit` contains the ages at which
individuals’ events were recorded.

Let’s inspect the custom visits data frame:

\
`custom_visits`` ``|>`` `[`head`](https://rdrr.io/r/utils/head.html)`(``)`\
`#> # A tibble: 6 × 2`\
`#>   block visit`\
`#>   <int> <int>`\
`#> 1     1     0`\
`#> 2     1     1`\
`#> 3     1     2`\
`#> 4     1     3`\
`#> 5     1     4`\
`#> 6     1     5`

Now we can pass this to the
[`simulate_life_history()`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md)
function:

\
`results`` ``|>`\
`  `[`simulate_life_history`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md)`(`\
`    event ``=`` ``"maturity"``,`\
`    visits ``=`` ``custom_visits`\
`  ``)`` ``|>`\
`  `[`head`](https://rdrr.io/r/utils/head.html)`(``)`\
`#> # A tibble: 6 × 11`\
`#>   par   spore block   sex sex_start sex_end maturity maturity_start maturity_end`\
`#>   <fct> <fct> <int> <int>     <int>   <int>    <dbl>          <dbl>        <dbl>`\
`#> 1 0     0         1     0        13    1000     14.2             14           15`\
`#> 2 0     0         1     0        13    1000     15.0             15           16`\
`#> 3 0     0         1     0        15    1000     13.2             13           14`\
`#> 4 0     0         1     0        14    1000     13.3             13           14`\
`#> 5 0     0         1     0        19    1000     13.2             13           14`\
`#> 6 0     0         1     0        12    1000     14.4             14           15`\
`#> # ℹ 2 more variables: total_n_offspring <dbl>, total_n_clutches <dbl>`

## Details

Requesting `event = "reproduction"` also simulates maturity and
mortality, because both events are required to determine when
reproduction can occur (e.g., after the maturity and before the death).
Visit masks apply to reproduction in both the standard simulation and
the reproduction-survival trade-off simulation.

\
`results`` ``|>`\
`  `[`simulate_life_history`](https://nrode.github.io/Lifelihood/reference/simulate_life_history.md)`(`\
`    event ``=`` ``"reproduction"``,`\
`    visits ``=`` ``custom_visits``,`\
`    seed ``=`` ``1`\
`  ``)`` ``|>`\
`  `[`head`](https://rdrr.io/r/utils/head.html)`(``)`\
`#> # A tibble: 6 × 116`\
`#>   par   spore block   sex sex_start sex_end mortality maturity maturity_start`\
`#>   <fct> <fct> <int> <int>     <int>   <int>     <dbl>    <dbl>          <dbl>`\
`#> 1 0     0         1     0        13    1000      79.6     13.4             13`\
`#> 2 0     0         1     0        13    1000      80.7     13.8             13`\
`#> 3 0     0         1     0        15    1000      82.9     13.2             13`\
`#> 4 0     0         1     0        14    1000      93.6     14.7             14`\
`#> 5 0     0         1     0        19    1000      91.9     13.9             13`\
`#> 6 0     0         1     0        12    1000      70.8     13.2             13`\
`#> # ℹ 107 more variables: maturity_end <dbl>, mortality_start <dbl>,`\
`#> #   mortality_end <dbl>, clutch_start_1 <dbl>, clutch_end_1 <dbl>,`\
`#> #   clutch_size_1 <int>, clutch_start_2 <dbl>, clutch_end_2 <dbl>,`\
`#> #   clutch_size_2 <int>, clutch_start_3 <dbl>, clutch_end_3 <dbl>,`\
`#> #   clutch_size_3 <int>, clutch_start_4 <dbl>, clutch_end_4 <dbl>,`\
`#> #   clutch_size_4 <int>, clutch_start_5 <dbl>, clutch_end_5 <dbl>,`\
`#> #   clutch_size_5 <int>, clutch_start_6 <dbl>, clutch_end_6 <dbl>, …`

The following rules define how visit masks are constructed and applied:

- Visit masks are block-specific. The `visits` data frame must contain
  the configured block column and a `visit` column, with visit times for
  every block represented in the simulation. When `newdata` is supplied,
  it must also contain the configured block column.
- [`get_visits()`](https://nrode.github.io/Lifelihood/reference/get_visits.md)
  collects the observed sex, maturity, and clutch interval bounds. It
  removes missing values and the right-censoring date, then deduplicates
  and sorts visit times within each block.
- An event is censored by the last visit before it occurred and the
  first visit after it occurred. The supplied visits must cover every
  non-missing event age for its block; an error is raised when an event
  precedes the first visit or follows the last visit.
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

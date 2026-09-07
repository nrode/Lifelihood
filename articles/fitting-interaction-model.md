# Fit interaction model using `group_by_group = TRUE`

## Create a `lifelihoodData` object

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`lifelihood`](https://nrode.github.io/Lifelihood/)`)`\
`#> Loading required package: tidyverse`\
`#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──`\
`#> ✔ dplyr     1.2.1     ✔ readr     2.2.0`\
`#> ✔ forcats   1.0.1     ✔ stringr   1.6.0`\
`#> ✔ ggplot2   4.0.3     ✔ tibble    3.3.1`\
`#> ✔ lubridate 1.9.5     ✔ tidyr     1.3.2`\
`#> ✔ purrr     1.2.2     `\
`#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──`\
`#> ✖ dplyr::filter() masks stats::filter()`\
`#> ✖ dplyr::lag()    masks stats::lag()`\
`#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyverse`](https://tidyverse.tidyverse.org)`)`\
\
`df`` ``<-`` ``datapierrick`` ``|>`\
`  `[`as_tibble`](https://tibble.tidyverse.org/reference/as_tibble.html)`(``)`` ``|>`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`    par ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``par``)``,`\
`    geno ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``geno``)``,`\
`    spore ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``spore``)`\
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
`  death_start ``=`` ``"death_start"``,`\
`  death_end ``=`` ``"death_end"``,`\
`  covariates ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"par"``, ``"geno"``)``,`\
`  dist ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``mortality ``=`` ``"wei"``, maturity ``=`` ``"gam"``, reproduction ``=`` ``"lgn"``)`\
`)`\
\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``42``)`

## Difference between default and interaction models

- Default model

\
`time_default`` ``<-`` `[`system.time`](https://rdrr.io/r/base/system.time.html)`(``{`\
`  ``results_default`` ``<-`` `[`lifelihood`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)`(`\
`    ``lifelihoodData``,`\
`    path_config ``=`` `[`use_test_config`](https://nrode.github.io/Lifelihood/reference/use_test_config.md)`(``"config_gbg"``)``,`\
`    group_by_group ``=`` ``FALSE`` ``# Default value`\
`  ``)`\
`}``)`\
`time_default`\
`#>    user  system elapsed `\
`#>  31.393   0.348  32.914`

- Interaction model using the `group_by_group` argument (default to
  `FALSE`)

\
`time_gbg`` ``<-`` `[`system.time`](https://rdrr.io/r/base/system.time.html)`(``{`\
`  ``results_gbg`` ``<-`` `[`lifelihood`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)`(`\
`    ``lifelihoodData``,`\
`    path_config ``=`` `[`use_test_config`](https://nrode.github.io/Lifelihood/reference/use_test_config.md)`(``"config_gbg"``)``,`\
`    group_by_group ``=`` ``TRUE`\
`  ``)`\
`}``)`\
`time_gbg`\
`#>    user  system elapsed `\
`#>   1.975   0.100   2.129`

Fitting interaction model with group by group is faster than default
model.

- Comparison

\
`results_default``$``likelihood`\
`#> [1] -343781.9`\
`results_gbg``$``likelihood`\
`#> [1] -343764.9`

Log-likelihood is also higher with group by group indicating better
convergence towards the maximum log-likelihood value.

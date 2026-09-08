# Compute fitted mortality rate

Calculate the empirical mortality rate over a given interval on some new
data.

## Usage

``` r
compute_fitted_event_rate(
  lifelihoodResults,
  interval_width,
  event = c("mortality", "maturity", "reproduction"),
  newdata = NULL,
  max_time = NULL,
  groupby = NULL,
  mcmc.ci.fit = FALSE
)
```

## Arguments

- lifelihoodResults:

  output of
  [`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md).

- interval_width:

  The interval width used to calculate the event rate. For instance, if
  the time unit for deaths in the original dataset is days and
  `interval_width` is set to 10, the event rate will be calculated every
  10 days for each group.

- event:

  Which event to compute? Must be one of "mortality", "maturity",
  "reproduction".

- newdata:

  Optional `data.frame` providing covariate values for prediction. If
  `NULL`, the original model data are used.

- max_time:

  The maximum time for calculating the event rate. If set to NULL, the
  time of the last observed death is used.

- groupby:

  One or multiple covariates used to group the computation.

- mcmc.ci.fit:

  Whether or not to retrieve MCMC CI estimations.

## Value

A dataframe with 3 columns: Interval (time interval, based on
`interval_width` value), group (identifier of a given subgroup, or
"Overall" if groupby = NULL), and Event_rate (event rate over the
interval). Note that for reproduction event, the first reproduction
event of each individual cannot be computed if maturity was not observed
(i.e. mat_clutch is true) When the interval between the last
reproduction event of an individual and their death is greater than
`interval_width` the individuals are included in the computation of
reproduction rate

## Examples

``` r
df <- datapierrick |>
as_tibble() |>
 mutate(par = as.factor(par))

# name of the columns of the clutchs into a single vector
clutchs <- generate_clutch_vector(28)
dataLFH <- as_lifelihoodData(
 df = df,
 sex = "sex",
 sex_start = "sex_start",
 sex_end = "sex_end",
 maturity_start = "mat_start",
 maturity_end = "mat_end",
 clutchs = clutchs,
 death_start = "death_start",
 death_end = "death_end",
 matclutch = FALSE,
 covariates = c("par", "geno"),
 dist = c(mortality = "wei", maturity = "gam", reproduction = "lgn")
)
results <- lifelihood(
lifelihoodData = dataLFH,
config = list(
 mortality = list(expt_death = "par", survival_param2 = 1),
 maturity = list(expt_maturity = 1, maturity_param2 = 1),
 reproduction = list(
   expt_reproduction = 1,
   reproduction_param2 = 1,
   n_offspring = 1
 )
),
seeds = c(1, 2, 3, 4)
)
fitted_emergence_rate <- compute_fitted_event_rate(
 lifelihoodResults = results,
 interval_width = 5,
 event = c("mortality"),
 max_time=150,
 groupby=c("par"))|>
 dplyr::mutate(sex = paste0("sex=", sex))
#> Error in dplyr::mutate(compute_fitted_event_rate(lifelihoodResults = results,     interval_width = 5, event = c("mortality"), max_time = 150,     groupby = c("par")), sex = paste0("sex=", sex)): ℹ In argument: `sex = paste0("sex=", sex)`.
#> Caused by error:
#> ! object 'sex' not found

p <- fitted_emergence_rate |>
 ggplot2::ggplot(
   ggplot2::aes(
     x = Mean_Interval,
     y = Event_Rate,
     color = par,
     shape = par
   )
 )+
 geom_point()+
 geom_line(linewidth=0.5)+
 xlab("Time (days)")+
 ylab("Fitted mortality rate over 5 day-periods")+
 facet_wrap(vars(sex, par), labeller = "label_both")
#> Error: object 'fitted_emergence_rate' not found
p
#> Error: object 'p' not found
```

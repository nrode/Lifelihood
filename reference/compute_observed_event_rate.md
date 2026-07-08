# Compute empirical event rate

Calculate the empirical event rate over a given interval.

## Usage

``` r
compute_observed_event_rate(
  lifelihoodData,
  interval_width,
  event = c("mortality", "maturity", "reproduction"),
  max_time = NULL,
  min_sample_size = 1,
  groupby = NULL
)
```

## Arguments

- lifelihoodData:

  `lifelihoodData` object created with
  [`as_lifelihoodData()`](https://nrode.github.io/Lifelihood/reference/as_lifelihoodData.md).

- interval_width:

  The interval width used to calculate the event rate. For instance, if
  the time unit for deaths in the original dataset is days and
  `interval_width` is set to 10, the event rate will be calculated every
  10 days for each group.

- event:

  Which event to compute? Must be one of "mortality", "maturity",
  "reproduction".

- max_time:

  The maximum time for calculating the event rate. If set to NULL, the
  time of the last observed death is used.

- min_sample_size:

  The minimum number of individuals alive at the beggining of a time
  interval for computing the observed event rate.

- groupby:

  One or multiple factors used to define a group for which the event
  rate the will be computed. If NULL, calculates a single overall rate.
  If `"all"`, calculates rate over each combination of all factors
  Otherwise must be a character or character vector with factors names.

## Value

A dataframe with 3 columns: Interval (time interval, based on
`interval_width` value), group (identifier of a given subgroup, or
"Overall" if groupby = NULL), and Event_rate (event rate at this time).

## Examples

``` r
df <- datapierrick |>
as_tibble() |>
 mutate(par = as.factor(par))
 
# name of the columns of the clutchs into a single vector
generate_clutch_vector <- function(N) {
 return(paste(
   "clutch",
   rep(c("start", "end", "size"), N),
   rep(1:N, each = 3),
   sep = "_"
 ))
}
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
 dist = c("wei", "gam", "lgn")
)
observed_emergence_rate <- compute_observed_event_rate(
 lifelihoodData = dataLFH,
 interval_width = 5,
 event = c("mortality"),
 min_sample_size = 1,
 max_time=150,
 groupby=c("par"))
p <- observed_emergence_rate |>
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
 ylab("Observed mortality rate over 5 day-periods")+
 facet_wrap(vars(par))
p
#> Warning: Removed 30 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 30 rows containing missing values or values outside the scale range
#> (`geom_line()`).

```

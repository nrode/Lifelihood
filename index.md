<br><br>

## Description

`lifelihood` is a class of continuous time multi-event models which provide the joined likelihood of all the events in an individual life-history (time of maturity, reproductive events, death).

<br>

## Quick start

```r
library(lifelihood)

df <- read.csv(here::here("data_internals/fake_sample.csv"))
head(df)
df$type <- as.factor(df$type)
df$geno <- as.factor(df$geno)

clutchs <- c(
  "clutch_start1", "clutch_end1", "clutch_size1",
  "clutch_start2", "clutch_end2", "clutch_size2"
)

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
  covariates = c("geno", "type"),
  model_specs = c("gam", "lgn", "wei")
)

results <- lifelihood(
  lifelihoodData = dataLFH,
  path_config = system.file("configs/config.yaml", package = "lifelihood"),
  seeds = c(1, 2, 3, 4)
)
summary(results)
```

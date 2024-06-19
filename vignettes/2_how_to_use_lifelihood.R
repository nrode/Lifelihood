rm(list=ls())
library(lifelihood)

df <- read.csv(here::here("data/fake_sample.csv"))

clutchs <- c(
   "clutch_start1", "clutch_end1", "clutch_size1",
   "clutch_start2", "clutch_end2", "clutch_size2",
   "clutch_start3", "clutch_end3", "clutch_size3"
)

results <- lifelihood(
   df = df,
   sex = "sex",
   sex_start = "sex_start",
   sex_end = "sex_end",
   maturity_start = "mat_start",
   maturity_end = "mat_end",
   clutchs = clutchs,
   death_start = "mor_start",
   death_end = "mor_end",
   extra1 = "geno"
)
summary(results)
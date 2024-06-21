rm(list=ls())
devtools::load_all() # load the package

df <- read.csv(here::here("data/fake_sample.csv"))

clutchs <- c(
   "clutch_start1", "clutch_end1", "clutch_size1",
   "clutch_start2", "clutch_end2", "clutch_size2"
)

param_ranges <- data.frame(
   param = c(
      "E(tmort)f", "morta", "RE(tmort)m", "mortp", "propmal", "E(tmat)f", "mata",
      "RE(tmat)m", "E(tpon)", "ponta", "pontn", "to(ps)int", "to(ps)am", "to(ps)tp",
      "sen(pu)t", "sen(pu)t2", "sen(pn)t", "sen(pn)t2", "to(pupn)", "W"
   ),
   min = c(
      1, 0.001, 0.1, 0.0001, 0.00001, 1, 0.0001, 0.1, 0.1, 0.001, 1, 0.00001,
      0.0000001, 0.0000001, -20, -20, -8, -8, -8, 0.001
   ),
   max = c(302, 20, 4, 1, 0.99999, 100, 15, 10, 200, 12, 50, 10, 10, 10, 20, 20, 10, 10, 10, 1200)
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
   extra1 = "geno",
   matclutch = FALSE,
   param_range_df = param_ranges,
   seeds = c(1, 2, 3, 4),
   models = c("gam", "lgn", "wei")
)
#summary(results)

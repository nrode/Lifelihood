rm(list = ls())
devtools::load_all() # load the package

source('dev/compile.R')
compile()

df <- read.csv(here::here("data/fake_sample.csv"))

clutchs <- c(
   "clutch_start1", "clutch_end1", "clutch_size1",
   "clutch_start2", "clutch_end2", "clutch_size2"
)

results <- lifelihood(
   df = df,
   path_config = "config.yaml",
   sex = "sex",
   sex_start = "sex_start",
   sex_end = "sex_end",
   maturity_start = "mat_start",
   maturity_end = "mat_end",
   clutchs = clutchs,
   death_start = "mor_start",
   death_end = "mor_end",
   covariates = c("geno", "type"),
   matclutch = FALSE,
   seeds = c(1, 2, 3, 4),
   models = c("gam", "lgn", "wei")
)
summary(results)
results$likelihood
results$effects

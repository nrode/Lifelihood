rm(list = ls())
devtools::load_all() # load the package
df <- read.csv(here::here("data/fake_re_sample.csv"))
df$type <- as.factor(df$type)
df$geno <- as.factor(df$geno)

clutchs <- c(
   "clutch_start1", "clutch_end1", "clutch_size1",
   "clutch_start2", "clutch_end2", "clutch_size2"
)
results <- lifelihood(
   df = df,
   path_config = "config2.yaml",
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
   seeds = c(11, 22, 34, 44),
   model_specs = c("lgn", "lgn", "exp"),
   delete_temp_files = FALSE,
   raise_estimation_warning = TRUE
)
results$effects
results$parameter_ranges
summary(results)
head(results$effects)

plot_mortality_rate(
   results_lifelihood = results,
   newdata = df,
   covariates = c("geno", "type"),
   intervals = seq(0,20,5),
   use_log_x = TRUE,
   use_log_y = TRUE
)

stats::predict.glm

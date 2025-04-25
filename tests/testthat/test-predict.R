library(lifelihood)

df <- fakesample
df$type <- as.factor(df$type)
df$geno <- as.factor(df$geno)

clutchs <- c(
  "clutch_start1",
  "clutch_end1",
  "clutch_size1",
  "clutch_start2",
  "clutch_end2",
  "clutch_size2"
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
  path_config = system.file("configs/config2.yaml", package = "lifelihood"),
  seeds = c(1, 2, 3, 4),
  raise_estimation_warning = FALSE
)


test_that("predict works with expt_death", {
  preds <- predict(results, "expt_death", type = "response")
  expect_true(all(preds >= 0))
})

test_that("predict raises an error with invalid type argument", {
  expect_error(predict(results, "expt_death", type = "invalid input"))
})

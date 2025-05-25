library(lifelihood)

df <- fakesample |>
  mutate(
    geno = as.factor(geno),
    type = as.factor(type)
  )

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
  path_config = get_config_path("config2"),
  seeds = c(1, 2, 3, 4),
  raise_estimation_warning = FALSE
)


test_that("prediction works with expt_death", {
  preds <- prediction(results, "expt_death", type = "response")
  expect_true(all(preds >= 0))
})

test_that("prediction raises an error with invalid type argument", {
  expect_error(prediction(results, "expt_death", type = "invalid input"))
})

test_that("prediction raises an error with invalid object argument", {
  invalid_input <- c(1, 2, 3)
  expect_error(
    prediction(invalid_input, "expt_death"),
    regexp = "`prediction` function expect a 'lifelihoodResults' object, not:"
  )
})

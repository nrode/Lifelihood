testthat::test_that("predictions work", {
  path_config <- if (rlang::is_interactive()) {
    "tests/testthat/config.yaml"
  } else {
    "config.yaml"
  }

  df <- datapierrick |>
    as_tibble() |>
    mutate(
      par = as.factor(par),
      geno = as.factor(geno),
      spore = as.factor(spore)
    )

  clutchs <- generate_clutch_vector(28)

  lifelihoodData <- as_lifelihoodData(
    df = df,
    sex = "sex",
    sex_start = "sex_start",
    sex_end = "sex_end",
    maturity_start = "mat_start",
    maturity_end = "mat_end",
    clutchs = clutchs,
    death_start = "death_start",
    death_end = "death_end",
    covariates = c("par", "spore"),
    model_specs = c("wei", "gam", "exp")
  )

  results <- lifelihood(
    lifelihoodData,
    path_config = path_config,
    raise_estimation_warning = FALSE
  )

  pred <- prediction(
    results,
    parameter_name = "expt_death"
  )
  pred_resp <- prediction(
    results,
    parameter_name = "expt_death",
    type = "response"
  )
  expect_true(length(pred) == 550)
  expect_true(length(pred_resp) == 550)
  expect_true(class(pred) == "numeric")
  expect_true(class(pred_resp) == "numeric")
  expect_true(all(pred <= pred_resp))
})

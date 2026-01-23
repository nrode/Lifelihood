testthat::test_that("as_lifelihoodData works", {
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

  expect_true(!is.null(lifelihoodData$df))
  expect_true(lifelihoodData$sex == "sex")
  expect_true(lifelihoodData$sex_start == "sex_start")
  expect_true(lifelihoodData$sex_end == "sex_end")
  expect_true(lifelihoodData$maturity_start == "mat_start")
  expect_true(lifelihoodData$maturity_end == "mat_end")
  expect_true(all(lifelihoodData$clutchs == clutchs))
  expect_true(lifelihoodData$death_start == "death_start")
  expect_true(lifelihoodData$death_end == "death_end")
  expect_true(all(lifelihoodData$model_specs == c("wei", "gam", "exp")))
  expect_true(all(lifelihoodData$covariates == c("par", "spore")))
  expect_true(lifelihoodData$matclutch == FALSE)
  expect_true(lifelihoodData$right_censoring_date == 1000)
  expect_true(lifelihoodData$critical_age == 20)
  expect_true(lifelihoodData$ratiomax == 10)
  expect_null(lifelihoodData$block)
})

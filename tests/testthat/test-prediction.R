test_that("predictions work", {
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

test_that("Prediction with ratio expt death", {
  df_female <- datadaphnia |>
    as_tibble() |>
    mutate(
      par = as.factor(par),
      geno = as.factor(geno),
      spore = as.factor(spore)
    )

  df_male <- df_female |>
    mutate(
      sex = 1,
      across(starts_with("clutch"), ~NA_real_)
    )
  df <- rbind(df_female, df_male)

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
    model_specs = c("wei", "gam", "lgn")
  )

  results <- lifelihood(
    lifelihoodData = lifelihoodData,
    path_config = use_test_config("config_pierrick"),
    se.fit = TRUE
  )

  preds <- prediction(
    results,
    "ratio_expt_death",
    type = "response",
    se.fit = TRUE
  )

  expect_true(all(is.na(head(preds$fitted))))
  expect_true(!all(is.na(tail(preds$fitted))))
  expect_true(all(is.na(head(preds$se.fitted))))
  expect_true(!all(is.na(tail(preds$se.fitted))))
})

test_that("prediction reports fitted factor covariates with one level", {
  df <- data.frame(
    sex = c(0, 0, 0),
    par = factor(c("only", "only", "only"))
  )

  results <- list(
    lifelihoodData = list(
      df = df,
      sex = "sex"
    ),
    formula = list(expt_death = "par"),
    config = list(
      mortality = list(expt_death = "par")
    ),
    MCMC = 0,
    se.fit = FALSE
  )
  class(results) <- "lifelihoodResults"

  expect_error(
    prediction(results, "expt_death"),
    "fitted factor covariate `par` has fewer than two levels"
  )
})

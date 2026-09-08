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
    matclutch = FALSE,
    sex = "sex",
    sex_start = "sex_start",
    sex_end = "sex_end",
    maturity_start = "mat_start",
    maturity_end = "mat_end",
    clutchs = clutchs,
    death_start = "death_start",
    death_end = "death_end",
    covariates = c("par", "spore"),
    dist = c(mortality = "wei", maturity = "gam", reproduction = "exp")
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

  pred_multi <- prediction(
    results,
    parameter_name = c("expt_death", "survival_param2"),
    type = "response"
  )
  expect_named(pred_multi, c("expt_death", "survival_param2"))
  expect_equal(pred_multi$expt_death, pred_resp)
  expect_equal(
    pred_multi$survival_param2,
    prediction(results, "survival_param2", type = "response")
  )
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
    matclutch = FALSE,
    sex = "sex",
    sex_start = "sex_start",
    sex_end = "sex_end",
    maturity_start = "mat_start",
    maturity_end = "mat_end",
    clutchs = clutchs,
    death_start = "death_start",
    death_end = "death_end",
    covariates = c("par", "spore"),
    dist = c(mortality = "wei", maturity = "gam", reproduction = "lgn")
  )

  results <- lifelihood(
    lifelihoodData = lifelihoodData,
    path_config = use_test_config("config_pierrick"),
    seeds = c(9614, 1017, 8004, 4775),
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

  preds_multi <- prediction(
    results,
    c("expt_death", "survival_param2"),
    type = "response",
    se.fit = TRUE
  )
  expect_named(
    preds_multi,
    c(
      "expt_death_fitted",
      "expt_death_se.fitted",
      "survival_param2_fitted",
      "survival_param2_se.fitted"
    )
  )
  expt_death_preds <- prediction(
    results,
    "expt_death",
    type = "response",
    se.fit = TRUE
  )
  expect_equal(preds_multi$expt_death_fitted, expt_death_preds$fitted)
  expect_equal(preds_multi$expt_death_se.fitted, expt_death_preds$se.fitted)
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

test_that("prediction warns when keeping MCMC samples without MCMC prediction", {
  df <- data.frame(sex = c(0, 1))
  results <- list(
    lifelihoodData = list(
      df = df,
      sex = "sex"
    ),
    formula = list(expt_death = "1"),
    config = list(
      mortality = list(expt_death = "1")
    ),
    effects = data.frame(
      parameter = "expt_death",
      estimation = 1
    ),
    MCMC = 0,
    se.fit = FALSE
  )
  class(results) <- "lifelihoodResults"

  expect_warning(
    prediction(
      results,
      "expt_death",
      keep_mcmc_samples = TRUE,
      .warning_ratio_male = FALSE
    ),
    "`keep_mcmc_samples = TRUE` has no effect when `mcmc.fit = FALSE`",
    fixed = TRUE
  )
})

test_that("prediction reports link-scale male message only with males", {
  results <- list(
    lifelihoodData = list(
      df = data.frame(sex = c(0, 0), par = c(0, 1)),
      sex = "sex"
    ),
    formula = list(expt_death = "par"),
    config = list(
      mortality = list(expt_death = "par")
    ),
    effects = data.frame(
      parameter = c("expt_death", "expt_death"),
      estimation = c(1, 0.2)
    ),
    MCMC = 200,
    se.fit = FALSE,
    mcmc_sample = data.frame(
      int_expt_death = seq(0.5, 1.5, length.out = 200),
      eff_expt_death_par = sin(seq(0, 4 * pi, length.out = 200))
    )
  )
  class(results) <- "lifelihoodResults"

  expect_message(
    prediction(results, "expt_death"),
    NA
  )
  expect_message(
    prediction(
      results,
      "expt_death",
      mcmc.fit = TRUE,
      keep_mcmc_samples = TRUE
    ),
    NA
  )

  results$lifelihoodData$df <- data.frame(sex = c(0, 1), par = c(0, 1))

  expect_message(
    prediction(results, "expt_death"),
    "Lifelihood parameter estimate(s) for males are identical to that of females.",
    fixed = TRUE
  )
  expect_message(
    prediction(
      results,
      "expt_death",
      mcmc.fit = TRUE,
      keep_mcmc_samples = TRUE
    ),
    "Lifelihood parameter estimate(s) for males are identical to that of females.",
    fixed = TRUE
  )
})

test_that("prediction combines MCMC predictions for multiple parameters", {
  results <- list(
    lifelihoodData = list(
      df = data.frame(sex = c(0, 1)),
      sex = "sex"
    ),
    formula = list(
      expt_death = "1",
      survival_param2 = "1"
    ),
    config = list(
      mortality = list(
        expt_death = "1",
        survival_param2 = "1"
      )
    ),
    effects = data.frame(
      parameter = c("expt_death", "survival_param2"),
      estimation = c(1, 0.2)
    ),
    MCMC = 200,
    se.fit = FALSE,
    mcmc_sample = data.frame(
      int_expt_death = seq(0.5, 1.5, length.out = 200),
      int_survival_param2 = seq(0.1, 0.3, length.out = 200)
    )
  )
  class(results) <- "lifelihoodResults"

  predictions <- prediction(
    results,
    c("expt_death", "survival_param2"),
    newdata = data.frame(sex = 0),
    mcmc.fit = TRUE,
    keep_mcmc_samples = TRUE
  )

  expect_named(
    predictions$pred,
    c(
      "expt_death_fitted",
      "expt_death_mcmc_est",
      "expt_death_mcmc_se",
      "survival_param2_fitted",
      "survival_param2_mcmc_est",
      "survival_param2_mcmc_se"
    )
  )
  expect_named(
    predictions$mcmc_samples,
    c(
      paste0("expt_death_mcmc_sample_", seq_len(200)),
      paste0("survival_param2_mcmc_sample_", seq_len(200))
    )
  )
})

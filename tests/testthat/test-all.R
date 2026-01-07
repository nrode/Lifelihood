testthat::test_that("Overall demo works", {
  df <- datapierrick |>
    as_tibble() |>
    mutate(
      par = as.factor(par),
      geno = as.factor(geno),
      spore = as.factor(spore)
    )

  generate_clutch_vector <- function(N) {
    return(paste(
      "pon",
      rep(c("start", "end", "size"), N),
      rep(1:N, each = 3),
      sep = "_"
    ))
  }
  clutchs <- generate_clutch_vector(28)

  lifelihoodData <- lifelihoodData(
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

  results <- lifelihood(lifelihoodData, "config.yaml")

  expect_true(results$config |> length() == 3)
  expect_true(results$config$mortality |> length() == 5)
  expect_true(results$config$maturity |> length() == 3)
  expect_true(results$config$reproduction |> length() == 12)
  expect_true(is.numeric(results$likelihood))
  expect_true(ncol(results$parameter_ranges) == 3)
  expect_true(results$sample_size == 550)
  expect_true(length(results$seeds) == 4)

  df_effects <- results$effects

  expect_s3_class(df_effects, "data.frame")

  expect_named(
    df_effects,
    c("name", "estimation", "stderror", "parameter", "kind", "event")
  )

  expect_type(df_effects$name, "character")
  expect_type(df_effects$parameter, "character")
  expect_type(df_effects$kind, "character")
  expect_type(df_effects$event, "character")
  expect_type(df_effects$estimation, "double")
  expect_type(df_effects$stderror, "double")

  expect_setequal(
    unique(df_effects$kind),
    c("intercept", "coefficient_category")
  )
  expect_setequal(
    unique(df_effects$event),
    c("mortality", "maturity", "reproduction")
  )

  expect_true(length(coef(results)) == 16)
  expect_true(all(grepl("^(int_|eff_)", names(coef(results)))))
  expect_true(length(coeff(results, "expt_death")) == 6)
  expect_true(length(coeff(results, "survival_param2")) == 1)
  expect_true(length(coeff(results, "n_offspring")) == 1)
  expect_true(is.numeric(AIC(results)))
  expect_true(is.numeric(BIC(results)))
  expect_true(is.numeric(logLik(results)))

  check_compute_event_rate <- function(event) {
    df_rate <- compute_fitted_event_rate(
      results,
      interval_width = 15,
      event = event
    )
    expect_s3_class(df_rate, "data.frame")
    expect_type(df_rate$time, "double")
    expect_type(df_rate$par, "integer")
    expect_type(df_rate$Interval_start, "double")
    expect_type(df_rate$Interval_end, "double")
    expect_type(df_rate$Mean_Interval, "double")
    expect_type(df_rate$Event_Rate, "double")
    expect_type(df_rate$group, "integer")
    expect_true(all(df_rate$group == "Overall"))
  }
  check_compute_event_rate("maturity")
  check_compute_event_rate("mortality")
  check_compute_event_rate("reproduction")

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

  suppressWarnings({
    simul <- simulate_life_history(results)
    expect_true(ncol(simul) >= 100)
    expect_type(simul$mortality, "double")
    expect_type(simul$maturity, "double")
    expect_type(simul$clutch_1, "double")
    expect_type(simul$n_offspring_clutch_1, "integer")

    simul <- simulate_life_history(results, event = "mortality")
    expect_type(simul$mortality, "double")
    expect_true(nrow(simul) == 550)

    simul <- simulate_life_history(results, event = "maturity")
    expect_type(simul$maturity, "double")
    expect_true(nrow(simul) == 550)
  })

  for (event in c("maturity", "mortality", "reproduction")) {
    p <- plot_fitted_event_rate(
      results,
      interval_width = 10,
      event = event,
      xlab = "Age (days)",
      ylab = "Fitted Maturity Rate"
    )
    expect_true(
      all(
        class(p) ==
          c("ggplot2::ggplot", "ggplot", "ggplot2::gg", "S7_object", "gg")
      )
    )
  }

  for (event in c("maturity", "mortality", "reproduction")) {
    p <- plot_observed_event_rate(
      lifelihoodData,
      interval_width = 10,
      groupby = "par",
      event = "reproduction",
      use_facet = TRUE,
      xlab = "Age (days)",
      ylab = "Observed Mortality Rate"
    )
    expect_true(
      all(
        class(p) ==
          c("ggplot2::ggplot", "ggplot", "ggplot2::gg", "S7_object", "gg")
      )
    )
  }
})

testthat::test_that("visualization works", {
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

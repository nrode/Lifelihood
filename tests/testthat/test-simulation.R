testthat::test_that("simulations work", {
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
})

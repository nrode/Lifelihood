test_that("simulations work", {
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

test_that("trade-off simulations work for reproduction events", {
  path_config <- if (rlang::is_interactive()) {
    "tests/testthat/config_tradeoff.yaml"
  } else {
    "config_tradeoff.yaml"
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
    covariates = c("par", "geno"),
    model_specs = c("wei", "gam", "lgn")
  )

  results <- lifelihood(
    lifelihoodData,
    path_config = path_config,
    raise_estimation_warning = FALSE
  )

  sim_reproduction <- simulate_life_history(
    results,
    event = "reproduction",
    seed = 1
  )
  expect_true(all(c("mortality", "maturity") %in% names(sim_reproduction)))
  clutch_cols <- grep("^clutch_", names(sim_reproduction), value = TRUE)
  n_offspring_cols <- grep(
    "^n_offspring_clutch_",
    names(sim_reproduction),
    value = TRUE
  )
  expect_true(length(clutch_cols) > 0)
  expect_true(length(n_offspring_cols) > 0)
  expect_type(sim_reproduction[[n_offspring_cols[1]]], "integer")

  sim_mortality <- simulate_life_history(results, event = "mortality", seed = 1)
  expect_true(identical(names(sim_mortality), "mortality"))
  expect_true(nrow(sim_mortality) == nrow(df))
})

test_that("censoring works for reproduction and validates block in newdata", {
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
    model_specs = c("wei", "gam", "exp"),
    block = "geno"
  )

  results <- lifelihood(
    lifelihoodData,
    path_config = path_config,
    raise_estimation_warning = FALSE
  )

  sim <- simulate_life_history(
    results,
    event = "reproduction",
    use_censoring = TRUE,
    seed = 1
  )
  expect_true(all(c(
    "maturity_start",
    "maturity_end",
    "mortality_start",
    "mortality_end"
  ) %in% names(sim)))

  newdata_without_block <- df[1:5, c("par", "spore")]
  expect_error(
    simulate_life_history(
      results,
      event = "mortality",
      newdata = newdata_without_block,
      use_censoring = TRUE
    ),
    "requires a `geno` column"
  )
})

test_that("truncated Poisson simulation handles invalid means", {
  expect_true(is.na(simulate_truncPois_single(0)))
  expect_true(is.na(simulate_truncPois_single(NA_real_)))

  draws <- simulate_truncPois(c(0, 0.5, NA_real_), n = 5)
  expect_true(all(is.na(draws[, 1])))
  expect_true(all(draws[, 2] >= 1L))
  expect_true(all(is.na(draws[, 3])))
})

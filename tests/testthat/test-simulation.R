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
    dist = c("wei", "gam", "exp")
  )

  results <- lifelihood(
    lifelihoodData,
    path_config = path_config,
    raise_estimation_warning = FALSE
  )

  suppressWarnings({
    simul <- simulate_life_history(results)
    expect_true(ncol(simul) >= 100)
    expect_type(simul$death_start, "double")
    expect_type(simul$death_end, "double")
    expect_type(simul$maturity_start, "double")
    expect_type(simul$maturity_end, "double")
    expect_type(simul$clutch_start_1, "double")
    expect_type(simul$clutch_end_1, "double")
    expect_type(simul$clutch_size_1, "integer")

    simul <- simulate_life_history(results, event = "mortality")
    expect_type(simul$death_start, "double")
    expect_type(simul$death_end, "double")
    expect_true(nrow(simul) == 550)

    simul <- simulate_life_history(results, event = "maturity")
    expect_type(simul$maturity_start, "double")
    expect_type(simul$maturity_end, "double")
    expect_true(nrow(simul) == 550)
  })
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
    dist = c("wei", "gam", "exp"),
    block = "geno",
    matclutch = FALSE
  )

  results <- lifelihood(
    lifelihoodData,
    path_config = path_config,
    raise_estimation_warning = FALSE
  )

  visits <- get_visits(lifelihoodData)
  expect_true(all(c("geno", "visit") %in% names(visits)))

  expect_error(
    simulate_life_history(
      results,
      event = "maturity",
      use_censoring = TRUE,
      seed = 1
    ),
    "`visits` cannot be NULL"
  )

  sim <- simulate_life_history(
    results,
    event = "reproduction",
    use_censoring = TRUE,
    visits = visits,
    seed = 1
  )
  expect_true(all(
    c(
      "maturity_start",
      "maturity_end",
      "mortality_start",
      "mortality_end",
      "clutch_1",
      "clutch_start_1",
      "clutch_end_1",
      "clutch_size_1"
    ) %in%
      names(sim)
  ))
  size_cols <- grep("^clutch_size_[0-9]+$", names(sim), value = TRUE)
  no_repro_data <- rowSums(!is.na(sim[size_cols])) == 0
  # Individuals with no clutch-size data at all (e.g. males) must stay NA so
  # "no reproduction data" is distinct from a genuine zero-offspring female.
  expect_true(all(is.na(sim$total_n_offspring[no_repro_data])))
  expect_equal(
    sim$total_n_offspring[!no_repro_data],
    rowSums(sim[size_cols], na.rm = TRUE)[!no_repro_data]
  )

  newdata_without_block <- df[1:5, c("par", "spore")]
  expect_error(
    simulate_life_history(
      results,
      event = "mortality",
      newdata = newdata_without_block,
      use_censoring = TRUE,
      visits = visits
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

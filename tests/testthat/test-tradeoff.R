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
    matclutch = FALSE,
    sex = "sex",
    sex_start = "sex_start",
    sex_end = "sex_end",
    maturity_start = "mat_start",
    maturity_end = "mat_end",
    clutchs = clutchs,
    death_start = "death_start",
    death_end = "death_end",
    covariates = c("par", "geno"),
    dist = c("wei", "gam", "lgn"),
    block = "geno"
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
  expect_true(all(
    c("mortality_start", "mortality_end", "maturity_start", "maturity_end") %in%
      names(sim_reproduction)
  ))
  clutch_cols <- grep("^clutch_", names(sim_reproduction), value = TRUE)
  n_offspring_cols <- grep(
    "^clutch_size_",
    names(sim_reproduction),
    value = TRUE
  )
  expect_true(length(clutch_cols) > 0)
  expect_true(length(n_offspring_cols) > 0)
  expect_type(sim_reproduction[[n_offspring_cols[1]]], "integer")
  expect_equal(nrow(sim_reproduction), nrow(df))

  sim_reproduction_censored <- simulate_life_history(
    results,
    event = "reproduction",
    use_censoring = TRUE,
    visits = get_visits(lifelihoodData),
    seed = 1
  )
  expect_true(all(
    c("clutch_start_1", "clutch_end_1", "clutch_size_1") %in%
      names(sim_reproduction_censored)
  ))
  expect_false(any(grepl("^clutch_[0-9]+$", names(sim_reproduction_censored))))
  expect_equal(nrow(sim_reproduction_censored), nrow(df))

  sim_reproduction_censored_exact <- simulate_life_history(
    results,
    event = "reproduction",
    use_censoring = TRUE,
    remove_exact_clutch_dates = FALSE,
    visits = get_visits(lifelihoodData),
    seed = 1
  )
  expect_true(any(grepl(
    "^exact_clutch_date_[0-9]+$",
    names(sim_reproduction_censored_exact)
  )))
  expect_false(any(grepl(
    "^clutch_[0-9]+$",
    names(sim_reproduction_censored_exact)
  )))

  maturity_observed <- sim_reproduction$maturity_start <
    sim_reproduction$mortality_start
  clutch_cols <- grep(
    "^clutch_start_[0-9]+$",
    names(sim_reproduction),
    value = TRUE
  )
  for (clutch_col in clutch_cols) {
    clutch <- sim_reproduction[[clutch_col]]
    has_clutch <- !is.na(clutch)
    expect_true(all(
      maturity_observed[has_clutch] &
        clutch[has_clutch] > sim_reproduction$maturity_start[has_clutch] &
        clutch[has_clutch] < sim_reproduction$mortality_start[has_clutch]
    ))
  }

  sim_mortality <- simulate_life_history(results, event = "mortality", seed = 1)
  expect_identical(
    sort(names(sim_mortality)),
    sort(c(
      "par",
      "geno",
      "sex",
      "sex_start",
      "sex_end",
      "mortality_start",
      "mortality_end",
      "total_n_clutches",
      "total_n_offspring"
    ))
  )
  expect_true(nrow(sim_mortality) == nrow(df))
})

path_config <- if (rlang::is_interactive()) {
  "tests/testthat/config_gbg.yaml"
} else {
  "config_gbg.yaml"
}

test_that("validate_group_by_group_config rejects mixed formulas", {
  config <- list(
    mortality = list(
      expt_death = "par + spore",
      survival_param2 = "1",
      ratio_expt_death = "not_fitted",
      prob_death = "not_fitted",
      sex_ratio = "not_fitted"
    ),
    maturity = list(
      expt_maturity = "par",
      maturity_param2 = "1",
      ratio_expt_maturity = "not_fitted"
    ),
    reproduction = list(
      expt_reproduction = "par",
      reproduction_param2 = "1",
      n_offspring = "1",
      increase_death_hazard = "not_fitted",
      tof_reduction_rate = "not_fitted",
      increase_tof_n_offspring = "not_fitted",
      lin_decrease_hazard = "not_fitted",
      quad_decrease_hazard = "not_fitted",
      lin_change_n_offspring = "not_fitted",
      quad_change_n_offspring = "not_fitted",
      tof_n_offspring = "not_fitted",
      fitness = "not_fitted"
    )
  )

  expect_error(
    validate_group_by_group_config(config),
    "`group_by_group` argument requires the same covariate"
  )
})

test_that("extract_group_covariates parses formulas correctly", {
  expect_equal(extract_group_covariates("par"), "par")
  expect_equal(
    sort(extract_group_covariates("par + geno")),
    c("geno", "par")
  )
  expect_equal(
    sort(extract_group_covariates("par * geno")),
    c("geno", "par")
  )
  expect_equal(
    sort(extract_group_covariates("par + geno + par:geno")),
    c("geno", "par")
  )
  expect_equal(
    sort(extract_group_covariates("par + geno + spore")),
    c("geno", "par", "spore")
  )
})

test_that("split_data_by_groups creates correct sub-datasets", {
  df <- data.frame(
    x = 1:10,
    grp = factor(rep(c("A", "B"), each = 5)),
    sex = 1,
    sex_start = 0,
    sex_end = 1,
    mat_start = 5,
    mat_end = 10,
    death_start = 20,
    death_end = 30
  )

  mock_data <- list(
    df = df,
    covariates = c("grp"),
    sex = "sex",
    sex_start = "sex_start",
    sex_end = "sex_end",
    maturity_start = "mat_start",
    maturity_end = "mat_end",
    clutchs = c(),
    death_start = "death_start",
    death_end = "death_end",
    model_specs = c("wei", "gam", "exp"),
    block = NULL,
    matclutch = FALSE,
    matclutch_size = NULL,
    right_censoring_date = 1000,
    critical_age = 20,
    ratiomax = 10
  )
  class(mock_data) <- "lifelihoodData"

  sub <- split_data_by_groups(mock_data, "grp")

  expect_equal(length(sub), 2)
  expect_true("A" %in% names(sub))
  expect_true("B" %in% names(sub))
  expect_equal(nrow(sub[["A"]]$df), 5)
  expect_equal(nrow(sub[["B"]]$df), 5)
  expect_equal(sub[["A"]]$covariates, c("grp"))
  expect_equal(sub[["B"]]$covariates, c("grp"))
})

# --- Integration test ---

test_that("lifelihood with group_by_group=TRUE and n_fit > 1 works end-to-end", {
  df <- datapierrick |>
    as_tibble() |>
    mutate(
      par = as.factor(par),
      geno = as.factor(geno),
      spore = as.factor(spore)
    ) |>
    mutate(
      par = factor(par, levels = c(0, 1, 2), labels = c("par1", "par2", "par3"))
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
    covariates = c("par"),
    model_specs = c("wei", "gam", "exp")
  )

  results <- suppressWarnings(
    lifelihood(
      lifelihoodData,
      path_config = path_config,
      group_by_group = TRUE,
      n_fit = 4,
      delete_temp_files = TRUE
    )
  )

  # Check class
  expect_s3_class(results, "lifelihoodResults")

  # Check group_by_group flag
  expect_true(results$group_by_group)

  # Check likelihood
  expect_true(is.numeric(results$likelihood))

  # Check coef() returns named vector
  co <- coef(results)
  expect_true(is.numeric(co))
  expect_true(length(co) > 0)
  expect_true(all(nchar(names(co)) > 0))

  # Check AIC, BIC, logLik work
  expect_true(is.numeric(AIC(results)))
  expect_true(is.numeric(BIC(results)))
  expect_true(is.numeric(logLik(results)))

  # Check summary runs without error
  expect_no_error(summary(results))

  # Check prediction gives informative error
  expect_error(
    prediction(results, "expt_death"),
    "prediction\\(\\) is not supported for group_by_group results"
  )

  # Check simulate_life_history gives informative error
  expect_error(
    simulate_life_history(results),
    "simulate_life_history\\(\\) is not supported for group_by_group results"
  )

  # Check compute_fitted_event_rate gives informative error
  expect_error(
    compute_fitted_event_rate(results, interval_width = 10),
    "compute_fitted_event_rate\\(\\) is not supported for group_by_group results"
  )

  # Check group metadata
  expect_true(length(results$group_names) > 0)
  expect_true(length(results$group_likelihoods) == length(results$group_names))
})

test_that("lifelihood rejects seeds when group_by_group=TRUE and n_fit > 1", {
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
    covariates = c("par"),
    model_specs = c("wei", "gam", "exp")
  )

  expect_error(
    lifelihood(
      lifelihoodData,
      path_config = path_config,
      group_by_group = TRUE,
      n_fit = 2,
      seeds = c(1, 2, 3, 4),
      delete_temp_files = TRUE
    ),
    "Can't set `seeds` with `n_fit` > 1."
  )
})

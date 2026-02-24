path_config <- if (rlang::is_interactive()) {
  "tests/testthat/config.yaml"
} else {
  "config.yaml"
}

test_that("goodness_of_fit returns expected object structure", {
  df <- datapierrick |>
    as_tibble() |>
    mutate(
      par = as.factor(par),
      geno = as.factor(geno),
      spore = as.factor(spore)
    ) |>
    sample_n(120)

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

  gof <- goodness_of_fit(
    results,
    nsim = 1,
    seed = 1,
    show_progress = FALSE
  )

  expect_s3_class(gof, "lifelihoodGOF")
  expect_true(is.numeric(gof$original_loglik))
  expect_length(gof$simulated_loglik, 1)
  expect_length(gof$errors, 1)
  expect_equal(gof$n_success + gof$n_failed, 1)
  expect_true(is.numeric(gof$p_lower_or_equal))
  expect_length(gof$fits, 1)
})

test_that("goodness_of_fit input validation works", {
  fake_gbg <- structure(
    list(group_by_group = TRUE),
    class = "lifelihoodResults"
  )
  expect_error(
    goodness_of_fit(fake_gbg, nsim = 1, show_progress = FALSE),
    "group_by_group"
  )

  fake_fit <- structure(
    list(
      group_by_group = FALSE,
      lifelihoodData = list(matclutch = FALSE),
      effects = data.frame(
        parameter = c("expt_death", "survival_param2"),
        stringsAsFactors = FALSE
      ),
      config = list(),
      param_bounds_df = data.frame(),
      likelihood = -1
    ),
    class = "lifelihoodResults"
  )

  expect_error(
    goodness_of_fit(fake_fit, nsim = 0, show_progress = FALSE),
    "positive integer"
  )
  expect_error(
    goodness_of_fit(
      fake_fit,
      nsim = 1,
      fit_args = list(1),
      show_progress = FALSE
    ),
    "named list"
  )
})

test_that("plot.lifelihoodGOF works and handles empty successful fits", {
  gof_ok <- structure(
    list(
      original_loglik = -100,
      simulated_loglik = c(-110, -105, -95),
      nsim = 3,
      n_success = 3,
      n_failed = 0,
      p_lower_or_equal = 2 / 3,
      errors = c(NA, NA, NA),
      fits = vector("list", 3)
    ),
    class = "lifelihoodGOF"
  )
  p <- plot(gof_ok)
  expect_s3_class(p, "ggplot")

  gof_empty <- structure(
    list(
      original_loglik = -100,
      simulated_loglik = c(NA_real_, NA_real_),
      nsim = 2,
      n_success = 0,
      n_failed = 2,
      p_lower_or_equal = NA_real_,
      errors = c("err", "err"),
      fits = vector("list", 2)
    ),
    class = "lifelihoodGOF"
  )
  expect_error(
    plot(gof_empty),
    "No successful simulation fit available to plot"
  )
})

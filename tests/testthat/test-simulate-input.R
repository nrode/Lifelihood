simulation_input_config <- function() {
  list(
    mortality = list(
      expt_death = "par + spore",
      survival_param2 = 1,
      ratio_expt_death = "not_fitted",
      prob_death = "not_fitted",
      sex_ratio = "not_fitted"
    ),
    maturity = list(
      expt_maturity = "par",
      maturity_param2 = 1,
      ratio_expt_maturity = "not_fitted"
    ),
    reproduction = list(
      expt_reproduction = "par",
      reproduction_param2 = 1,
      n_offspring = 1,
      increase_death_hazard = "not_fitted",
      tof_decay = "not_fitted",
      increase_death_hazard_n_offspring = "not_fitted",
      lin_decrease_hazard = "not_fitted",
      quad_decrease_hazard = "not_fitted",
      lin_change_n_offspring = "not_fitted",
      quad_change_n_offspring = "not_fitted",
      tof_n_offspring = "not_fitted",
      fitness = "not_fitted"
    )
  )
}

simulation_input_effects <- function() {
  list(
    expt_death = list(
      par = c(0, -0.4, 0.2),
      spore = c(0, -0.2, 0.1, 0.3)
    ),
    survival_param2 = list(0),
    expt_maturity = list(par = c(0, 0.15, -0.1)),
    maturity_param2 = list(0),
    expt_reproduction = list(par = c(0, -0.3, 0.25)),
    reproduction_param2 = list(0),
    n_offspring = list(0)
  )
}

simulation_input_data <- function(n = 1000) {
  data.frame(
    par = factor(rep(0:2, length.out = n), levels = 0:2),
    spore = factor(rep(0:3, each = ceiling(n / 4))[seq_len(n)], levels = 0:3),
    sex = rep(c(0, 1), length.out = n)
  )
}

simulation_input_bounds <- function() {
  data.frame(
    param = c(
      "expt_death",
      "survival_param2",
      "expt_maturity",
      "maturity_param2",
      "expt_reproduction",
      "reproduction_param2",
      "n_offspring"
    ),
    min = c(1, 0.5, 1, 0.5, 1, 0.5, 1),
    max = c(100, 5, 30, 5, 10, 5, 10)
  )
}

test_that("create_simulation_input builds a simulation-ready results object", {
  n <- 1000
  results <- create_simulation_input(
    effects = simulation_input_effects(),
    data = simulation_input_data(n),
    covariates = c("par", "spore"),
    sex = "sex",
    config = simulation_input_config(),
    dist = c("wei", "wei", "wei"),
    param_bounds_df = simulation_input_bounds()
  )

  expect_s3_class(results, "lifelihoodResults")
  expect_s3_class(results$lifelihoodData, "lifelihoodData")
  expect_equal(results$sample_size, n)
  expect_equal(results$lifelihoodData$dist, rep("wei", 3))
  expect_true(all(
    c(
      "name",
      "estimation",
      "stderror",
      "parameter",
      "kind",
      "event"
    ) %in%
      names(results$effects)
  ))
  expect_equal(length(coeff(results, "expt_death")), 6)

  pred <- prediction(results, "expt_death", type = "response")
  expect_type(pred, "double")
  expect_equal(length(pred), n)
})

test_that("manual simulation input produces coherent offspring totals", {
  n <- 1000
  results <- create_simulation_input(
    effects = simulation_input_effects(),
    data = simulation_input_data(n),
    covariates = c("par", "spore"),
    sex = "sex",
    config = simulation_input_config(),
    dist = c("wei", "wei", "wei"),
    param_bounds_df = simulation_input_bounds()
  )

  simul <- simulate_life_history(results, seed = 1)
  clutch_cols <- grep("^clutch_", names(simul), value = TRUE)

  expect_equal(nrow(simul), n)
  expect_true(length(clutch_cols) > 0)
  expect_true(any(simul$total_n_offspring > 0))
  expect_true(any(rowSums(!is.na(simul[clutch_cols])) == 0))

  positive_offspring_without_clutch <- simul$total_n_offspring > 0 &
    rowSums(!is.na(simul[clutch_cols])) == 0

  expect_false(any(positive_offspring_without_clutch))

  for (clutch_col in clutch_cols) {
    suffix <- sub("^clutch_", "", clutch_col)
    n_offspring_col <- paste0("clutch_size_", suffix)
    if (n_offspring_col %in% names(simul)) {
      expect_true(all(is.na(simul[[n_offspring_col]][is.na(simul[[
        clutch_col
      ]])])))
    }
  }
})

test_that("create_simulation_input accepts explicit covariate data", {
  n <- 1000
  set.seed(1)
  newdata <- data.frame(
    par = factor(sample(0:2, n, replace = TRUE)),
    spore = factor(sample(0:3, n, replace = TRUE)),
    sex = rep(c(0, 1), length.out = n)
  )

  results <- create_simulation_input(
    effects = simulation_input_effects(),
    data = newdata,
    covariates = c("par", "spore"),
    sex = "sex",
    config = simulation_input_config(),
    dist = c("wei", "wei", "wei"),
    param_bounds_df = simulation_input_bounds()
  )

  simul <- simulate_life_history(results, seed = 1)
  clutch_cols <- grep("^clutch_", names(simul), value = TRUE)

  expect_equal(nrow(results$lifelihoodData$df), n)
  expect_equal(nrow(simul), n)
  expect_true(length(clutch_cols) > 0)
  expect_false(any(
    simul$total_n_offspring > 0 &
      rowSums(!is.na(simul[clutch_cols])) == 0
  ))
})

test_that("create_simulation_input validates fitted effects", {
  effects <- simulation_input_effects()
  effects$n_offspring <- NULL

  expect_error(
    create_simulation_input(
      effects = effects,
      data = simulation_input_data(10),
      covariates = c("par", "spore"),
      sex = "sex",
      config = simulation_input_config(),
      dist = c("wei", "wei", "wei"),
      param_bounds_df = simulation_input_bounds()
    ),
    "`effects` is missing fitted parameter"
  )
})

test_that("create_simulation_input requires data columns", {
  expect_error(
    create_simulation_input(
      effects = simulation_input_effects(),
      covariates = c("par", "spore"),
      sex = "sex",
      config = simulation_input_config(),
      dist = c("wei", "wei", "wei"),
      param_bounds_df = simulation_input_bounds()
    ),
    "`data` must be supplied"
  )

  expect_error(
    create_simulation_input(
      effects = simulation_input_effects(),
      data = subset(simulation_input_data(10), select = -spore),
      covariates = c("par", "spore"),
      sex = "sex",
      config = simulation_input_config(),
      dist = c("wei", "wei", "wei"),
      param_bounds_df = simulation_input_bounds()
    ),
    "`data` is missing column"
  )

  expect_error(
    create_simulation_input(
      effects = simulation_input_effects(),
      data = subset(simulation_input_data(10), select = -sex),
      covariates = c("par", "spore"),
      sex = "sex",
      config = simulation_input_config(),
      dist = c("wei", "wei", "wei"),
      param_bounds_df = simulation_input_bounds()
    ),
    "`data` is missing column"
  )
})

test_that("create_simulation_input expands rows by combination counts", {
  combo_data <- expand.grid(
    par = factor(0:2, levels = 0:2),
    spore = factor(0:3, levels = 0:3),
    sex = c(0, 1),
    KEEP.OUT.ATTRS = FALSE
  )
  combo_data$n <- seq_len(nrow(combo_data))

  results <- create_simulation_input(
    effects = simulation_input_effects(),
    data = combo_data,
    covariates = c("par", "spore"),
    sex = "sex",
    config = simulation_input_config(),
    dist = c("wei", "wei", "wei"),
    n_per_combination = "n",
    param_bounds_df = simulation_input_bounds()
  )

  expect_equal(results$sample_size, sum(combo_data$n))
  expect_equal(nrow(results$lifelihoodData$df), sum(combo_data$n))
  expect_false("n" %in% names(results$lifelihoodData$df))
})

test_that("create_simulation_input uses default lifelihood bounds", {
  results <- create_simulation_input(
    effects = simulation_input_effects(),
    data = simulation_input_data(10),
    covariates = c("par", "spore"),
    sex = "sex",
    config = simulation_input_config(),
    dist = c("wei", "wei", "wei")
  )

  expected_bounds <- normalize_simulation_bounds_df(
    default_bounds_df(results$lifelihoodData)
  )

  expect_equal(results$param_bounds_df, expected_bounds)
})

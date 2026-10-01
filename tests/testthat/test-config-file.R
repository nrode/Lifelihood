test_that("R_to_lifelihood expands interaction terms consistently", {
  covariates <- c("par", "geno")
  covar_types <- c("cat", "cat")

  expect_equal(
    R_to_lifelihood("par * geno", covariates, covar_types),
    c("0 1 2 21", "3")
  )

  expect_equal(
    R_to_lifelihood("par + geno + par * geno", covariates, covar_types),
    c("0 1 2 21", "3")
  )

  expect_equal(
    R_to_lifelihood("par + geno + par:geno", covariates, covar_types),
    c("0 1 2 12", "3")
  )
})

test_that("count_parameters uses expanded interaction terms", {
  config <- list(
    mortality = list(
      expt_death = "par * geno",
      survival_param2 = "1",
      ratio_expt_death = "not_fitted"
    )
  )

  expect_equal(count_parameters(config), 4)

  config$mortality$expt_death <- "par + geno + par * geno"
  expect_equal(count_parameters(config), 4)

  config$mortality$expt_death <- "par + geno + par:geno"
  expect_equal(count_parameters(config), 4)
})

test_that("validate_config_input fills omitted parameters with not_fitted", {
  config <- validate_config_input(
    list(
      mortality = list(expt_death = "par + spore"),
      reproduction = list(n_offspring = 1)
    )
  )

  expect_equal(config$mortality$expt_death, "par + spore")
  expect_equal(config$reproduction$n_offspring, 1)
  expect_equal(config$maturity$expt_maturity, "not_fitted")
  expect_equal(config$reproduction$fitness, "not_fitted")
  expect_equal(length(config), 3)
  expect_equal(length(config$mortality), 5)
  expect_equal(length(config$maturity), 3)
  expect_equal(length(config$reproduction), 12)
})

test_that("validate_config_input accepts YAML paths and rejects invalid lists", {
  config_path <- testthat::test_path("config_gbg.yaml")
  config_from_path <- validate_config_input(config_path)
  expect_equal(config_from_path, validate_config_input(config_from_path))

  expect_error(
    validate_config_input(list(mortality = list(unknown = 1))),
    "Unknown parameter"
  )
  expect_error(
    validate_config_input(list(unknown = list(expt_death = 1))),
    "Unknown configuration section"
  )
  expect_error(
    validate_config_input(list(mortality = 1)),
    "must be a named list"
  )
})

test_that("exponential distributions warn and disable fitted params2", {
  config <- validate_config_input(list(
    mortality = list(survival_param2 = 1),
    maturity = list(maturity_param2 = "par"),
    reproduction = list(reproduction_param2 = 1)
  ))

  expect_warning(
    mortality_config <- validate_config_input(
      config,
      dist = data.frame(
        mortality = "exp",
        maturity = "wei",
        reproduction = "wei"
      )
    ),
    "survival_param2.*exponential.*second parameter"
  )
  expect_warning(
    maturity_config <- validate_config_input(
      config,
      dist = data.frame(
        mortality = "wei",
        maturity = "exp",
        reproduction = "wei"
      )
    ),
    "maturity_param2.*exponential.*second parameter"
  )
  expect_warning(
    reproduction_config <- validate_config_input(
      config,
      dist = data.frame(
        mortality = "wei",
        maturity = "wei",
        reproduction = "exp"
      )
    ),
    "reproduction_param2.*exponential.*second parameter"
  )
  expect_identical(mortality_config$mortality$survival_param2, "not_fitted")
  expect_identical(maturity_config$maturity$maturity_param2, "not_fitted")
  expect_identical(
    reproduction_config$reproduction$reproduction_param2,
    "not_fitted"
  )
  expect_identical(mortality_config$maturity, config$maturity)
  expect_identical(mortality_config$reproduction, config$reproduction)
})

test_that("unfitted params2 are accepted with exponential distributions", {
  config <- validate_config_input(list())

  expect_identical(
    validate_config_input(
      config,
      dist = data.frame(
        mortality = "exp",
        maturity = "exp",
        reproduction = "exp"
      )
    ),
    config
  )
})

test_that("lifelihood warns and disables incompatible exponential parameters", {
  local_mocked_bindings(
    lifelihood_fit = function(config, ...) {
      structure(
        list(likelihood = -1, config = config),
        class = "lifelihoodResults"
      )
    },
    .package = "lifelihood"
  )
  lifelihood_data <- structure(
    list(
      dist = data.frame(
        mortality = "exp",
        maturity = "wei",
        reproduction = "wei"
      )
    ),
    class = "lifelihoodData"
  )

  expect_warning(
    result <- lifelihood(
      lifelihoodData = lifelihood_data,
      config = list(mortality = list(survival_param2 = 1))
    ),
    "survival_param2.*exponential.*second parameter"
  )
  expect_identical(result$config$mortality$survival_param2, "not_fitted")
})

test_that("simulation input warns and disables incompatible exponential parameters", {
  expect_warning(
    result <- create_simulation_input(
      effects = list(expt_death = 0),
      data = data.frame(sex = 0),
      covariates = character(),
      sex = "sex",
      config = list(mortality = list(expt_death = 1, survival_param2 = 1)),
      dist = data.frame(
        mortality = "exp",
        maturity = "wei",
        reproduction = "wei"
      )
    ),
    "survival_param2.*exponential.*second parameter"
  )
  expect_identical(result$config$mortality$survival_param2, "not_fitted")
  expect_false("survival_param2" %in% result$effects$parameter)
})

test_that("path_config is retained as a deprecated lifelihood argument", {
  lifelihood_data <- structure(list(), class = "lifelihoodData")

  expect_warning(
    expect_error(
      lifelihood(
        lifelihoodData = lifelihood_data,
        path_config = "missing-config.yaml"
      ),
      "Configuration file not found"
    ),
    "`path_config` is deprecated; use `config` instead."
  )

  expect_error(
    lifelihood(
      lifelihoodData = lifelihood_data,
      config = list(),
      path_config = "missing-config.yaml"
    ),
    "Supply only one"
  )
})

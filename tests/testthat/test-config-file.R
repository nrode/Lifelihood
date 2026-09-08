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

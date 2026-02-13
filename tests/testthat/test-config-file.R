test_that("R_to_lifelihood expands interaction terms consistently", {
  covariates <- c("par", "geno")
  covar_types <- c("cat", "cat")

  expect_equal(
    R_to_lifelihood("par * geno", covariates, covar_types),
    c("0 1 2 12", "3")
  )

  expect_equal(
    R_to_lifelihood("par + geno + par * geno", covariates, covar_types),
    c("0 1 2 12", "3")
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

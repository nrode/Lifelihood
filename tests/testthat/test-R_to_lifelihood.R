test_that("Conversion config file to valid input format for lifelihood.", {
  result <- lifelihood:::R_to_lifelihood(
    "geno + type + geno*type",
    covariates = c("geno", "type")
  )
  expect_equal(result, c("0 1 2 12", "3"))

  result <- lifelihood:::R_to_lifelihood(
    "geno + geno*type",
    covariates = c("geno", "type")
  )
  expect_equal(result, c("0 1 12", "2"))

  result <- lifelihood:::R_to_lifelihood(
    "geno + treatment + geno*type",
    covariates = c("geno", "treatment", "type")
  )
  expect_equal(result, c("0 1 2 13", "3"))

  result <- lifelihood:::R_to_lifelihood(
    "not_fitted",
    covariates = c("geno", "treatment", "type")
  )
  expect_equal(result, "-1")

  result <- lifelihood:::R_to_lifelihood(
    "1",
    covariates = c("geno", "treatment", "type")
  )
  expect_equal(result, "0")
})

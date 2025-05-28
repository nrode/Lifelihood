results <- get_lifelihoodResults()

test_that("`coef` and `coeff` returns valid object", {
  invalid_input <- c(1, 2, 3)
  expect_error(coeff(invalid_input, "expt_death"))
  expect_error(coef(invalid_input))

  coefs <- coef(results)
  coeffs <- coeff(results, "expt_death")

  expect_type(coefs, "double")
  expect_true(is.vector(coefs))
  expect_true(!is.null(names(coefs)))
  expect_true(all(nzchar(names(coefs))))
  expect_true(length(coeffs) < length(coefs))
  expect_type(coeffs, "double")
  expect_true(is.vector(coeffs))
  expect_true(!is.null(names(coeffs)))
  expect_true(all(nzchar(names(coeffs))))
})

test_that("`logLik()` returns valid object", {
  invalid_input <- c(1, 2, 3)
  expect_error(logLik(invalid_input))

  loglik <- logLik(results)
  expect_type(loglik, "double")
})

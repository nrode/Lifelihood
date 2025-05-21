test_that("`coeff` raises valid error message with wrong input", {
  invalid_input <- c(1, 2, 3)
  expect_error(
    coeff(invalid_input),
    regexp = "`coeff` function expect a 'lifelihoodResults' object, not:"
  )
})

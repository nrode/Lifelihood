test_that("check_lifelihoodResults() raises errors", {
  obj <- c(1, 2)
  expect_error(
    check_lifelihoodResults(obj),
    regexp = "`object` expects a 'lifelihoodResults' object, not: '"
  )
})
test_that("check_lifelihoodResults() does raise errors", {
  obj <- c(1, 2)
  class(obj) <- "lifelihoodResults"
  expect_silent(check_lifelihoodResults(obj))
})

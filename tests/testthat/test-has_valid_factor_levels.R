test_that("has_valid_factor_levels returns TRUE for matching levels", {
  newdata <- data.frame(A = factor(c("a", "b")), B = factor(c("x", "y")))
  df_train <- data.frame(A = factor(c("a", "b", "c")), B = factor(c("x", "y", "z")))
  covariates <- c("A", "B")

  expect_true(has_valid_factor_levels(df_train, newdata, covariates))
})

test_that("has_valid_factor_levels returns FALSE for non-matching levels", {
  newdata <- data.frame(A = factor(c("a", "d")), B = factor(c("x", "y")))
  df_train <- data.frame(A = factor(c("a", "b", "c")), B = factor(c("x", "y", "z")))
  covariates <- c("A", "B")

  expect_false(has_valid_factor_levels(df_train, newdata, covariates))
})

test_that("has_valid_factor_levels returns TRUE if covariates list is empty", {
  newdata <- data.frame(A = factor(c("a", "b")), B = factor(c("x", "y")))
  df_train <- data.frame(A = factor(c("a", "b", "c")), B = factor(c("x", "y", "z")))
  covariates <- character(0)


  expect_warning(expect_false(has_valid_factor_levels(df_train, newdata, covariates)))
})

test_that("has_valid_factor_levels returns FALSE if a covariate is missing in newdata", {
  newdata <- data.frame(A = factor(c("a", "b")))
  df_train <- data.frame(A = factor(c("a", "b", "c")), B = factor(c("x", "y", "z")))
  covariates <- c("A", "B")

  expect_warning(expect_false(has_valid_factor_levels(df_train, newdata, covariates)))
})

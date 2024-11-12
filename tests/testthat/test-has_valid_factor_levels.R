test_that("has_valid_factor_levels returns TRUE for matching levels", {
  # Create data frames with matching levels
  df_train <- data.frame(A = factor(c("a", "b")), B = factor(c("x", "y")))
  df <- data.frame(A = factor(c("a", "b", "c")), B = factor(c("x", "y", "z")))
  covariates <- c("A", "B")

  # Expect TRUE because all levels in df_train are in df
  expect_true(has_valid_factor_levels(df_train, df, covariates))
})

test_that("has_valid_factor_levels returns FALSE for non-matching levels", {
  # Create data frames with non-matching levels
  df_train <- data.frame(A = factor(c("a", "d")), B = factor(c("x", "y")))
  df <- data.frame(A = factor(c("a", "b", "c")), B = factor(c("x", "y", "z")))
  covariates <- c("A", "B")

  # Expect FALSE because "d" in df_train$A is not in df$A
  expect_false(has_valid_factor_levels(df_train, df, covariates))
})

test_that("has_valid_factor_levels returns TRUE if covariates list is empty", {
  # Create data frames with any levels
  df_train <- data.frame(A = factor(c("a", "b")), B = factor(c("x", "y")))
  df <- data.frame(A = factor(c("a", "b", "c")), B = factor(c("x", "y", "z")))
  covariates <- character(0) # Empty covariates list

  # Expect TRUE because there are no covariates to check
  expect_true(has_valid_factor_levels(df_train, df, covariates))
})

test_that("has_valid_factor_levels returns FALSE if a covariate is missing in df_train", {
  # Create data frames with one missing covariate in df_train
  df_train <- data.frame(A = factor(c("a", "b")))
  df <- data.frame(A = factor(c("a", "b", "c")), B = factor(c("x", "y", "z")))
  covariates <- c("A", "B")

  # Expect a warning and FALSE because covariate "B" is missing in df_train
  expect_warning(expect_false(has_valid_factor_levels(df_train, df, covariates)))
})

test_that("has_valid_factor_levels returns FALSE if a covariate is missing in df", {
  # Create data frames with one missing covariate in df
  df_train <- data.frame(A = factor(c("a", "b")), B = factor(c("x", "y")))
  df <- data.frame(A = factor(c("a", "b", "c")))
  covariates <- c("a", "b")

  # Expect a warning and FALSE because covariate "B" is missing in df
  expect_warning(expect_false(has_valid_factor_levels(df_train, df, covariates)))
})

test_that("has_valid_factor_levels returns warning if covariate is not a factor", {
  # Create data frames where one covariate is not a factor
  df_train <- data.frame(A = factor(c("a", "b")), B = c("x", "y")) # B is not a factor
  df <- data.frame(A = factor(c("a", "b", "c")), B = factor(c("x", "y", "z")))
  covariates <- c("A", "B")

  # Expect a warning because B is not a factor in df_train and function returns TRUE for A only
  expect_warning(expect_true(has_valid_factor_levels(df_train, df, covariates)))
})

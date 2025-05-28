library(lifelihood)
library(tidyverse)

test_that("format_config correctly reads and formats the config file", {
  path_config <- test_path(system.file(
    "configs/config.yaml",
    package = "lifelihood"
  ))
  covariates <- c("geno", "type")

  result <- format_config(path_config, covariates)

  expect_equal(length(result), 19)
  expect_equal(result[1], "expt_death 0 1 2")
  expect_equal(result[2], "survival_shape 0 1 2 12")
  expect_equal(result[3], "ratio_expt_death 0 1")
  expect_equal(result[4], "prob_death 0 1")
  expect_equal(result[5], "sex_ratio -1")
  expect_equal(result[9], "expt_reproduction 0 1")
  expect_equal(result[10], "reproduction_shape -1")
  expect_equal(result[19], "tof_n_offspring 0")
})

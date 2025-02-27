test_that("Default bounds df.", {
  df <- read.csv(here::here("data/fake_sample.csv"))
  df$type <- as.factor(df$type)
  df$geno <- as.factor(df$geno)

  clutchs <- c(
    "clutch_start1",
    "clutch_end1",
    "clutch_size1",
    "clutch_start2",
    "clutch_end2",
    "clutch_size2"
  )
  dataLFH <- lifelihoodData(
    df = df,
    sex = "sex",
    sex_start = "sex_start",
    sex_end = "sex_end",
    maturity_start = "mat_start",
    maturity_end = "mat_end",
    clutchs = clutchs,
    death_start = "death_start",
    death_end = "death_end",
    covariates = c("geno", "type"),
    model_specs = c("gam", "lgn", "wei")
  )

  bounds_df <- default_bounds_df(dataLFH)

  max_death <- as.numeric(bounds_df[bounds_df$param == "expt_death", "max"])
  expect_equal(max_death, 40.0)

  max_maturity <- as.numeric(bounds_df[
    bounds_df$param == "expt_maturity",
    "max"
  ])
  expect_equal(max_maturity, 8.0)

  max_clutch <- as.numeric(bounds_df[
    bounds_df$param == "expt_reproduction",
    "max"
  ])
  expect_equal(max_clutch, 10.0)

  maturity_shape_min <- as.numeric(bounds_df[
    bounds_df$param == "maturity_shape",
    "min"
  ])
  expect_equal(maturity_shape_min, 0.005)

  maturity_shape_max <- as.numeric(bounds_df[
    bounds_df$param == "maturity_shape",
    "max"
  ])
  expect_equal(maturity_shape_max, 600.0)

  death_shape_min <- as.numeric(bounds_df[
    bounds_df$param == "survival_shape",
    "min"
  ])
  expect_equal(death_shape_min, 0.05)

  death_shape_max <- as.numeric(bounds_df[
    bounds_df$param == "survival_shape",
    "max"
  ])
  expect_equal(death_shape_max, 500.0)
})

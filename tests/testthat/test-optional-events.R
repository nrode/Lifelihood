test_that("omitted event columns do not change a partial-fit likelihood", {
  df <- datapierrick |>
    as_tibble() |>
    mutate(
      par = as.factor(par),
      spore = as.factor(spore)
    ) |>
    slice_head(n = 150)
  stripped_df <- df |>
    select(-mat_start, -mat_end, -starts_with("clutch_"))

  make_mortality_data <- function(data) {
    as_lifelihoodData(
      df = data,
      sex = "sex",
      sex_start = "sex_start",
      sex_end = "sex_end",
      death_start = "death_start",
      death_end = "death_end",
      covariates = c("par", "spore"),
      dist = data.frame(mortality = "wei")
    )
  }

  fit <- function(data) {
    suppressWarnings(lifelihood(
      data,
      config = list(
        mortality = list(expt_death = "par + spore", survival_param2 = 1)
      ),
      seeds = c(1, 2, 3, 4),
      raise_estimation_warning = FALSE,
      se.fit = FALSE
    ))
  }

  with_omitted_columns <- fit(make_mortality_data(df))
  without_omitted_columns <- fit(make_mortality_data(stripped_df))
  expect_gt(as.numeric(logLik(without_omitted_columns)), -1e6)
  expect_equal(
    as.numeric(logLik(with_omitted_columns)),
    as.numeric(logLik(without_omitted_columns)),
    tolerance = 1e-10
  )
})

test_that("omitted death columns do not affect a maturity-only likelihood", {
  df <- datapierrick |>
    as_tibble() |>
    mutate(par = as.factor(par)) |>
    slice_head(n = 150)
  stripped_df <- df |>
    select(-death_start, -death_end)

  make_maturity_data <- function(data) {
    as_lifelihoodData(
      df = data,
      sex = "sex",
      sex_start = "sex_start",
      sex_end = "sex_end",
      maturity_start = "mat_start",
      maturity_end = "mat_end",
      covariates = "par",
      dist = data.frame(maturity = "gam")
    )
  }
  fit <- function(data) {
    suppressWarnings(lifelihood(
      data,
      config = list(
        maturity = list(expt_maturity = "par", maturity_param2 = 1)
      ),
      seeds = c(1, 2, 3, 4),
      raise_estimation_warning = FALSE,
      se.fit = FALSE
    ))
  }

  with_omitted_columns <- fit(make_maturity_data(df))
  without_omitted_columns <- fit(make_maturity_data(stripped_df))
  expect_gt(as.numeric(logLik(without_omitted_columns)), -1e6)
  expect_equal(
    as.numeric(logLik(with_omitted_columns)),
    as.numeric(logLik(without_omitted_columns)),
    tolerance = 1e-10
  )
})

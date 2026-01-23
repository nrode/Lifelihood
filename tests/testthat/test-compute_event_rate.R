testthat::test_that("Overall demo works", {
  path_config <- if (rlang::is_interactive()) {
    "tests/testthat/config.yaml"
  } else {
    "config.yaml"
  }

  df <- datapierrick |>
    as_tibble() |>
    mutate(
      par = as.factor(par),
      geno = as.factor(geno),
      spore = as.factor(spore)
    )

  clutchs <- generate_clutch_vector(28)

  lifelihoodData <- as_lifelihoodData(
    df = df,
    sex = "sex",
    sex_start = "sex_start",
    sex_end = "sex_end",
    maturity_start = "mat_start",
    maturity_end = "mat_end",
    clutchs = clutchs,
    death_start = "death_start",
    death_end = "death_end",
    covariates = c("par", "spore"),
    model_specs = c("wei", "gam", "exp")
  )

  results <- lifelihood(
    lifelihoodData,
    path_config = path_config,
    raise_estimation_warning = FALSE
  )

  check_compute_event_rate <- function(event, interval_width) {
    df_rate <- compute_fitted_event_rate(
      results,
      interval_width = interval_width,
      event = event
    )
    expect_s3_class(df_rate, "data.frame")
    expect_type(df_rate$time, "double")
    expect_type(df_rate$par, "integer")
    expect_type(df_rate$Interval_start, "double")
    expect_type(df_rate$Interval_end, "double")
    expect_type(df_rate$Mean_Interval, "double")
    expect_type(df_rate$Event_Rate, "double")
    expect_type(df_rate$group, "integer")
    expect_true(all(df_rate$group == "Overall"))
  }

  check_compute_event_rate("maturity", 5)
  check_compute_event_rate("maturity", 15)

  check_compute_event_rate("mortality", 5)
  check_compute_event_rate("mortality", 15)

  check_compute_event_rate("reproduction", 5)
  check_compute_event_rate("reproduction", 15)
})

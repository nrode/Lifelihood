testthat::test_that("lifelihoodResults works", {
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
    ) |>
    sample_n(200)

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

  args_list <- list(
    list(
      lifelihoodData = lifelihoodData,
      path_config = path_config,
      n_fit = 3,
      raise_estimation_warning = FALSE,
      se.fit = FALSE
    ),
    list(
      lifelihoodData = lifelihoodData,
      path_config = path_config,
      n_fit = 1,
      raise_estimation_warning = FALSE,
      se.fit = TRUE
    ),
    list(
      lifelihoodData = lifelihoodData,
      path_config = path_config,
      n_fit = 1,
      raise_estimation_warning = FALSE,
      MCMC = 30
    )
  )

  results_list <- lapply(args_list, function(args) {
    do.call(lifelihood, args)
  })

  # Run tests on each result
  lapply(results_list, function(results) {
    expect_true(results$config |> length() == 3)
    expect_true(results$config$mortality |> length() == 5)
    expect_true(results$config$maturity |> length() == 3)
    expect_true(results$config$reproduction |> length() == 12)
    expect_true(is.numeric(results$likelihood))
    expect_true(ncol(results$parameter_ranges) == 3)
    expect_true(results$sample_size == 200)
    expect_true(length(results$seeds) == 4)

    df_effects <- results$effects

    expect_s3_class(df_effects, "data.frame")

    print(names(df_effects))
    expect_true(
      all(
        c("name", "estimation", "stderror", "parameter", "kind", "event") %in%
          names(df_effects)
      )
    )

    expect_type(df_effects$name, "character")
    expect_type(df_effects$parameter, "character")
    expect_type(df_effects$kind, "character")
    expect_type(df_effects$event, "character")
    expect_type(df_effects$estimation, "double")
    expect_type(df_effects$stderror, "double")

    expect_setequal(
      unique(df_effects$kind),
      c("intercept", "coefficient_category")
    )
    expect_setequal(
      unique(df_effects$event),
      c("mortality", "maturity", "reproduction")
    )

    expect_true(length(coef(results)) == 16)
    expect_true(all(grepl("^(int_|eff_)", names(coef(results)))))
    expect_true(length(coeff(results, "expt_death")) == 6)
    expect_true(length(coeff(results, "survival_param2")) == 1)
    expect_true(length(coeff(results, "n_offspring")) == 1)
    expect_true(is.numeric(AIC(results)))
    expect_true(is.numeric(BIC(results)))
    expect_true(is.numeric(logLik(results)))
  })
})

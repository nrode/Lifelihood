multiple_model_data <- function() {
  df <- datapierrick |>
    group_by(par) |>
    slice_head(n = 10) |>
    ungroup() |>
    mutate(par = as.factor(par))

  as_lifelihoodData(
    df = df,
    sex = "sex",
    sex_start = "sex_start",
    sex_end = "sex_end",
    maturity_start = "mat_start",
    maturity_end = "mat_end",
    clutchs = generate_clutch_vector(28),
    death_start = "death_start",
    death_end = "death_end",
    matclutch = FALSE,
    covariates = "par",
    dist = data.frame(
      reproduction = c("lgn", "exp"),
      maturity = c("gam", "exp"),
      mortality = c("wei", "exp")
    )
  )
}

multiple_model_config <- function(formula = 1) {
  list(
    mortality = list(expt_death = formula, survival_param2 = formula),
    maturity = list(expt_maturity = formula, maturity_param2 = formula),
    reproduction = list(
      expt_reproduction = formula,
      reproduction_param2 = formula,
      n_offspring = formula
    )
  )
}

test_that("every distribution row and replicate is fitted and retained", {
  data <- multiple_model_data()
  result <- suppressWarnings(lifelihood(
    data,
    config = multiple_model_config(),
    n_fit = 2,
    raise_estimation_warning = FALSE
  ))

  expect_s3_class(result, "lifelihoodResults")
  expect_s3_class(result$all_models, "all_models")
  expect_named(
    result$all_models,
    c(
      "lifelihood_fit_1_1",
      "lifelihood_fit_2_1",
      "lifelihood_fit_1_2",
      "lifelihood_fit_2_2"
    )
  )
  expect_equal(
    result$likelihood,
    max(vapply(
      result$all_models,
      function(x) x$likelihood,
      numeric(1)
    ))
  )
  expect_identical(result$dist, result$lifelihoodData$dist)
  expect_equal(nrow(result$dist), 1)
  expect_equal(nrow(data$dist), 2)

  for (j in 1:2) {
    for (i in 1:2) {
      fit <- result$all_models[[paste0("lifelihood_fit_", i, "_", j)]]
      expect_identical(fit$dist, data$dist[j, , drop = FALSE])
      expect_identical(fit$dist, fit$lifelihoodData$dist)
      expect_equal(length(prediction(fit, "expt_death")), nrow(data$df))
      rates <- compute_fitted_event_rate(
        fit,
        event = "mortality",
        interval_width = 5
      )
      expect_s3_class(rates, "data.frame")
      expect_true(all(is.finite(rates$Event_Rate)))
      expect_s3_class(
        simulate_life_history(fit, event = "mortality", seed = 1),
        "data.frame"
      )
    }
  }

  weibull_fit <- result$all_models[[1]]
  exponential_fit <- result$all_models[[3]]
  expect_equal(nrow(weibull_fit$effects) - nrow(exponential_fit$effects), 3)
  expect_equal(weibull_fit$config$mortality$survival_param2, 1)
  expect_identical(
    exponential_fit$config$mortality$survival_param2,
    "not_fitted"
  )
  expect_identical(
    exponential_fit$config$maturity$maturity_param2,
    "not_fitted"
  )
  expect_identical(
    exponential_fit$config$reproduction$reproduction_param2,
    "not_fitted"
  )
  expect_equal(
    as.numeric(weibull_fit$param_bounds_df$max[
      weibull_fit$param_bounds_df$param == "survival_param2"
    ]),
    500
  )
  expect_equal(
    as.numeric(exponential_fit$param_bounds_df$max[
      exponential_fit$param_bounds_df$param == "survival_param2"
    ]),
    1000
  )

  comparison <- summary(result$all_models)
  expect_equal(nrow(comparison), 4)
  expect_equal(
    comparison$n_parameters,
    vapply(
      result$all_models[comparison$fit],
      function(x) nrow(x$effects),
      integer(1)
    ),
    ignore_attr = TRUE
  )
  expect_true(all(diff(comparison$AICc) >= 0))
  expect_equal(
    comparison[["\u0394AICc"]],
    comparison$AICc - min(comparison$AICc)
  )
  expect_true("int_survival_param2" %in% names(comparison))
  expect_true(all(is.na(comparison$int_survival_param2[
    comparison$dist_mortality == "exp"
  ])))
})

test_that("multiple distribution rows work with group-by-group replicates", {
  data <- multiple_model_data()
  result <- suppressWarnings(lifelihood(
    data,
    config = multiple_model_config("par"),
    group_by_group = TRUE,
    n_fit = 2
  ))

  expect_length(result$all_models, 4)
  for (fit in result$all_models) {
    expect_true(fit$group_by_group)
    expect_equal(nrow(fit$dist), 1)
    expect_identical(fit$dist, fit$lifelihoodData$dist)
    expect_length(fit$group_names, 3)
    expect_equal(fit$likelihood, sum(fit$group_likelihoods))
  }
  expect_equal(nrow(summary(result$all_models)), 4)
})

test_that("convergence compares replicates within each model and seeds are preserved", {
  captured <- list()
  local_mocked_bindings(
    lifelihood_fit = function(lifelihoodData, config, seeds, temp_dir, ...) {
      captured[[length(captured) + 1]] <<- list(
        seeds = seeds,
        temp_dir = temp_dir
      )
      structure(
        list(
          likelihood = if (lifelihoodData$dist$mortality == "exp") {
            -200
          } else {
            -100
          },
          lifelihoodData = lifelihoodData,
          dist = lifelihoodData$dist,
          config = config
        ),
        class = "lifelihoodResults"
      )
    },
    .package = "lifelihood"
  )

  data <- multiple_model_data()
  seeds <- c(1, 2, 3, 4)
  expect_no_warning(lifelihood(
    data,
    config = multiple_model_config(),
    seeds = seeds
  ))
  expect_identical(captured[[1]]$seeds, seeds)
  expect_identical(captured[[2]]$seeds, seeds)
  expect_false(identical(captured[[1]]$temp_dir, captured[[2]]$temp_dir))

  captured <- list()
  set.seed(1)
  expect_no_warning(lifelihood(
    data,
    config = multiple_model_config(),
    n_fit = 2
  ))
  expect_length(captured, 4)
  expect_equal(length(unique(lapply(captured, function(x) x$seeds))), 4)
})

test_that("simulation inputs and custom default bounds require one model", {
  data <- multiple_model_data()
  expect_error(
    default_bounds_df(data),
    "requires one model"
  )
  expect_error(
    create_simulation_input(
      effects = list(),
      data = data$df,
      covariates = "par",
      sex = "sex",
      config = list(),
      dist = data$dist
    ),
    "exactly one row"
  )
})

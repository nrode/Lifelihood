sex_ratio_simulation_input <- function(
  n = 200,
  fitted = TRUE,
  tradeoff = FALSE
) {
  data <- data.frame(
    group = factor(rep(c("a", "b"), length.out = n)),
    sex_code = rep(0, n)
  )
  config <- list(
    mortality = list(
      expt_death = 1,
      ratio_expt_death = 1,
      sex_ratio = if (fitted) "group" else "not_fitted"
    ),
    maturity = list(expt_maturity = 1, ratio_expt_maturity = 1),
    reproduction = list(expt_reproduction = 1, n_offspring = 1)
  )
  effects <- list(
    expt_death = 0,
    ratio_expt_death = 0,
    expt_maturity = 0,
    ratio_expt_maturity = 0,
    expt_reproduction = 0,
    n_offspring = 0
  )
  if (fitted) {
    effects$sex_ratio <- c(qlogis(0.1), qlogis(0.9) - qlogis(0.1))
  }
  if (tradeoff) {
    config$reproduction$increase_death_hazard <- 1
    effects$increase_death_hazard <- 0
  }

  create_simulation_input(
    effects = effects,
    data = data,
    covariates = "group",
    sex = "sex_code",
    block = "group",
    config = config,
    dist = data.frame(
      mortality = "exp",
      maturity = "exp",
      reproduction = "exp"
    ),
    param_bounds_df = data.frame(
      param = c(
        "expt_death",
        "ratio_expt_death",
        "sex_ratio",
        "expt_maturity",
        "ratio_expt_maturity",
        "expt_reproduction",
        "n_offspring",
        "increase_death_hazard"
      ),
      min = c(1, 0, 0, 0.5, 0, 0.2, 1, 0),
      max = c(9, 4, 1, 1.5, 4, 0.8, 3, 2)
    )
  )
}

test_that("fitted sex ratios draw reproducible sex using covariates", {
  object <- sex_ratio_simulation_input(n = 2000)
  original <- object
  sim <- simulate_life_history(object, event = "mortality", seed = 7)

  expect_identical(
    sim,
    simulate_life_history(object, event = "mortality", seed = 7)
  )
  expect_identical(object, original)
  expect_true(all(sim$sex_code %in% c(0, 1)))
  expect_equal(sum(names(sim) == "sex_code"), 1)
  expect_false(any(grepl("sex_code\\.\\.\\.", names(sim))))
  expect_true(any(sim$sex_code != original$lifelihoodData$df$sex_code))
  expect_lt(abs(mean(sim$sex_code[sim$group == "a"]) - 0.1), 0.05)
  expect_lt(abs(mean(sim$sex_code[sim$group == "b"]) - 0.9), 0.05)
  expect_identical(sim$sex_start, original$lifelihoodData$df$sex_start)
  expect_identical(sim$sex_end, original$lifelihoodData$df$sex_end)
})

test_that("drawn sex controls mortality and maturity predictions", {
  object <- sex_ratio_simulation_input()

  for (event in c("mortality", "maturity")) {
    set.seed(7)
    data <- object$lifelihoodData$df
    data$sex_code <- rbinom(
      nrow(data),
      size = 1,
      prob = prediction(object, "sex_ratio", type = "response")
    )
    expected <- simulate_event(object, event, newdata = data)[[event]]
    sim <- simulate_life_history(object, event = event, seed = 7)

    expect_identical(sim$sex_code, data$sex_code)
    expect_equal(sim[[paste0(event, "_start")]], expected)
    expect_equal(sim[[paste0(event, "_end")]], expected)
  }
})

test_that("newdata sex is preserved without changing its rows or input values", {
  object <- sex_ratio_simulation_input()
  newdata <- data.frame(
    id = 30:1,
    group = factor(rep(c("b", "a"), each = 15), levels = c("a", "b")),
    sex_code = rep(c(0, 1), 15)
  )
  original <- newdata
  sim <- simulate_life_history(
    object,
    event = "mortality",
    newdata = newdata,
    seed = 7
  )
  expect_identical(newdata, original)
  expect_identical(sim$id, newdata$id)
  expect_identical(sim$group, newdata$group)
  expect_identical(sim$sex_code, newdata$sex_code)
  expect_equal(sum(names(sim) == "sex_code"), 1)
})

test_that("newdata without a sex column gives a clear error", {
  object <- sex_ratio_simulation_input()
  without_sex <- data.frame(group = factor(c("a", "b")))
  expect_error(
    simulate_life_history(object, event = "mortality", newdata = without_sex),
    "`newdata` must include a column for the sex of individuals named `sex_code`"
  )
})

test_that("unfitted sex ratios preserve input sex", {
  object <- sex_ratio_simulation_input(fitted = FALSE)
  object$lifelihoodData$df$sex_code <- rep(c(0, 1), length.out = 200)
  sim <- simulate_life_history(object, seed = 7)
  expect_identical(sim$sex_code, object$lifelihoodData$df$sex_code)

  newdata <- object$lifelihoodData$df[10:1, c("group", "sex_code")]
  sim_new <- simulate_life_history(object, newdata = newdata, seed = 7)
  expect_identical(sim_new$sex_code, newdata$sex_code)
})

test_that("drawn sex controls reproduction with and without visit masks", {
  visits <- tidyr::expand_grid(group = c("a", "b"), visit = 0:500)

  for (tradeoff in c(FALSE, TRUE)) {
    object <- sex_ratio_simulation_input(n = 40, tradeoff = tradeoff)
    original <- object
    raw <- simulate_life_history(object, event = "reproduction", seed = 7)
    masked <- simulate_life_history(
      object,
      visits = visits,
      remove_exact_clutch_dates = FALSE,
      seed = 7
    )
    expect_identical(object, original)
    expect_identical(raw$sex_code, masked$sex_code)

    for (sim in list(raw, masked)) {
      males <- sim$sex_code == 1
      females <- sim$sex_code == 0
      clutch_cols <- grep(
        "^(clutch_|exact_clutch_date_)",
        names(sim),
        value = TRUE
      )
      expect_true(any(males))
      expect_true(any(females))
      expect_true(all(is.na(sim[males, clutch_cols])))
      expect_true(all(is.na(sim$total_n_clutches[males])))
      expect_true(all(is.na(sim$total_n_offspring[males])))
      expect_true(any(sim$total_n_offspring[females] > 0, na.rm = TRUE))
    }
  }
})

test_that("male mortality is unaffected by reproduction tradeoffs", {
  for (fitted in c(FALSE, TRUE)) {
    object <- sex_ratio_simulation_input(
      n = 20,
      fitted = fitted,
      tradeoff = TRUE
    )
    object$lifelihoodData$df$sex_code[] <- 1
    if (fitted) {
      object$effects$estimation[object$effects$parameter == "sex_ratio"] <- c(
        Inf,
        0
      )
    }
    without_hazard <- object
    without_hazard$effects$estimation[
      without_hazard$effects$parameter == "increase_death_hazard"
    ] <- -Inf

    sim <- simulate_life_history(object, seed = 7)
    control <- simulate_life_history(without_hazard, seed = 7)
    expect_true(all(sim$sex_code == 1))
    expect_identical(sim, control)

    # Predict male newdata from an otherwise female input dataset, ensuring
    # the tradeoff path uses the sex supplied for the simulated individuals.
    male_newdata <- object$lifelihoodData$df[10:1, c("group", "sex_code")]
    object$lifelihoodData$df$sex_code[] <- 0
    without_hazard$lifelihoodData$df$sex_code[] <- 0
    sim_new <- simulate_life_history(object, newdata = male_newdata, seed = 7)
    control_new <- simulate_life_history(
      without_hazard,
      newdata = male_newdata,
      seed = 7
    )
    expect_true(all(sim_new$sex_code == 1))
    expect_identical(sim_new, control_new)
  }
})

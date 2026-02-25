#' @title Simulation for a single life event
#'
#' @description Internal function used to simulate one of the
#' life history event (maturity, reproduction, mortality).
#'
#' @param object Output of [`lifelihood()`]
#' @param ev A character of the event (must be one of "mortality",
#' "reproduction" or "maturity")
#' @param newdata An optional dataset used for prediction
#' @param lifelihoodData Output of [lifelihood::as_lifelihoodData()].
#' @param visits Dataframe with 2 columns: "block" (must be the same as passed
#' in [lifelihood::as_lifelihoodData()] `block` argument) and exactly "visit". For
#' each block, "visit" corresponds to the ages where the events of individuals
#' have been recorded.
#'
#' @keywords internal
simulate_event <- function(
  object,
  ev,
  newdata,
  lifelihoodData,
  use_censoring,
  visits,
  block_values = NULL
) {
  if (ev == "mortality") {
    expt_name <- "expt_death"
    shape_name <- "survival_param2"
    fam_id <- 1
    n <- 1
  } else if (ev == "reproduction") {
    expt_name <- "expt_reproduction"
    shape_name <- "reproduction_param2"
    fam_id <- 3
  } else if (ev == "maturity") {
    expt_name <- "expt_maturity"
    shape_name <- "maturity_param2"
    fam_id <- 2
    n <- 1
  }

  expected <- prediction(
    object,
    expt_name,
    type = "response",
    newdata = newdata
  )
  shape <- tryCatch(
    prediction(object, shape_name, type = "response", newdata = newdata),
    error = function(e) return(NULL)
  )
  if (is.null(expected) || is.null(shape)) {
    return(NULL)
  }

  family <- object$lifelihoodData$model_specs[[fam_id]]
  if (ev == "reproduction") {
    expt_death <- tryCatch(
      prediction(object, "expt_death", type = "response", newdata = newdata),
      error = function(e) return(NULL)
    )
    survival_param2 <- tryCatch(
      prediction(
        object,
        "survival_param2",
        type = "response",
        newdata = newdata
      ),
      error = function(e) return(NULL)
    )

    ## max longeity= value of longevity so that 99% of individuals with this shape and scale parameters die before this age
    family_mortality <- object$lifelihoodData$model_specs[[1]]
    if (family_mortality == "wei") {
      scale <- expt_death / gamma(1 + 1 / survival_param2)
      long <- qweibull(0.99999999, shape = survival_param2, scale = scale)
    } else if (family_mortality == "lgn") {
      mu <- log(expt_death) - 0.5 * log(1 + survival_param2 / (expt_death^2))
      sigma <- sqrt(log(1 + survival_param2 / (expt_death^2)))
      long <- qlnorm(0.99999999, meanlog = mu, sdlog = sigma)
    } else if (family_mortality == "gam") {
      shape <- expt_death / survival_param2
      long <- qgamma(0.99999999, shape = shape, scale = survival_param2)
    } else if (family_mortality == "exp") {
      long <- qexp(0.99999999, rate = 1 / expt_death)
    }
    max_long <- max(long) # maximum predicted longevity in the dataset

    # minimum predicted reproduction interval in the dataset
    min_reproduction_interval <- min(expected)

    # Maximum number of reproduction for all individual
    n <- floor(max_long / min_reproduction_interval) + 1

    n_offspring <- tryCatch(
      prediction(
        object,
        "n_offspring",
        type = "response",
        newdata = newdata
      ),
      error = function(e) return(rep(NA, length(expected)))
    )

    simul_n_offspring <- simulate_truncPois(expected = n_offspring, n = n)
  }

  if (family == "wei") {
    simul <- simulate_weibull(expected, shape, n = n)
  } else if (family == "gam") {
    simul <- simulate_gamma(expected, scale = shape, n = n)
  } else if (family == "lgn") {
    simul <- simulate_lognormal(expected, vp1 = shape, n = n)
  } else if (family == "exp") {
    simul <- simulate_exponential(expected, n = n)
  } else {
    stop(sprintf("Unknown family '%s' for event '%s'.", family, ev))
  }

  # transpose the matrix to ensure consistent format in this specific event
  if (ev == "reproduction") {
    if (n > 1) {
      simul_t <- t(simul)
      simul_n_offspring_t <- t(simul_n_offspring)
      column_names <- paste("clutch", 1:n, sep = "_")
      n_offspring_column_names <- paste("n_offspring_clutch", 1:n, sep = "_")
    } else {
      column_names <- "clutch_1"
      n_offspring_column_names <- "n_offspring_clutch_1"
      simul_t <- simul
      simul_n_offspring_t <- simul_n_offspring
    }
    simul_df <- simul_t |> as_tibble(.name_repair = "minimal")
    colnames(simul_df) <- column_names
    simul_df_n_offspring_t <- simul_n_offspring_t |>
      as_tibble(.name_repair = "minimal")
    colnames(simul_df_n_offspring_t) <- n_offspring_column_names

    simul_df_full <- bind_cols(simul_df, simul_df_n_offspring_t)
    columns_order <- as.vector(rbind(column_names, n_offspring_column_names))
    simul_df_full <- simul_df_full[, columns_order]
  } else {
    column_names <- ev
    simul_df_full <- simul |> as_tibble(.name_repair = "minimal")
    colnames(simul_df_full) <- column_names
  }

  # add blocks, if provided
  if (!is.null(lifelihoodData$block) && use_censoring && ev != "reproduction") {
    simul_df_full <- add_visit_masks(
      simul_df = simul_df_full,
      lifelihoodData = lifelihoodData,
      event = ev,
      visits = visits,
      block_values = block_values
    )
  }

  return(simul_df_full)
}

#' @keywords internal
simulate_life_history_tradeoff <- function(
  object,
  newdata,
  lifelihoodData,
  dt = 0.1
) {
  n_obs <- if (is.null(newdata)) nrow(lifelihoodData$df) else nrow(newdata)

  family_mortality <- lifelihoodData$model_specs[[1]]
  family_maturity <- lifelihoodData$model_specs[[2]]
  family_reproduction <- lifelihoodData$model_specs[[3]]

  expt_death <- prediction(
    object,
    "expt_death",
    type = "response",
    newdata = newdata
  )
  survival_param2 <- tryCatch(
    prediction(object, "survival_param2", type = "response", newdata = newdata),
    error = function(e) NULL
  )
  if (is.null(survival_param2)) {
    if (family_mortality == "exp") {
      survival_param2 <- rep(1, n_obs)
    } else {
      stop(
        "Could not predict `survival_param2` required for mortality simulation."
      )
    }
  }

  expt_maturity <- prediction(
    object,
    "expt_maturity",
    type = "response",
    newdata = newdata
  )
  maturity_param2 <- tryCatch(
    prediction(object, "maturity_param2", type = "response", newdata = newdata),
    error = function(e) NULL
  )
  if (is.null(maturity_param2)) {
    if (family_maturity == "exp") {
      maturity_param2 <- rep(1, n_obs)
    } else {
      stop(
        "Could not predict `maturity_param2` required for maturity simulation."
      )
    }
  }

  expt_reproduction_link <- prediction(
    object,
    "expt_reproduction",
    type = "link",
    newdata = newdata
  )
  reproduction_param2 <- tryCatch(
    prediction(
      object,
      "reproduction_param2",
      type = "response",
      newdata = newdata
    ),
    error = function(e) NULL
  )
  if (is.null(reproduction_param2)) {
    if (family_reproduction == "exp") {
      reproduction_param2 <- rep(1, n_obs)
    } else {
      stop(
        "Could not predict `reproduction_param2` required for reproduction simulation."
      )
    }
  }

  n_offspring <- if (is_parameter_fitted(object, "n_offspring")) {
    tryCatch(
      prediction(object, "n_offspring", type = "response", newdata = newdata),
      error = function(e) rep(NA_real_, n_obs)
    )
  } else {
    rep(NA_real_, n_obs)
  }

  d <- predict_or_default(object, "increase_death_hazard", newdata, n_obs)
  da <- predict_or_default(object, "tof_reduction_rate", newdata, n_obs)
  dn <- predict_or_default(object, "increase_tof_n_offspring", newdata, n_obs)
  senput <- predict_or_default(object, "lin_decrease_hazard", newdata, n_obs)

  expt_reproduction_bounds <- subset(
    object$param_bounds_df,
    param == "expt_reproduction"
  )
  expt_reproduction_min <- suppressWarnings(
    as.numeric(expt_reproduction_bounds$min[1])
  )
  expt_reproduction_max <- suppressWarnings(
    as.numeric(expt_reproduction_bounds$max[1])
  )
  if (!is.finite(expt_reproduction_min) || !is.finite(expt_reproduction_max)) {
    stop("Could not retrieve finite numeric bounds for `expt_reproduction`.")
  }

  max_long <- compute_max_longevity(
    expected = expt_death,
    shape = survival_param2,
    family = family_mortality
  )
  max_long[!is.finite(max_long)] <- lifelihoodData$right_censoring_date
  max_long <- pmax(max_long, lifelihoodData$right_censoring_date)

  maturity <- rep(NA_real_, n_obs)
  mortality <- rep(NA_real_, n_obs)
  clutch_times <- vector("list", n_obs)
  clutch_sizes <- vector("list", n_obs)

  for (i in seq_len(n_obs)) {
    t <- 0
    alive <- TRUE
    matured <- FALSE
    maturity_time <- NA_real_
    last_reproduction_time <- NA_real_
    clutch_times_i <- numeric(0)
    clutch_sizes_i <- integer(0)
    max_iter <- ceiling(max_long[i] / dt) + 1
    iter <- 0

    while (alive && t < max_long[i] && iter <= max_iter) {
      iter <- iter + 1

      if (length(clutch_times_i) > 0) {
        offspring_effect <- ifelse(is.na(clutch_sizes_i), 0, clutch_sizes_i)
        elapsed <- pmax(t - clutch_times_i, 0)
        if (da[i] == 0) {
          decay <- rep(1, length(elapsed))
        } else {
          decay <- exp(-da[i] * elapsed)
        }
        discount <- sum((d[i] + dn[i] * offspring_effect) * decay)
      } else {
        discount <- 0
      }

      p_die <- prob_event_interval_dt_safe(
        t = t,
        dt = dt,
        param1 = expt_death[i],
        param2 = survival_param2[i],
        family = family_mortality
      )
      p_die <- clamp_probability(p_die + discount * dt)
      if (runif(1) < p_die) {
        mortality[i] <- t + dt / 2
        alive <- FALSE
        break
      }

      matured_this_interval <- FALSE
      if (!matured) {
        p_maturity <- prob_event_interval_dt_safe(
          t = t,
          dt = dt,
          param1 = expt_maturity[i],
          param2 = maturity_param2[i],
          family = family_maturity
        )
        if (runif(1) < p_maturity) {
          maturity_time <- t + dt / 2
          maturity[i] <- maturity_time
          last_reproduction_time <- maturity_time
          matured <- TRUE
          matured_this_interval <- TRUE
        }
      }

      if (matured && !matured_this_interval) {
        t_from_last <- max(t - last_reproduction_time, 0)
        t_from_maturity <- max(t - maturity_time, 0)
        expt_reproduction <- link(
          estimate = expt_reproduction_link[i] + senput[i] * t_from_maturity,
          min = expt_reproduction_min,
          max = expt_reproduction_max
        )
        p_reproduction <- prob_event_interval_dt_safe(
          t = t_from_last,
          dt = dt,
          param1 = expt_reproduction,
          param2 = reproduction_param2[i],
          family = family_reproduction
        )
        if (runif(1) < p_reproduction) {
          reproduction_time <- t + dt / 2
          clutch_times_i <- c(clutch_times_i, reproduction_time)
          clutch_size_i <- if (is.finite(n_offspring[i])) {
            simulate_truncPois_single(n_offspring[i])
          } else {
            NA_integer_
          }
          clutch_sizes_i <- c(clutch_sizes_i, clutch_size_i)
          last_reproduction_time <- reproduction_time
        }
      }

      t <- t + dt
    }

    if (is.na(mortality[i])) {
      mortality[i] <- max_long[i]
    }

    clutch_times[[i]] <- clutch_times_i
    clutch_sizes[[i]] <- clutch_sizes_i
  }

  clutch_intervals <- vector("list", n_obs)
  for (i in seq_len(n_obs)) {
    clutch_times_i <- clutch_times[[i]]
    if (length(clutch_times_i) == 0 || is.na(maturity[i])) {
      clutch_intervals[[i]] <- numeric(0)
      next
    }

    clutch_intervals_i <- clutch_times_i
    clutch_intervals_i[1] <- clutch_times_i[1] - maturity[i]
    if (length(clutch_times_i) > 1) {
      clutch_intervals_i[2:length(clutch_times_i)] <- diff(clutch_times_i)
    }
    clutch_intervals[[i]] <- clutch_intervals_i
  }

  n_clutch <- max(c(1L, lengths(clutch_intervals)))
  clutch_matrix <- matrix(NA_real_, nrow = n_obs, ncol = n_clutch)
  n_offspring_matrix <- matrix(NA_integer_, nrow = n_obs, ncol = n_clutch)

  for (i in seq_len(n_obs)) {
    n_i <- length(clutch_intervals[[i]])
    if (n_i > 0) {
      clutch_matrix[i, seq_len(n_i)] <- clutch_intervals[[i]]
      n_offspring_matrix[i, seq_len(n_i)] <- as.integer(clutch_sizes[[i]])
    }
  }

  sim_df <- tibble(mortality = mortality)
  for (j in seq_len(n_clutch)) {
    sim_df[[paste0("clutch_", j)]] <- clutch_matrix[, j]
    sim_df[[paste0("n_offspring_clutch_", j)]] <- n_offspring_matrix[, j]
  }
  sim_df$maturity <- maturity

  return(sim_df)
}

#' @title Simulate outcomes from a fitted lifelihood model
#'
#' @description This function generates simulated data from a fitted lifelihood model,
#' for one or several life history events. By default, all fitted events are simulated.
#'
#' @param object A fitted `lifelihoodResults` object.
#' @param event Character string specifying the event(s) to simulate.
#'   Must be one of `"mortality"`, `"maturity"`, or `"all"` (event=`"reproduction"` is equivalent to `"all"` as maturity and mortality are needed to simulate reproduction events).
#'   Default is `"all"`, which simulates all fitted events.
#' @param newdata Optional `data.frame` providing covariate values for prediction.
#'   If `NULL`, the original model data are used.
#' @param use_censoring Whether to retrieve censoring time intervals for scalar
#' events (`maturity`, `mortality`). For example, returns `mortality_start` and
#' `mortality_end` instead of only `mortality`. If `newdata` is provided and
#' censoring is enabled, `newdata` must include the block column.
#' In this case, it's advised to use the `visits` argument and provide your own
#' visit data. Otherwise, visits are inferred from observed event ages.
#' @param visits Optionnal dataframe with 2 columns: "block" (must be the same name
#' as passed in [lifelihood::as_lifelihoodData()] `block` argument) and exactly "visit".
#' For each block, "visit" corresponds to the ages where the events of individuals
#' have been recorded.
#' @param seed Optional integer. If provided, sets the random seed for reproducibility.
#'
#' @return A list of `data.frame` with one column per simulated event.
#'   Each column contains simulated values for that event.
#'
#' @export
simulate_life_history <- function(
  object,
  event = c("all", "mortality", "reproduction", "maturity"),
  newdata = NULL,
  use_censoring = FALSE,
  visits = NULL,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  check_lifelihoodResults(object)

  if (isTRUE(object$group_by_group)) {
    stop(
      "simulate_life_history() is not supported for group_by_group results. ",
      "Use coef() to access per-group estimates."
    )
  }

  event <- match.arg(
    event,
    c("all", "mortality", "reproduction", "maturity"),
    several.ok = TRUE
  )

  events <- if ("all" %in% event | "reproduction" %in% event) {
    c("maturity", "reproduction", "mortality")
  } else {
    event
  }

  lifelihoodData <- object$lifelihoodData
  block_values <- NULL
  if (!is.null(lifelihoodData$block) && use_censoring) {
    block_col <- lifelihoodData$block
    if (is.null(newdata)) {
      block_values <- lifelihoodData$df[[block_col]]
    } else {
      if (!(block_col %in% names(newdata))) {
        stop(
          "`use_censoring = TRUE` with `newdata` requires a `",
          block_col,
          "` column."
        )
      }
      block_values <- newdata[[block_col]]
    }
  }

  if ("reproduction" %in% events) {
    fitted_params <- object$formula |> names()
    if (
      !("expt_maturity" %in% fitted_params && "expt_death" %in% fitted_params)
    ) {
      stop(
        "To simulate reproduction, the fitted object must also include both ",
        "maturity and mortality models."
      )
    }
  }

  use_tradeoff_path <- "reproduction" %in%
    events &&
    uses_tradeoff_simulation(object)

  if (use_tradeoff_path) {
    df_sims <- simulate_life_history_tradeoff(
      object,
      newdata = newdata,
      lifelihoodData = lifelihoodData
    )
    if (!is.null(lifelihoodData$block) && use_censoring) {
      df_sims <- add_visit_masks(
        simul_df = df_sims,
        lifelihoodData = lifelihoodData,
        event = "maturity",
        visits = visits,
        block_values = block_values
      )
      df_sims <- add_visit_masks(
        simul_df = df_sims,
        lifelihoodData = lifelihoodData,
        event = "mortality",
        visits = visits,
        block_values = block_values
      )
    }
  } else {
    df_sims <- NULL
    for (ev in events) {
      sim <- simulate_event(
        object,
        ev,
        newdata,
        lifelihoodData,
        use_censoring,
        visits,
        block_values
      )
      df_sims <- bind_cols(sim, df_sims)
    }
  }

  if ("reproduction" %in% events) {
    ## Compute actual age for each reproduction event (=age at maturity + sum over all previous reproduction events)
    df_sims_up <- df_sims |>
      mutate(clutch_1 = clutch_1 + maturity) |>
      relocate(maturity, .before = clutch_1)

    clutch_cols <- grep("^clutch_", names(df_sims_up), value = TRUE)
    n_offspring_cols <- grep(
      "^n_offspring_clutch_",
      names(df_sims_up),
      value = TRUE
    )

    # Sum duration between clutches to get age at which clutches are made
    df_sims_up[clutch_cols] <- t(apply(df_sims_up[clutch_cols], 1, cumsum))

    # Convert to NA clutches that occurred after simulated death
    df_sims_up_na <- df_sims_up |>
      mutate(across(starts_with("clutch_"), ~ ifelse(. > mortality, NA, .)))

    # Remove clutch columns with only NA because they all occurred
    # after simulated death
    remove_cols <- colnames(df_sims_up_na[clutch_cols])[
      colSums(is.na(df_sims_up_na[clutch_cols])) == nrow(df_sims_up_na)
    ]
    remove_cols_all <- colnames(df_sims_up_na)[c(
      which(colnames(df_sims_up_na) %in% remove_cols) + 1,
      which(colnames(df_sims_up_na) %in% remove_cols)
    )]
    df_sims_up_na <- df_sims_up_na |> select(-all_of(remove_cols_all))

    if (object$lifelihoodData$matclutch) {
      ## Remove maturity which is not observed when matchcluth is TRUE
      df_sims_up_na <- df_sims_up_na |>
        select(-maturity) |>
        rename(
          maturity = clutch_1,
          !!as.symbol(
            object$lifelihoodData$matclutch_size
          ) := n_offspring_clutch_1
        )
    }
  } else {
    df_sims_up_na <- df_sims
  }

  if ("reproduction" %in% events) {
    df_sims_up_na <- df_sims_up_na |>
      mutate(
        total_n_offspring = rowSums(
          across(starts_with("n_offspring_clutch_")),
          na.rm = TRUE
        )
      )
  }

  if (is.null(newdata)) {
    simul_lifelihood_Data <- object$lifelihoodData
    simul_lifelihood_Data$df <- df_sims_up_na
    #sex <- object$lifelihoodData$
  }

  as_lifelihoodData(df = df_sims_up_na)

  df_sims_up_na
}

#' @title Parallelized simulations
#'
#' @param ... Arguments passed to [simulate_life_history()].
#' @param nsim Number of simulations
#' @param parallel_seed Logical or integer, seed used for
parallel.simulate <- function(..., nsim, parallel_seed = FALSE) {
  sims <- future.apply::future_lapply(
    1:nsim,
    function(i) simulate_life_history(...),
    future.seed = parallel_seed
  )
  sims <- sims[!vapply(sims, is.null, logical(1))]

  return(sims)
}

#' @keywords internal
simulate_weibull <- function(expected, shape, n) {
  # expected = scale * gamma(1 + 1 / shape)
  scale <- expected / gamma(1 + 1 / shape)
  return(mapply(rweibull, shape = shape, scale = scale, n = n))
}

#' @keywords internal
simulate_gamma <- function(expected, scale, n) {
  # expected = shape * scale
  shape <- expected / scale
  return(mapply(rgamma, shape = shape, scale = scale, n = n))
}

#' @keywords internal
simulate_lognormal <- function(expected, vp1, n) {
  mu <- log(expected) - 0.5 * log(1 + vp1 / (expected^2))
  sigma <- sqrt(log(1 + vp1 / (expected^2)))
  return(mapply(rlnorm, meanlog = mu, sdlog = sigma, n = n))
}

#' @keywords internal
simulate_exponential <- function(expected, n) {
  rate <- 1 / expected
  return(sapply(rate, rexp, n = n))
}

#' @keywords internal
simulate_truncPois_single <- function(expected) {
  simulate_truncPois_draws(lambda = expected, n = 1L)[1]
}

#' @keywords internal
simulate_truncPois_draws <- function(lambda, n) {
  if (!is.finite(lambda) || lambda <= 0) {
    return(rep(NA_integer_, n))
  }

  p0 <- exp(-lambda)
  u <- runif(n)
  draws <- qpois(p0 + u * (1 - p0), lambda = lambda)
  draws <- as.integer(draws)
  draws[draws < 1L] <- 1L
  draws
}

#' @keywords internal
simulate_truncPois <- function(expected, n) {
  n <- as.integer(n)
  if (length(n) != 1 || is.na(n) || n < 1) {
    stop("`n` must be a single positive integer.")
  }

  expected <- as.numeric(expected)
  if (length(expected) == 0) {
    return(integer(0))
  }
  draws <- lapply(
    expected,
    function(lambda) simulate_truncPois_draws(lambda = lambda, n = n)
  )

  if (n == 1L) {
    return(vapply(draws, function(x) x[1], integer(1)))
  }

  out <- do.call(cbind, draws)
  storage.mode(out) <- "integer"
  out
}

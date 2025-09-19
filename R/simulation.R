#' @title Simulation for a single life event
#'
#' @description Internal function used to simulate one of the
#' life history event (maturity, reproduction, mortality).
#'
#' @param object Output of [`lifelihood()`]
#' @param ev A character of the event (must be one of "mortality",
#' "reproduction" or "maturity")
#' @param newdata An optional dataset used for prediction
#'
#' @keywords internal
simulate_event <- function(object, ev, newdata) {
  if (ev == "mortality") {
    expt_name <- "expt_death"
    shape_name <- "survival_shape"
    fam_id <- 1
    n <- 1
  } else if (ev == "reproduction") {
    expt_name <- "expt_reproduction"
    shape_name <- "reproduction_shape"
    fam_id <- 3
  } else if (ev == "maturity") {
    expt_name <- "expt_maturity"
    shape_name <- "maturity_shape"
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
    survival_shape <- tryCatch(
      prediction(
        object,
        "survival_shape",
        type = "response",
        newdata = newdata
      ),
      error = function(e) return(NULL)
    )

    ## max longeity= value of longevity so that 99% of individuals with this shape and scale parameters die before this age
    family_mortality <- object$lifelihoodData$model_specs[[1]]
    if (family_mortality == "wei") {
      scale <- expt_death / gamma(1 + 1 / survival_shape)
      long <- qweibull(0.99999999, shape = survival_shape, scale = scale)
    } else if (family_mortality == "lgn") {
      mu <- log(expt_death) - 0.5 * log(1 + survival_shape / (expt_death^2))
      sigma <- sqrt(log(1 + survival_shape / (expt_death^2)))
      long <- qlnorm(0.99999999, meanlog = mu, sdlog = sigma)
    } else if (family_mortality == "gam") {
      shape <- expt_death / survival_shape
      long <- qgamma(0.99999999, shape = shape, scale = survival_shape)
    } else if (family_mortality == "exp") {
      long <- qexp(0.99999999, rate = 1 / expt_death)
    }
    max_long <- max(long) # maximum predicted longevity in the dataset

    # minimum predicted reproduction interval in the dataset
    min_reproduction_interval <- min(expected)

    # Maximum number of reproduction for all individual
    n <- floor(max_long / min_reproduction_interval) + 1

    n_offspring <- tryCatch(
      prediction(object, "n_offspring", type = "response", newdata = newdata),
      error = function(e) return(rep(NA, length(expected)))
    )
    #n_offspring <- floor(expt_death) # temporary workaround
    simul_n_offspring <- simulate_truncPois(
      expected = n_offspring,
      n = n
    )
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
    stop(
      sprintf("Unknown family '%s' for event '%s'.", family, ev),
      call. = FALSE
    )
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
    simul_df <- simul_t |> as.tibble()
    colnames(simul_df) <- column_names
    simul_df_n_offspring_t <- simul_n_offspring_t |> as.tibble()
    colnames(simul_df_n_offspring_t) <- n_offspring_column_names

    simul_df_full <- bind_cols(simul_df, simul_df_n_offspring_t)
    columns_order <- as.vector(rbind(column_names, n_offspring_column_names))
    simul_df_full <- simul_df_full[, columns_order]
  } else {
    column_names <- ev
    simul_df_full <- simul |> as.tibble()
    colnames(simul_df_full) <- column_names
  }

  return(simul_df_full)
}

#' @title Simulate outcomes from a fitted lifelihood model
#'
#' @description This function generates simulated data from a fitted lifelihood model,
#' for one or several life history events. By default, all fitted events are simulated.
#'
#' @param object A fitted `lifelihoodResults` object.
#' @param event Character string specifying the event(s) to simulate.
#'   Must be one of `"mortality"`, `"reproduction"`, `"maturity"`, or `"all"`.
#'   Default is `"all"`, which simulates all fitted events.
#' @param newdata Optional `data.frame` providing covariate values for prediction.
#'   If `NULL`, the original model data are used.
#' @param seed Optional integer. If provided, sets the random seed for reproducibility.
#' @param nsim Number of simulations per individual in newdata (default: nsim=1)
#'
#' @return A list of `data.frame` with one column per simulated event.
#'   Each column contains simulated values for that event.
#'
#' @export
simulate_life_history <- function(
  object,
  event = c("all", "mortality", "reproduction", "maturity"),
  newdata = NULL,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  check_valid_lifelihoodResults(object)

  event <- match.arg(event, c("all", "mortality", "reproduction", "maturity"))

  events <- if (event == "all") {
    c("maturity", "reproduction", "mortality")
  } else {
    event
  }

  df_sims <- NULL
  for (event in events) {
    sim <- simulate_event(object, event, newdata)
    df_sims <- bind_cols(sim, df_sims)
  }

  if ("reproduction" %in% events) {
    df_sims_up <- df_sims |>
      mutate(clutch_1 = clutch_1 + maturity) |>
      relocate(maturity, .before = clutch_1)

    clutch_cols <- grep("^clutch_", names(df_sims_up), value = TRUE)
    n_offspring_cols <- grep(
      "^n_offspring_clutch_",
      names(df_sims_up),
      value = TRUE
    )
    df_sims_up[clutch_cols] <- t(apply(df_sims_up[clutch_cols], 1, cumsum))

    df_sims_up_na <- df_sims_up |>
      mutate(across(starts_with("clutch_"), ~ ifelse(. > mortality, NA, .)))

    remove_cols <- colnames(df_sims_up_na[clutch_cols])[
      apply(df_sims_up_na[clutch_cols], 2, is.na) |>
        colSums() ==
        nrow(df_sims_up_na)
    ]
    remove_cols_all <- colnames(df_sims_up_na)[c(
      which(colnames(df_sims_up_na) %in% remove_cols) + 1,
      which(colnames(df_sims_up_na) %in% remove_cols)
    )]

    df_sims_up_na <- df_sims_up_na |> select(-all_of(remove_cols_all))

    if (object$lifelihoodData$matclutch) {
      df_sims_up_na <- df_sims_up_na |>
        select(-maturity) |>
        rename(maturity = clutch_1, n_offspring_maturity = n_offspring_clutch_1)
    }
  } else {
    df_sims_up_na <- df_sims
  }

  df_sims_up_na
}

#' @title Parallelized simulations
#'
#' @param ... Arguments passed to [simulate_life_history()].
#' @param nsim Number of simulations
#' @param parallel_seed Logical or integer, seed used for
parallel.simulate <- function(..., nsim, parallel_seed = 1) {
  sims <- future.apply::future_lapply(
    1:nsim,
    simulate_life_history,
    ...,
    future.seed = parallel_seed
  )
  sims <- sims[!vapply(sims, is.null, logical(1))]

  sims
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
simulate_truncPois <- function(expected, n) {
  trunc_Pois <- function(lambda) {
    zer <- 0
    while (zer == 0) {
      data_num_offspring <- rpois(n = n, lambda = lambda)
      ifelse(
        all(data_num_offspring != 0 | is.na(data_num_offspring)),
        zer <- 1,
        zer <- 0
      )
    }
    data_num_offspring
  }
  sapply(expected, trunc_Pois)
}

#' @title Parametric goodness-of-fit from simulated datasets
#'
#' @description
#' Simulate datasets from a fitted model, refit the model on each simulated
#' dataset, and compare simulated log-likelihood values to the original fit.
#'
#' @param object Output of [lifelihood()].
#' @param nsim Number of simulated datasets to generate and refit.
#' @param seed Optional integer seed for reproducibility.
#' @param fit_args Named list of additional arguments passed to [lifelihood()]
#' when refitting each simulated dataset. By default, refits use
#' `n_fit = 1`, `MCMC = 0`, and `se.fit = FALSE` for speed.
#' @param keep_fits Whether to store fitted objects for each successful
#' simulation. Default is `FALSE`.
#'
#' @return A `lifelihoodGOF` object (list) with:
#' - `original_loglik`: log-likelihood of the original fit
#' - `simulated_loglik`: numeric vector of simulated/refitted log-likelihoods
#' - `n_success`: number of successful refits
#' - `n_failed`: number of failed refits
#' - `p_lower_or_equal`: proportion of simulated log-likelihoods lower than or
#' equal to the original value
#' - `errors`: per-simulation error messages (if any)
#' - `fits`: optional list of fitted objects (only if `keep_fits = TRUE`)
#'
#' @export
goodness_of_fit <- function(
  object,
  nsim,
  seed = NULL,
  fit_args = list()
) {
  check_lifelihoodResults(object)

  if (isTRUE(object$group_by_group)) {
    stop(
      "goodness_of_fit() is not supported for group_by_group results.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(nsim) ||
      length(nsim) != 1 ||
      is.na(nsim) ||
      nsim < 1 ||
      nsim %% 1 != 0
  ) {
    stop("`nsim` must be a single positive integer.", call. = FALSE)
  }
  nsim <- as.integer(nsim)

  if (!is.list(fit_args)) {
    stop("`fit_args` must be a list.", call. = FALSE)
  }
  if (length(fit_args) > 0) {
    if (is.null(names(fit_args)) || any(names(fit_args) == "")) {
      stop("`fit_args` must be a named list.", call. = FALSE)
    }
  }

  lfh <- object$lifelihoodData
  if (isTRUE(lfh$matclutch)) {
    stop(
      "goodness_of_fit() does not currently support models fitted with ",
      "`matclutch = TRUE`.",
      call. = FALSE
    )
  }

  events_to_simulate <- infer_simulation_events(object)
  if (length(events_to_simulate) == 0) {
    stop(
      "No fitted event can be simulated from this model.",
      call. = FALSE
    )
  }

  temp_config <- NULL
  if (is.null(object$config)) {
    stop(
      "`path_config` is NULL and `object$config` is missing. ",
      "Provide `path_config` explicitly.",
      call. = FALSE
    )
  }
  temp_config <- tempfile(
    pattern = "lifelihood_gof_config_",
    fileext = ".yaml"
  )
  yaml::write_yaml(object$config, file = temp_config)
  path_config <- temp_config

  if (!is.null(temp_config)) {
    on.exit(unlink(temp_config), add = TRUE)
  }

  sim_seeds <- sample.int(.Machine$integer.max, nsim)
  fit_seeds <- replicate(
    nsim,
    sample.int(10000, 4, replace = TRUE),
    simplify = FALSE
  )

  original_loglik <- as.numeric(logLik(object))
  simulated_loglik <- rep(NA_real_, nsim)
  errors <- rep(NA_character_, nsim)
  fits <- vector("list", nsim)

  for (i in seq_len(nsim)) {
    simulated <- simulate_life_history(
      object = object,
      event = events_to_simulate,
      seed = sim_seeds[i]
    )

    sim_df <- build_simulated_lifelihood_df(
      simulated = simulated,
      lifelihoodData = lfh
    )

    sim_lfh <- as_lifelihoodData(
      df = sim_df,
      sex = lfh$sex,
      sex_start = lfh$sex_start,
      sex_end = lfh$sex_end,
      maturity_start = lfh$maturity_start,
      maturity_end = lfh$maturity_end,
      clutchs = lfh$clutchs,
      death_start = lfh$death_start,
      death_end = lfh$death_end,
      model_specs = lfh$model_specs,
      covariates = lfh$covariates,
      block = lfh$block,
      matclutch = lfh$matclutch,
      matclutch_size = lfh$matclutch_size,
      right_censoring_date = lfh$right_censoring_date,
      critical_age = lfh$critical_age,
      ratiomax = lfh$ratiomax
    )

    base_fit_args <- list(
      lifelihoodData = sim_lfh,
      path_config = path_config,
      param_bounds_df = object$param_bounds_df,
      n_fit = 1,
      group_by_group = FALSE,
      MCMC = 0,
      se.fit = FALSE,
      raise_estimation_warning = FALSE
    )
    call_args <- merge_lifelihood_args(base_fit_args, fit_args)
    if (is.null(call_args$seeds)) {
      call_args$seeds <- fit_seeds[[i]]
    }

    fit <- tryCatch(
      do.call(lifelihood, call_args),
      error = function(e) e
    )

    if (inherits(fit, "error")) {
      errors[i] <- conditionMessage(fit)
      next
    }

    simulated_loglik[i] <- as.numeric(logLik(fit))
    fits[[i]] <- fit
  }

  valid <- !is.na(simulated_loglik)
  p_lower_or_equal <- if (any(valid)) {
    mean(simulated_loglik[valid] <= original_loglik)
  } else {
    NA_real_
  }

  out <- list(
    original_loglik = original_loglik,
    simulated_loglik = simulated_loglik,
    nsim = nsim,
    n_success = sum(valid),
    n_failed = sum(!valid),
    p_lower_or_equal = p_lower_or_equal,
    errors = errors
  )
  out$fits <- fits

  class(out) <- "lifelihoodGOF"
  return(out)
}

#' @title Plot goodness-of-fit histogram
#'
#' @description
#' Plot a histogram of simulated/refitted log-likelihood values and overlay a
#' vertical line for the original model log-likelihood.
#'
#' @param x Output of [goodness_of_fit()].
#' @param bins Number of histogram bins.
#' @param fill Histogram fill color.
#' @param color Histogram border color.
#' @param line_color Color of the vertical line showing original log-likelihood.
#' @param ... Ignored.
#'
#' @return A ggplot object.
#'
#' @export
plot.lifelihoodGOF <- function(
  gof,
  bins = 30,
  fill = "grey80",
  color = "white",
  line_color = "red",
  ...
) {
  check_lifelihoodGOF(gof)

  if (
    !is.numeric(bins) ||
      length(bins) != 1 ||
      is.na(bins) ||
      bins < 1 ||
      bins %% 1 != 0
  ) {
    stop("`bins` must be a single positive integer.", call. = FALSE)
  }
  bins <- as.integer(bins)

  plot_df <- data.frame(loglikelihood = gof$simulated_loglik)
  plot_df <- plot_df[!is.na(plot_df$loglikelihood), , drop = FALSE]

  if (nrow(plot_df) == 0) {
    stop("No successful simulation fit available to plot.", call. = FALSE)
  }

  ggplot(plot_df, aes(x = loglikelihood)) +
    geom_histogram(bins = bins, fill = fill, color = color) +
    geom_vline(
      xintercept = gof$original_loglik,
      color = line_color,
      linewidth = 1
    ) +
    labs(
      x = "Simulated log-likelihood",
      y = "Count",
      title = "Goodness of fit",
      subtitle = paste0(
        "Original log-likelihood: ",
        signif(gof$original_loglik, 6)
      )
    ) +
    theme_minimal()
}

#' @keywords internal
infer_simulation_events <- function(object) {
  has_param <- function(param_name) {
    param_name %in% object$effects$parameter
  }

  can_sim_mortality <- has_param("expt_death") &&
    has_param("survival_param2")
  can_sim_maturity <- has_param("expt_maturity") &&
    has_param("maturity_param2")
  can_sim_reproduction <- has_param("expt_reproduction") &&
    has_param("reproduction_param2") &&
    can_sim_mortality &&
    can_sim_maturity

  if (can_sim_reproduction) {
    return("reproduction")
  }

  events <- c()
  if (can_sim_maturity) {
    events <- c(events, "maturity")
  }
  if (can_sim_mortality) {
    events <- c(events, "mortality")
  }
  return(events)
}

#' @keywords internal
build_simulated_lifelihood_df <- function(simulated, lifelihoodData) {
  if (is.null(simulated) || !is.data.frame(simulated)) {
    stop(
      "simulate_life_history() did not return a data.frame.",
      call. = FALSE
    )
  }

  df <- lifelihoodData$df
  right_censoring_date <- lifelihoodData$right_censoring_date

  if (nrow(simulated) != nrow(df)) {
    stop(
      "Simulated dataset and original dataset have different numbers of rows.",
      call. = FALSE
    )
  }

  if ("maturity" %in% colnames(simulated)) {
    maturity_intervals <- to_integer_intervals(
      times = simulated$maturity,
      right_censoring_date = right_censoring_date
    )
    df[[lifelihoodData$maturity_start]] <- maturity_intervals$start
    df[[lifelihoodData$maturity_end]] <- maturity_intervals$end
  }

  if ("mortality" %in% colnames(simulated)) {
    mortality_intervals <- to_integer_intervals(
      times = simulated$mortality,
      right_censoring_date = right_censoring_date
    )
    df[[lifelihoodData$death_start]] <- mortality_intervals$start
    df[[lifelihoodData$death_end]] <- mortality_intervals$end
  }

  if (length(lifelihoodData$clutchs) %% 3 != 0) {
    stop(
      "`lifelihoodData$clutchs` must contain start/end/size triplets.",
      call. = FALSE
    )
  }

  n_clutch_slots <- as.integer(length(lifelihoodData$clutchs) / 3)
  for (i in seq_len(n_clutch_slots)) {
    start_col <- lifelihoodData$clutchs[((i - 1) * 3) + 1]
    end_col <- lifelihoodData$clutchs[((i - 1) * 3) + 2]
    size_col <- lifelihoodData$clutchs[((i - 1) * 3) + 3]

    sim_time_col <- paste0("clutch_", i)
    sim_size_col <- paste0("n_offspring_clutch_", i)

    if (sim_time_col %in% colnames(simulated)) {
      clutch_times <- simulated[[sim_time_col]]
      clutch_times[clutch_times >= right_censoring_date] <- NA_real_
      clutch_intervals <- to_integer_intervals(
        times = clutch_times,
        right_censoring_date = right_censoring_date,
        keep_na = TRUE
      )
      df[[start_col]] <- clutch_intervals$start
      df[[end_col]] <- clutch_intervals$end
    } else {
      df[[start_col]] <- NA_integer_
      df[[end_col]] <- NA_integer_
    }

    if (sim_size_col %in% colnames(simulated)) {
      clutch_size <- simulated[[sim_size_col]]
      if (sim_time_col %in% colnames(simulated)) {
        clutch_size[is.na(df[[start_col]])] <- NA
      }
      df[[size_col]] <- as.integer(clutch_size)
    } else {
      df[[size_col]] <- NA_integer_
    }
  }

  return(df)
}

#' @keywords internal
to_integer_intervals <- function(
  times,
  right_censoring_date,
  keep_na = FALSE
) {
  times <- as.numeric(times)
  missing_values <- is.na(times)

  if (!keep_na) {
    times[missing_values] <- right_censoring_date
  }

  times <- pmax(times, 0)
  times <- pmin(times, right_censoring_date)

  start <- floor(times)
  end <- ceiling(times)

  # Keep positive-width intervals for exact integer event times.
  needs_width <- !is.na(start) &
    !is.na(end) &
    (end <= start) &
    (start < right_censoring_date)
  end[needs_width] <- start[needs_width] + 1
  end <- pmin(end, right_censoring_date)

  if (keep_na) {
    start[missing_values] <- NA
    end[missing_values] <- NA
  }

  list(
    start = as.integer(start),
    end = as.integer(end)
  )
}

#' @keywords internal
merge_lifelihood_args <- function(defaults, overrides) {
  if (length(overrides) == 0) {
    return(defaults)
  }

  for (nm in names(overrides)) {
    defaults[[nm]] <- overrides[[nm]]
  }
  return(defaults)
}

#' @keywords internal
check_lifelihoodGOF <- function(object) {
  if (!(inherits(object, "lifelihoodGOF"))) {
    stop(paste0(
      "`object` expects a 'lifelihoodGOF' object, not: '",
      class(object),
      "'"
    ))
  }
}

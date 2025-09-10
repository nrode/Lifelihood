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
simulation <- function(
  object,
  event = c("all", "mortality", "reproduction", "maturity"),
  newdata = NULL,
  seed = NULL,
  nsim = 1
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  check_valid_lifelihoodResults(object)

  event <- match.arg(event, c("all", "mortality", "reproduction", "maturity"))

  simulate_one <- function(ev) {
    if (ev == "mortality") {
      expt_name <- "expt_death"
      shape_name <- "survival_shape"
      fam_id <- 1
    } else if (ev == "reproduction") {
      expt_name <- "expt_reproduction"
      shape_name <- "reproduction_shape"
      fam_id <- 3
    } else if (ev == "maturity") {
      expt_name <- "expt_maturity"
      shape_name <- "maturity_shape"
      fam_id <- 2
    }

    expected <- tryCatch(
      prediction(object, expt_name, type = "response", newdata = newdata),
      error = function(e) return(NULL)
    )
    shape <- tryCatch(
      prediction(object, shape_name, type = "response", newdata = newdata),
      error = function(e) return(NULL)
    )
    if (is.null(expected) || is.null(shape)) {
      return(NULL)
    }

    family <- object$lifelihoodData$model_specs[[fam_id]]

    if (family == "wei") {
      simulate_weibull(expected, shape, nsim)
    } else if (family == "gam") {
      simulate_gamma(expected, scale = shape, nsim)
    } else if (family == "lgn") {
      simulate_lognormal(expected, vp1 = shape, nsim)
    } else if (family == "exp") {
      simulate_exponential(expected, nsim)
    } else {
      stop(
        sprintf("Unknown family '%s' for event '%s'.", family, ev),
        call. = FALSE
      )
    }
  }

  events <- if (event == "all") {
    c("maturity", "reproduction", "mortality")
  } else {
    event
  }

  sims <- future.apply::future_lapply(
    events,
    simulate_one,
    future.seed = if (!is.null(seed)) TRUE else NA
  )
  sims <- sims[!vapply(sims, is.null, logical(1))]

  if (length(sims) == 0) {
    stop(
      "No events could be simulated. Check your fitted model.",
      call. = FALSE
    )
  }

  out <- as.data.frame(sims)
  names(out) <- names(sims) <- events[seq_along(sims)]
  out
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
simulate_truncPois <- function(expected, max_num_clutches) {
  zer <- 0
  while (zer == 0) {
    data_num_offspring <- rpois(max_num_clutches, expected)
    ifelse(all(data_num_offspring != 0), zer <- 1, zer <- 0)
  }
  return(data_num_offspring)
}

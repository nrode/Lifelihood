#' @title Simulate outcomes from a fitted lifelihood model
#'
#' @description This function generates simulated data from a fitted lifelihood model,
#' based on the specified event type and the underlying distribution family
#' (Weibull, Gamma, Lognormal, or Exponential). The parameters of the chosen
#' distribution are derived from the model predictions.
#'
#' @param object A fitted `lifelihoodResults` object.
#' @param event Character string specifying the type of event to simulate.
#'   Must be one of `"mortality"`, `"reproduction"`, or `"maturity"`.
#' @param newdata Optional `data.frame` providing covariate values for prediction.
#'   If `NULL`, the original model data are used.
#'
#' @return A numeric vector of simulated values, with length equal to the number
#'   of observations in the model data.
#'
#' @details
#' The distribution used depends on the model specification:
#' - `"wei"`: Weibull distribution
#' - `"gam"`: Gamma distribution
#' - `"lgn"`: Lognormal distribution
#' - `"exp"`: Exponential distribution
#'
#' The scale/shape parameters are derived such that the mean of the simulated
#' distribution matches the predicted expectation from the model.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming `results` is a lifelihoodResults object
#' sims <- simulation(results, event = "mortality")
#' hist(sims, breaks = 30)
#' }
simulation <- function(
  object,
  event = c("mortality", "reproduction", "maturity"),
  newdata = NULL
) {
  check_valid_lifelihoodResults(object)

  event <- match.arg(event)
  if (event == "mortality") {
    expt_name <- "expt_death"
    shape_name <- "survival_shape"
  } else if (event == "reproduction") {
    expt_name <- "expt_reproduction"
    shape_name <- "reproduction_shape"
  } else if (event == "maturity") {
    expt_name <- "expt_maturity"
    shape_name <- "maturity_shape"
  }

  expected <- tryCatch(
    {
      prediction(object, expt_name, type = "response", newdata = newdata) |>
        mean()
    },
    error = function(e) {
      stop(
        sprintf(
          "Event '%s' has not been fitted. Check your configuration file.",
          event
        ),
        call. = FALSE
      )
    }
  )

  shape <- tryCatch(
    {
      prediction(object, shape_name, type = "response", newdata = newdata) |>
        mean()
    },
    error = function(e) {
      stop(
        sprintf(
          "Event '%s' has not been fitted. Check your configuration file.",
          event
        ),
        call. = FALSE
      )
    }
  )

  families <- results$lifelihoodData$model_specs

  if (event == "mortality") {
    family <- families[[3]]
  } else if (event == "reproduction") {
    family <- families[[2]]
  } else if (event == "maturity") {
    family <- families[[1]]
  }

  n <- nrow(object$lifelihoodData$df)

  if (family == "wei") {
    simulate_weibull(expected = expected, shape = shape, n = n)
  } else if (family == "gam") {
    simulate_gamma(expected = expected, scale = shape, n = n)
  } else if (family == "lgn") {
    simulate_lognormal(expected = expected, vp1 = shape, n = n)
  } else if (family == "exp") {
    simulate_exponential(expected = expected, n = n)
  }
}

#' @keywords internal
simulate_weibull <- function(expected, shape, n) {
  # expected = scale * gamma(1 + 1 / shape)
  scale <- expected / gamma(1 + 1 / shape)
  return(rweibull(n = n, shape = shape, scale = scale))
}

#' @keywords internal
simulate_gamma <- function(expected, scale, n) {
  # expected = shape * scale
  shape <- expected / scale
  return(rgamma(n = n, shape = shape, scale = scale))
}

#' @keywords internal
simulate_lognormal <- function(expected, vp1, n) {
  mu <- log(expected) - 0.5 * log(1 + vp1 / (expected^2))
  sigma <- sqrt(log(1 + vp1 / (expected^2)))
  return(rlnorm(n = n, meanlog = mu, sdlog = sigma))
}

#' @keywords internal
simulate_exponential <- function(expected, n) {
  rate <- 1 / expected
  rexp(n = n, rate = rate)
}

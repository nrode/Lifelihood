#' @title Check whether a parameter was fitted
#'
#' @description
#' Internal helper used by simulation code to determine whether a parameter
#' is available in a fitted `lifelihoodResults` object.
#'
#' @param object A fitted `lifelihoodResults` object (output of [lifelihood()]).
#' @param parameter_name Character scalar with the parameter name to check.
#'
#' @return A logical scalar. `TRUE` if `parameter_name` is present among fitted
#'   effects, `FALSE` otherwise.
#'
#' @keywords internal
is_parameter_fitted <- function(object, parameter_name) {
  parameter_name %in% object$effects$parameter
}

#' @title Detect tradeoff-aware simulation requirements
#'
#' @description
#' Internal helper that checks whether the fitted model includes any tradeoff
#' parameters requiring the tradeoff simulation path.
#'
#' @param object A fitted `lifelihoodResults` object (output of [lifelihood()]).
#'
#' @return A logical scalar indicating whether tradeoff simulation should be
#'   used.
#'
#' @keywords internal
uses_tradeoff_simulation <- function(object) {
  tradeoff_params <- c(
    "increase_death_hazard",
    "increase_tof_n_offspring",
    "tof_reduction_rate",
    "lin_decrease_hazard"
  )
  any(tradeoff_params %in% object$effects$parameter)
}

#' @title Predict a parameter with fallback values
#'
#' @description
#' Internal helper used in simulations to predict a parameter on the response
#' scale and safely fall back to a default value when the parameter is not
#' fitted, prediction fails, or predicted values are non-finite.
#'
#' @param object A fitted `lifelihoodResults` object (output of [lifelihood()]).
#' @param parameter_name Character scalar with the parameter name to predict.
#' @param newdata Optional `data.frame` used for prediction.
#' @param n_obs Integer number of observations expected in the output vector.
#' @param default Numeric default value used as fallback.
#'
#' @return A numeric vector of length `n_obs`.
#'
#' @keywords internal
predict_or_default <- function(
  object,
  parameter_name,
  newdata,
  n_obs,
  default = 0
) {
  if (!is_parameter_fitted(object, parameter_name)) {
    return(rep(default, n_obs))
  }

  pred <- tryCatch(
    prediction(
      object,
      parameter_name,
      type = "response",
      newdata = newdata
    ),
    error = function(e) rep(default, n_obs)
  )

  pred <- as.numeric(pred)
  pred[!is.finite(pred)] <- default
  pred
}

#' @title Clamp probabilities to valid bounds
#'
#' @description
#' Internal helper that constrains probability values to the `[0, 1]` interval
#' and replaces non-finite entries by `0`.
#'
#' @param x Numeric vector of probabilities.
#'
#' @return A numeric vector with values in `[0, 1]`.
#'
#' @keywords internal
clamp_probability <- function(x) {
  x <- as.numeric(x)
  x[!is.finite(x)] <- 0
  pmin(pmax(x, 0), 1)
}

#' @title Safe interval event probability
#'
#' @description
#' Internal wrapper around [prob_event_interval_dt()] that guarantees output
#' probabilities are valid by applying [clamp_probability()].
#'
#' @inheritParams prob_event_interval_dt
#'
#' @return A numeric vector of probabilities in `[0, 1]`.
#'
#' @keywords internal
prob_event_interval_dt_safe <- function(t, dt, param1, param2, family) {
  prob <- prob_event_interval_dt(
    t = t,
    dt = dt,
    param1 = param1,
    param2 = param2,
    family = family
  )
  if (any(!is.finite(prob))) {
    surv_t <- surv(t, param1, param2, family)
    bad <- !is.finite(prob)
    prob[bad] <- ifelse(
      is.finite(surv_t[bad]) & surv_t[bad] <= .Machine$double.eps,
      1,
      0
    )
  }
  clamp_probability(prob)
}

#' @title Compute a high-quantile longevity bound
#'
#' @description
#' Internal helper used by tradeoff simulations to compute an upper longevity
#' bound from mortality parameters. The bound corresponds to a very high
#' quantile (`0.99999999`) of the mortality distribution.
#'
#' @param expected Numeric vector of expected mortality times.
#' @param shape Numeric vector of second distribution parameters.
#' @param family Character scalar indicating the mortality family. Must be one
#'   of `"wei"`, `"lgn"`, `"gam"`, or `"exp"`.
#'
#' @return A numeric vector with high-quantile longevity values.
#'
#' @keywords internal
compute_max_longevity <- function(expected, shape, family) {
  if (family == "wei") {
    scale <- expected / gamma(1 + 1 / shape)
    qweibull(0.99999999, shape = shape, scale = scale)
  } else if (family == "lgn") {
    mu <- log(expected) - 0.5 * log(1 + shape / (expected^2))
    sigma <- sqrt(log(1 + shape / (expected^2)))
    qlnorm(0.99999999, meanlog = mu, sdlog = sigma)
  } else if (family == "gam") {
    qgamma(0.99999999, shape = expected / shape, scale = shape)
  } else if (family == "exp") {
    qexp(0.99999999, rate = 1 / expected)
  } else {
    stop("Unsupported family for mortality simulation: ", family)
  }
}

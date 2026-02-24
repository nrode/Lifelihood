#' @keywords internal
is_parameter_fitted <- function(object, parameter_name) {
  parameter_name %in% object$effects$parameter
}

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

#' @keywords internal
clamp_probability <- function(x) {
  ifelse(is.finite(x), pmin(pmax(x, 0), 1), 1)
}

#' @keywords internal
prob_event_interval_dt_safe <- function(t, dt, param1, param2, family) {
  prob <- prob_event_interval_dt(
    t = t,
    dt = dt,
    param1 = param1,
    param2 = param2,
    family = family
  )
  clamp_probability(prob)
}

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
    stop("Unsupported family for mortality simulation: ", family, call. = FALSE)
  }
}

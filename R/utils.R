#' @title Make the design matrix
#'
#' @keywords internal
#'
#' @description
#' Create the design matrix for the expected death and
#' the survival shape.
#'
#' @inheritParams lifelihood
#'
#' @export
make_design_matrix <- function(covariates, data) {
  if (!all(covariates %in% names(data))) {
    stop("Some covariate names are not present in the data.")
  }

  factor_levels <- lapply(covariates, function(cov) {
    if (is.factor(data[[cov]])) {
      levels(data[[cov]])
    } else {
      unique(data[[cov]])
    }
  })
  factor_levels

  names(factor_levels) <- covariates
  fitted_data <- do.call(expand.grid, factor_levels)
  formula_str <- paste("~", paste(covariates, collapse = " + "))
  formula <- as.formula(formula_str)

  mat_expt_death <- model.matrix(formula, fitted_data)
  mat_survival_param2 <- model.matrix(formula, fitted_data)

  return(list(
    fitted_data = fitted_data,
    mat_expt_death = mat_expt_death,
    mat_survival_param2 = mat_survival_param2
  ))
}

#' @title Weibull survival function
#'
#' @description
#' Weibull survival function
#'
#' @keywords internal
#'
#' @param t Numeric. The time to event
#' @param expt_time_to_event Numeric. The expected longevity or time to maturity.
#' @param shape Numeric. The shape parameter
#'
#' @return The survival probability (numeric)
#'
#' @export
SurvWei <- function(t, expt_time_to_event, shape) {
  scale <- expt_time_to_event / gamma(1 + 1 / shape)
  return(exp(-(t / scale)^shape))
}

#' @title Probability of dying between t and t+dt afer living until t.
#'
#' @keywords internal
#'
#' @param t Numeric. The time to event
#' @param dt Interval
#' @param param1 Numeric. The expected longevity or time to maturity.
#' @param param2 Numeric. The second parameter returned by lifelihood.
#' @param family One of "exp", "wei", "gam", "lgn"
#'
#' @return The survival probability (numeric)
#'
#' @export
prob_event_interval_dt <- function(t, dt, param1, param2, family) {
  (surv(t, param1, param2, family) -
    surv(t + dt, param1, param2, family)) /
    surv(t, param1, param2, family)
}

#' @title Probability of dying between t and t+dt afer living until t.
#'
#' @keywords internal
#'
#' @param t Numeric. The time to event
#' @param param1 Numeric. The expected longevity or time to maturity.
#' @param param2 Numeric. The second parameter returned by lifelihood.
#' @param family One of "exp", "wei", "gam", "lgn"
#'
#' @return The probability of event (being alive or not mature) at time t.
surv <- function(
  t,
  param1,
  param2,
  family = c("exp", "wei", "gam", "lgn")
) {
  family <- match.arg(family)
  if (family == "wei") {
    return(SurvWei(t = t, expt_time_to_event = param1, shape = param2))
  } else if (family == "gam") {
    return(SurvGam(t = t, expt_time_to_event = param1, scale = param2))
  } else if (family == "lgn") {
    return(SurvLgn(t = t, expt_time_to_event = param1, vp1 = param2))
  } else if (family == "exp") {
    return(SurvExp(t = t, expt_time_to_event = param1))
  }
}

#' @title Log-normal survival function
#'
#' @description
#' Log-normal survival function
#'
#' @keywords internal
#'
#' @param t Numeric. The time to event
#' @param expt_time_to_event Numeric. The expected longevity or time to maturity.
#' @param vp1 Numeric. The vp1 parameter
#'
#' @return The survival probability (numeric)
#'
#' @export
SurvLgn <- function(t, expt_time_to_event, vp1) {
  mu <- log(expt_time_to_event) - 0.5 * log(1 + vp1 / (expt_time_to_event^2))
  sigma <- sqrt(log(1 + vp1 / (expt_time_to_event^2)))
  plnorm(t, meanlog = mu, sdlog = sigma, lower.tail = FALSE, log.p = FALSE)
}

#' @title Gamma survival function
#'
#' @description
#' Gamma survival function
#'
#' @keywords internal
#'
#' @param t Numeric. The time to event
#' @param expt_time_to_event Numeric. The expected longevity or time to maturity.
#' @param scale Numeric. The scale parameter
#'
#' @return The survival probability (numeric)
#'
#' @export
SurvGam <- function(t, expt_time_to_event, scale) {
  shape <- expt_time_to_event / scale
  pgamma(t, shape = shape, scale = scale, lower.tail = FALSE, log.p = FALSE)
}

#' @title Exponential survival function
#'
#' @description
#' Exponential survival function
#'
#' @keywords internal
#'
#' @param t Numeric. The time to event
#' @param expt_time_to_event Numeric. The expected longevity or time to maturity.
#'
#' @return The survival probability (numeric)
#'
#' @export
SurvExp <- function(t, expt_time_to_event) {
  exp(-(t / expt_time_to_event))
}

#' @title Find the operating system of the user
#'
#' @description
#' `detect_os()` finds the operating system name
#'
#' @keywords internal
#'
#' @return String with the name of the operating system, either "Windows" or "Unix-like"
#'
#' @examples
#' detect_os()
#'
#' @export
detect_os <- function() {
  compatible_oses <- c("Windows", "Darwin", "Linux")
  os <- Sys.info()["sysname"]
  if (os %in% compatible_oses) {
    return(os)
  } else {
    stop(
      "lifelihood only works with MacOS, Windows, and Linux at the moment, not: ",
      os
    )
  }
}

#' @title Get the path to a built-in configuration file.
#'
#' @description
#' `lifelihood` embeds a few configuration files, and
#' this function is a simple tool to access one of them.
#'
#' It takes the name one of the available configuration
#' and returns the path to it.
#'
#' For more info about configuration files, see
#' \code{vignette("setting-up-the-configuration-file", package = "lifelihood")}
#'
#' @param config_name Configuration name. Currently available options:
#' - config
#' - config2
#' - config_pierrick
#' By default, it will use "config".
#'
#' @return Absolute path to the configuration file
#'
#' @examples
#' use_test_config("config")
#' use_test_config("config2")
#'
#' @export
use_test_config <- function(
  config_name = c(
    "config",
    "config2",
    "config_pierrick",
    "config_with_tradeoff",
    "config_without_tradeoff",
    "example_config_se",
    "example_config_mcmc",
    "config_pierrick_geno_death",
    "config_gbg"
  )
) {
  config_name <- match.arg(config_name)

  config_path <- system.file(
    "configs",
    paste0(config_name, ".yaml"),
    package = "lifelihood"
  )

  return(config_path)
}

#' @title Check that an object is of class `lifelihoodResults`
#'
#' @description
#' Internally, `lifelihood` has to check multiple times
#' that the passed object is the expected one.
#'
#' It basically raises an explicit error if the object
#' is not of class `lifelihoodResults`.
#'
#' @param object An object to test.
#'
#' @examples
#' \dontrun{
#' # raise an error
#' obj <- c(1,2,3)
#' check_lifelihoodResults(obj)
#'
#' # works (does nothing)
#' class(obj) = "lifelihoodResults"
#' check_lifelihoodResults(obj)
#' }
#'
#' @keywords internal
check_lifelihoodResults <- function(object) {
  if (!(inherits(object, "lifelihoodResults"))) {
    stop(paste0(
      "`object` expects a 'lifelihoodResults' object, not: '",
      class(object),
      "'"
    ))
  }
}

#' @title Check that an object is of class `lifelihoodData`
#'
#' @description
#' Internally, `lifelihood` has to check multiple times
#' that the passed object is the expected one.
#'
#' It basically raises an explicit error if the object
#' is not of class `lifelihoodData`.
#'
#' @param object An object to test.
#'
#' @examples
#' \dontrun{
#' # raise an error
#' obj <- c(1,2,3)
#' check_lifelihoodData(obj)
#'
#' # works (does nothing)
#' class(obj) = "lifelihoodData"
#' check_lifelihoodData(obj)
#' }
#'
#' @keywords internal
check_lifelihoodData <- function(object) {
  if (!(inherits(object, "lifelihoodData"))) {
    stop(paste0(
      "`object` expects a 'lifelihoodData' object, not: '",
      class(object),
      "'"
    ))
  }
}

#' @title Convert a vector to factor codes
#'
#' @description
#' Convert values to 0-based factor codes for the Pascal program.
#'
#' @param x A vector
#'
#' @returns Numeric vector of 0-based factor codes
#'
#' @keywords internal
factor_to_num <- function(x) {
  as.numeric(as.factor(x)) - 1
}

#' @title Count total number of parameter to fit
#'
#' @description
#' Simple utility function that parses the config yaml file
#' and count the total number of parameter that will be fit.
#'
#' @param x Config file parsed by [`yaml::yaml.load_file()`].
#'
#' @returns Total number of parameter to fit.
#'
#' @keywords internal
count_parameters <- function(x) {
  leaves <- unlist(x, recursive = TRUE, use.names = FALSE)

  leaves <- leaves[leaves != "not_fitted"]

  sum(vapply(
    leaves,
    function(v) {
      if (is.numeric(v)) {
        1
      } else {
        length(expand_formula_terms(v))
      }
    },
    integer(1)
  ))
}

#' @title Utility to generate vector of clutch names.
#'
#' @export
generate_clutch_vector <- function(N) {
  return(paste(
    "clutch",
    rep(c("start", "end", "size"), N),
    rep(1:N, each = 3),
    sep = "_"
  ))
}

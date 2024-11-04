#' @title Make the design matrix
#' @name make_design_matrix
#' @keywords internal
#' @description Create the design matrix for the expected death and the survival shape.
#' @inheritParams lifelihood
#' @inheritParams pred_mortality_rate
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
  mat_survival_shape <- model.matrix(formula, fitted_data)

  return(list(
    fitted_data = fitted_data,
    mat_expt_death = mat_expt_death,
    mat_survival_shape = mat_survival_shape
  ))
}

#' @title Link function
#' @name link
#' @description Link function to transform the parameters to the original scale
#' @keywords internal
#' @param estimate Numeric. The estimate of the parameter
#' @param min_and_max Numeric vector of length 2. The minimum and maximum values of the parameter
#' @return Numeric. The transformed parameter
#' @export
link <- function(estimate, min_and_max) {
  min <- min_and_max[1]
  max <- min_and_max[2]
  return(min + (max - min) / (1 + exp(-estimate)))
}

#' @title Delink function
#' @name delink
#' @description Delink function to transform the parameters to the original scale
#' @keywords internal
#' @param obs Numeric. The observed value of the parameter
#' @param min_and_max Numeric vector of length 2. The minimum and maximum values of the parameter
#' @return Numeric. The transformed parameter
#' @export
delink <- function(obs, min_and_max) {
  min <- min_and_max[1]
  max <- min_and_max[2]
  return(log((obs - min) / (max - obs)))
}

#' @title Weibull survival function
#' @name SurvWei
#' @description Weibull survival function
#' @keywords internal
#' @param t Numeric. The time to event
#' @param ExpLong Numeric. The expected longevity
#' @param Shape Numeric. The shape parameter
#' @return Numeric. The survival probability
#' @export
SurvWei <- function(t, ExpLong, Shape) {
  Scale <- ExpLong / gamma(1 + 1 / Shape)
  exp(-(t / Scale)^Shape)
}

#' @title Find the operating system of the user
#' @description `detect_os()` finds the operating system name
#' @keywords internal
#' @name detect_os
#' @return String with the name of the operating system
#' @export
detect_os <- function() {
  os <- Sys.info()["sysname"]
  if (os == "Windows") {
    return("Windows")
  } else if (os == "Linux" || os == "Darwin") {
    return("Unix-like")
  } else {
    return("Unknown")
  }
}

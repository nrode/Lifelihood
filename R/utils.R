#' @title Make the design matrix
#'
#' @keywords internal
#'
#' @description
#' Create the design matrix for the expected death and
#' the survival shape.
#'
#' @inheritParams lifelihood
#' @inheritParams pred_mortality_rate
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
  mat_survival_shape <- model.matrix(formula, fitted_data)

  return(list(
    fitted_data = fitted_data,
    mat_expt_death = mat_expt_death,
    mat_survival_shape = mat_survival_shape
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
#' @param ExpLong Numeric. The expected longevity
#' @param Shape Numeric. The shape parameter
#'
#' @return The survival probability (numeric)
#'
#' @export
SurvWei <- function(t, ExpLong, Shape) {
  Scale <- ExpLong / gamma(1 + 1 / Shape)
  return(exp(-(t / Scale)^Shape))
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
  os <- Sys.info()["sysname"]
  if (os == "Windows") {
    return("Windows")
  } else if (os == "Linux" || os == "Darwin") {
    return("Unix-like")
  } else {
    stop(paste0("Unexpected OS: ", os))
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
#' get_config_path("config")
#' get_config_path("config2")
#'
#' @export
get_config_path <- function(
  config_name = c("config", "config2", "config_pierrick")
) {
  config_name <- match.arg(config_name)

  config_path <- system.file(
    "configs",
    paste0(config_name, ".yaml"),
    package = "lifelihood"
  )

  return(config_path)
}
